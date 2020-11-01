open Coral_core
open Base

type compileContext = {
  context : Llvm.llcontext;
  llmodule : Llvm.llmodule;
  main_func : Llvm.llvalue option;
  main_builder : Llvm.llbuilder;
  ns : Names.t;
  llvals : (Ast.node, Llvm.llvalue, Ast.Node.comparator_witness) Map.t;
}

type func_compile_ctx = {
  parent : compileContext;
  current_func : Llvm.llvalue;
  builder : Llvm.llbuilder;
  terminated : bool;
}

module Lltype = struct
  type t = Llvm.lltype

  let rec of_type (data : compileContext) typ =
    let open Coral_core.Type in
    match typ with
    | Parameterized (Parameterized (Name "Func", ret), params) -> (
        let ll_ret_type =
          match ret with
          | [] -> Llvm.void_type data.context
          | [ ret ] -> of_type data ret
          | other ->
              Llvm.struct_type data.context @@ Array.of_list @@ List.map ~f:(of_type data) other
        in
        let n = List.length params in
        match List.nth params (n - 1) with
        | Some (Name "...") ->
            List.sub ~pos:0 ~len:(n - 1) params
            |> List.map ~f:(of_type data)
            |> Array.of_list
            |> Llvm.var_arg_function_type ll_ret_type
        | _ -> Llvm.function_type ll_ret_type (Array.of_list @@ List.map ~f:(of_type data) params)
        )
    | Name "Int8" -> Llvm.i8_type data.context
    | Name "Int16" -> Llvm.i16_type data.context
    | Name "Int32" -> Llvm.i32_type data.context
    | Name "Int64" -> Llvm.i64_type data.context
    | Name "Void" -> Llvm.void_type data.context
    | Name "Str" -> Llvm.pointer_type @@ Llvm.i8_type data.context
    | typ -> failwith (Coral_core.Type.show Ast.pp_node typ)
end

let memoize e (data, llval) =
  let data = { data with llvals = Map.set ~key:e ~data:llval data.llvals } in
  (data, llval)

(* This builds any "simple" expressions that can exist at either the module level (in a value
   initializer without requiring generating any code in the .init) or function level *)
let codegen_at_any_level (data : compileContext) expr =
  match expr with
  | Ast.StringLiteral s ->
      (* let str = Llvm.const_stringz data.context s in *)
      let globstr = Llvm.build_global_stringptr s "" data.main_builder in
      (data, globstr)
  | Ast.IntLiteral i -> (data, Llvm.const_int_of_string (Llvm.i64_type data.context) i 10)
  | expr -> failwith @@ Ast.show_node_data expr

(* This should be the main implementatiaon of codegen. we use a llvm.builder and construct llvm
   instructions for every node type *)
let rec _codegen_func (fctx : func_compile_ctx) node =
  let expr, _ = node in
  (* Stdio.printf "   [codegen-func: %s]\n" (Ast.nodeName @@ fst node);
   * Stdlib.flush_all (); *)
  match expr with
  | Ast.Block xs ->
      let fctx, _ll_items = List.fold_map ~init:fctx ~f:func_codegen xs in
      (fctx, Llvm.const_int (Llvm.i1_type fctx.parent.context) 0)
  | Ast.Binop { callee; args } | Ast.Call { callee; args } -> (
      (* hmmm. Either we can turn the returntype into a custom type LlValue | BuiltinOp | Lltype or
         we could have some other pass that rewrites anything resolving to a builtin to something
         pattern-matchable. For now, I guess the LLVM pass can be smart, but at some point we may
         have a dumber backend that requires prior analysis *)
      match Names.deref fctx.parent.ns callee with
      | Some (Ast.Builtin Builtins.EQ, _) -> (
          match List.fold_map ~init:fctx ~f:func_codegen args with
          | fctx, [ lhs; rhs ] -> (fctx, Llvm.build_icmp Llvm.Icmp.Eq lhs rhs "" fctx.builder)
          | _ -> failwith "bad eq arguments" )
      | Some (Ast.Builtin Builtins.LT, _) -> (
          match List.fold_map ~init:fctx ~f:func_codegen args with
          | fctx, [ lhs; rhs ] -> (fctx, Llvm.build_icmp Llvm.Icmp.Slt lhs rhs "" fctx.builder)
          | _ -> failwith "bad eq arguments" )
      | Some (Ast.Builtin Builtins.ADD, _) -> (
          match List.fold_map ~init:fctx ~f:func_codegen args with
          | fctx, [ lhs; rhs ] -> (fctx, Llvm.build_add lhs rhs "" fctx.builder)
          | _ -> failwith "bad add arguments" )
      | Some (Ast.Builtin Builtins.SUB, _) -> (
          match List.fold_map ~init:fctx ~f:func_codegen args with
          | fctx, [ lhs; rhs ] -> (fctx, Llvm.build_sub lhs rhs "" fctx.builder)
          | _ -> failwith "bad add arguments" )
      | Some (Ast.Builtin Builtins.MUL, _) -> (
          match List.fold_map ~init:fctx ~f:func_codegen args with
          | fctx, [ lhs; rhs ] -> (fctx, Llvm.build_mul lhs rhs "" fctx.builder)
          | _ -> failwith "bad add arguments" )
      | Some (Ast.Builtin Builtins.DIV, _) -> (
          match List.fold_map ~init:fctx ~f:func_codegen args with
          | fctx, [ lhs; rhs ] -> (fctx, Llvm.build_sdiv lhs rhs "" fctx.builder)
          | _ -> failwith "bad add arguments" )
      | Some (Ast.Builtin Builtins.MOD, _) -> (
          match List.fold_map ~init:fctx ~f:func_codegen args with
          | fctx, [ lhs; rhs ] -> (fctx, Llvm.build_srem lhs rhs "" fctx.builder)
          | _ -> failwith "bad add arguments" )
      | _ ->
          let fctx, ll_callee = func_codegen fctx callee in
          let fctx, ll_args = List.fold_map ~init:fctx ~f:func_codegen args in
          (fctx, Llvm.build_call ll_callee (Array.of_list ll_args) "" fctx.builder) )
  | Ast.Var _ -> (
      let reference = Names.deref fctx.parent.ns node in
      match reference with
      | None -> failwith @@ "invalid reference: " ^ Ast.show_node node
      | Some e -> func_codegen fctx e )
  | Ast.Func _ ->
      let new_parent, lval = _mod_codegen fctx.parent node in
      ({ fctx with parent = new_parent }, lval)
  | Ast.Return value -> (
      let fctx, llval = func_codegen fctx value in
      let fctx = { fctx with terminated = true } in
      match Llvm.classify_type @@ Llvm.type_of llval with
      | Llvm.TypeKind.Void -> (fctx, Llvm.build_ret_void fctx.builder)
      | _ -> (fctx, Llvm.build_ret llval fctx.builder) )
  | Ast.If (cond, b1, b2) ->
      (* TODO: this termination analysis needs to happen in a previous pass, Because if we know
         both sides are terminated, then we don't have to generate block3 *)
      let fctx, llcond = func_codegen fctx cond in
      let llcx = fctx.parent.context in
      let llblock1 = Llvm.append_block llcx "b1" fctx.current_func in
      let llblock2 = Llvm.append_block llcx "b2" fctx.current_func in
      let llblock3 = Llvm.append_block llcx "b3" fctx.current_func in
      let br = Llvm.build_cond_br llcond llblock1 llblock2 fctx.builder in
      Llvm.position_at_end llblock1 fctx.builder;
      let fctx, _ = func_codegen { fctx with terminated = false } b1 in
      let is_terminated_1 = fctx.terminated in
      if not is_terminated_1 then ignore (Llvm.build_br llblock3 fctx.builder);
      Llvm.position_at_end llblock2 fctx.builder;
      let fctx, _ = func_codegen { fctx with terminated = false } b2 in
      let is_terminated_2 = fctx.terminated in
      if not is_terminated_2 then ignore (Llvm.build_br llblock3 fctx.builder);
      if is_terminated_1 && is_terminated_2 then Llvm.remove_block llblock3
      else Llvm.position_at_end llblock3 fctx.builder;
      (fctx, br)
  | Ast.Tuple [] -> (fctx, Llvm.undef @@ Llvm.void_type fctx.parent.context)
  | Ast.Tuple items ->
      let fctx, llitems = List.fold_map ~init:fctx ~f:func_codegen items in
      (fctx, Llvm.const_struct fctx.parent.context (Array.of_list llitems))
  | StringLiteral _ | IntLiteral _ -> (fctx, snd @@ codegen_at_any_level fctx.parent expr)
  | expr ->
      Stdio.printf "Func-Codegen: %s\n" @@ Ast.nodeName expr;
      Stdio.print_endline @@ Ast.show_node_data expr;
      (fctx, Llvm.const_int (Llvm.i1_type fctx.parent.context) 0)

and memoizef f data expr =
  match Base.Map.find data.llvals expr with
  | Some x -> (data, x)
  | None ->
      let data, result = f data expr in
      memoize expr (data, result)

and func_codegen (data : func_compile_ctx) expr =
  match Base.Map.find data.parent.llvals expr with
  | Some x -> (data, x)
  | None ->
      let data, result = _codegen_func data expr in
      let p = { data.parent with llvals = Map.set ~key:expr ~data:result data.parent.llvals } in
      let data = { data with parent = p } in
      (data, result)

and param_codegen ll_func data node =
  match fst node with
  | Ast.Param { idx; _ } -> memoize node (data, Llvm.param ll_func idx)
  | _ -> failwith "expected parameter"

and mod_codegen data expr = memoizef _mod_codegen data expr

and _mod_codegen (data : compileContext) node =
  let open Ast in
  let expr, _ = node in
  match expr with
  (* Extern declarations *)
  | Extern { binding = "c"; name; typ } ->
      let func = Llvm.declare_function name (Lltype.of_type data typ) data.llmodule in
      (data, func)
  | Let (_, bar) ->
      let data, ll_bar = mod_codegen data bar in
      (data, ll_bar)
  | Func { name; ret_type; params; body } ->
      Stdio.printf "func = %s\n" name;
      Stdlib.flush_all ();
      (* TODO: need type analysis to get types of params *)
      let ptype data param =
        match fst param with
        | Param { typ = Some t; _ } -> (data, Lltype.of_type data t)
        | _ -> failwith "ptype"
      in
      let data, ll_ptype = List.fold_map ~init:data ~f:ptype params in
      let ll_ptype = Array.of_list ll_ptype in

      let ret_type = Option.value ~default:(Coral_core.Type.Name "Void") ret_type in
      let ll_ret_type = Lltype.of_type data ret_type in
      let ll_func_type = Llvm.function_type ll_ret_type ll_ptype in
      let ll_func = Llvm.define_function name ll_func_type data.llmodule in
      (* Recursion : The ll_func value must be visible inside the body
       * so it must be added to data.llvals. *)
      let data = { data with llvals = Map.set ~key:node ~data:ll_func data.llvals } in
      let data, _ = List.fold_map ~init:data ~f:(param_codegen ll_func) params in
      if String.equal name ".init" then
        Llvm.position_at_end (Llvm.entry_block ll_func) data.main_builder;
      let builder = Llvm.builder data.context in
      Llvm.position_at_end (Llvm.entry_block ll_func) builder;
      let fctx = { parent = data; current_func = ll_func; builder; terminated = false } in
      let fctx, _ = func_codegen fctx body in
      (fctx.parent, ll_func)
  | expr ->
      Stdio.printf "module-codegen: %s\n" @@ Ast.nodeName expr;
      Ast.iter (fun e -> mod_codegen data e |> ignore) node;
      Stdlib.flush_all ();
      (data, Llvm.const_int (Llvm.i1_type data.context) 0)

let print_ir (ns : Names.t) expr =
  let name, lines =
    match expr with
    | Ast.Module { name; lines }, _ -> (name, lines)
    | _ -> failwith "print_ir: expected module"
  in
  let context = Llvm.create_context () in
  let llmodule = Llvm.create_module context name in
  let main_builder = Llvm.builder context in
  let data =
    { context; llmodule; main_builder; main_func = None; ns; llvals = Map.empty (module Ast.Node) }
  in

  Stdio.printf "\n\n";
  List.fold_map ~init:data ~f:mod_codegen lines |> ignore;
  Llvm.dump_module llmodule;

  (* TODO: we're generating untyped operations with implicit casts *)
  ( match Llvm_analysis.verify_module llmodule with
  | None -> ()
  | Some e ->
      Stdio.printf "%s\n" e;
      Stdlib.flush_all () );

  let passmgrbuilder = Llvm_passmgr_builder.create () in
  let module_passmgr = Llvm.PassManager.create () in
  ignore (Llvm_passmgr_builder.set_opt_level 3 passmgrbuilder);
  Llvm_passmgr_builder.populate_module_pass_manager module_passmgr passmgrbuilder;
  ignore (Llvm.PassManager.run_module llmodule module_passmgr);

  try
    let open Ctypes in
    ignore (Llvm_executionengine.initialize ());
    let llengine = Llvm_executionengine.create llmodule in
    let ctype = Foreign.funptr (Ctypes.void @-> Ctypes.returning Ctypes.int32_t) in
    let init_func = Llvm_executionengine.get_function_address ".init" ctype llengine in
    ignore (init_func ())
  with exc -> Exn.to_string exc |> Stdio.print_string
