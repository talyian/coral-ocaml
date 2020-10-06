open Llvm
open Coral_core
open Base

type compileContext = {
  context : Llvm.llcontext;
  llmodule : Llvm.llmodule;
  main_func : Llvm.llvalue option;
  current_func : Llvm.llvalue option;
  builder : Llvm.llbuilder;
  ns : Name_resolution.Names.t;
  llvals : (Ast.node, Llvm.llvalue, Ast.Node.comparator_witness) Map.t;
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
              Llvm.struct_type data.context
              @@ Array.of_list
              @@ List.map ~f:(of_type data) other
        in
        let n = List.length params in
        match List.nth params (n - 1) with
        | Some (Name "...") ->
            List.sub ~pos:0 ~len:(n - 1) params
            |> List.map ~f:(of_type data)
            |> Array.of_list
            |> Llvm.var_arg_function_type ll_ret_type
        | _ ->
            Llvm.function_type ll_ret_type
              (Array.of_list @@ List.map ~f:(of_type data) params) )
    | Name "Void" -> Llvm.void_type data.context
    | Name "Str" -> Llvm.pointer_type @@ Llvm.i8_type data.context
    | typ -> failwith (Coral_core.Type.show Ast.pp_node typ)
end

let memoize e (data, llval) =
  let data =
    { data with llvals = Map.add_exn ~key:e ~data:llval data.llvals }
  in
  (data, llval)

(* This builds any "simple" expressions that can exist at either the module
   level (in a value initializer without requiring generating any code in the
   .init) or function level *)
let rec codegen_at_any_level (data : compileContext) expr =
  match expr with
  | Ast.StringLiteral s ->
      (* let str = Llvm.const_stringz data.context s in *)
      let globstr = Llvm.build_global_stringptr s "" data.builder in
      (data, globstr)
  | Ast.IntLiteral i ->
      (data, Llvm.const_int_of_string (Llvm.i64_type data.context) i 10)
  | expr -> failwith @@ Ast.show_node expr

let rec codegen_at_func_level (data : compileContext) expr =
  match expr with
  | Ast.Block xs ->
      let data, ll_items =
        List.fold_map ~init:data ~f:codegen_at_func_level xs
      in
      (data, Llvm.const_int (Llvm.i1_type data.context) 0)
  | Ast.Call { callee; args } ->
      let data, ll_args =
        List.fold_map ~init:data ~f:codegen_at_func_level args
      in
      let data, ll_callee = codegen_at_func_level data callee in
      (data, Llvm.build_call ll_callee (Array.of_list ll_args) "" data.builder)
  | Ast.Var { name; varType } ->
      let llvalue =
        Map.find data.ns.refs expr |> Option.bind ~f:(Map.find data.llvals)
      in
      (data, Option.value_exn llvalue)
  | Ast.Return (Ast.Tuple []) -> (data, Llvm.build_ret_void data.builder)
  | StringLiteral _ | IntLiteral _ -> codegen_at_any_level data expr
  | expr ->
      Stdio.printf "Func-Codegen: %s\n" @@ Ast.nodeName expr;
      (data, Llvm.const_int (Llvm.i1_type data.context) 0)

let rec codegen_at_module_level (data : compileContext) expr =
  let open Ast in
  match expr with
  (* Extern declarations *)
  | Call
      {
        callee = Var { name = "extern"; _ };
        args = [ StringLiteral "c"; Var { name; varType = Some typ } ];
      } ->
      let func =
        Llvm.declare_function name (Lltype.of_type data typ) data.llmodule
      in
      memoize expr (data, func)
  | Let (foo, bar) ->
      let data, ll_bar = codegen_at_module_level data bar in
      memoize expr (data, ll_bar)
  | Func { name; ret_type; params; body } ->
      (* let data, ll_params = List.fold_map ~init:data ~f:codegen_at_module_level params in *)
      let ret_type =
        match ret_type with Some s -> s | None -> Coral_core.Type.Name "Void"
      in
      let ll_params = [||] in
      let ll_ret_type = Lltype.of_type data ret_type in
      let ll_func_type = Llvm.function_type ll_ret_type ll_params in
      let ll_func = Llvm.define_function name ll_func_type data.llmodule in
      Llvm.position_at_end (Llvm.entry_block ll_func) data.builder;
      let data, _ = codegen_at_func_level data body in
      memoize expr (data, ll_func)
  | expr ->
      Stdio.printf "Codegen: %s\n" @@ Ast.nodeName expr;
      Ast.recurse_unit (fun e -> codegen_at_module_level data e |> ignore) expr;
      Stdlib.flush_all ();
      (data, Llvm.const_int (Llvm.i1_type data.context) 0)

let print_ir (ns : Name_resolution.Names.t) (Ast.Module { name; lines }) =
  let context = Llvm.create_context () in
  let llmodule = Llvm.create_module context name in
  (* let llmain_func_type = Llvm.function_type (Llvm.void_type context) [||] in
   * let func = Llvm.declare_function ".init" llmain_func_type llmodule in *)
  let builder = Llvm.builder context in
  (* let entry = Llvm.append_block context "entry" func in
   * Llvm.position_at_end entry builder; *)
  let data =
    {
      context;
      llmodule;
      builder;
      main_func = None;
      current_func = None;
      ns;
      llvals = Map.empty (module Ast.Node);
    }
  in

  Stdio.printf "\n\n";
  List.fold_map ~init:data ~f:codegen_at_module_level lines |> ignore;
  Llvm.dump_module llmodule;

  (* TODO: we're generating untyped operations with implicit casts *)
  ( match Llvm_analysis.verify_module llmodule with
  | None -> ()
  | Some e ->
      Stdio.printf "%s\n" @@ Llvm.string_of_llmodule llmodule;
      Stdio.printf "%s\n" e;
      Stdlib.flush_all () );

  let passmgrbuilder = Llvm_passmgr_builder.create () in
  let module_passmgr = Llvm.PassManager.create () in
  ignore (Llvm_passmgr_builder.set_opt_level 3 passmgrbuilder);
  Llvm_passmgr_builder.populate_module_pass_manager module_passmgr
    passmgrbuilder;
  ignore (Llvm.PassManager.run_module llmodule module_passmgr);

  try
    let open Ctypes in
    ignore (Llvm_executionengine.initialize ());
    let llengine = Llvm_executionengine.create llmodule in
    let ctype =
      Foreign.funptr (Ctypes.void @-> Ctypes.returning Ctypes.int32_t)
    in
    let init_func =
      Llvm_executionengine.get_function_address ".init" ctype llengine
    in
    ignore (init_func ())
  with exc -> Printexc.to_string exc |> Stdio.print_string
