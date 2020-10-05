open Llvm
open Coral_core
open Ctypes
open Base

type compileContext = {
  context : Llvm.llcontext;
  llmodule : Llvm.llmodule;
  main_func : Llvm.llvalue;
  current_func : Llvm.llvalue;
  builder : Llvm.llbuilder;
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

(* This builds any "simple" expressions that can exist at either the module
   level (in a value initializer without requiring generating any code in the
   .init) or function level *)
let rec codegen_at_any_level (data: compileContext) expr =
  match expr with
  | Ast.StringLiteral s ->
    data, Llvm.const_stringz data.context s
  | Ast.IntLiteral i ->
    data, Llvm.const_int_of_string (Llvm.i64_type data.context) i 10
  | expr -> failwith @@ Ast.show_node expr

let rec codegen_at_func_level (data : compileContext) expr =
  match expr with
  | Ast.Block xs ->
    let data, ll_items = List.fold_map ~init:data ~f:codegen_at_func_level xs in
    data, Llvm.const_int (Llvm.i1_type data.context)  0
  | Ast.Call {callee; args} ->
    let data, ll_args = List.fold_map ~init:data ~f:codegen_at_func_level args in
    let data, ll_callee = codegen_at_func_level data callee in
    data, ll_callee
  | Ast.Var {name;varType} ->
    data, Llvm.const_int (Llvm.i1_type data.context) 0
  | StringLiteral _
  | IntLiteral _ -> codegen_at_any_level data expr
  | expr ->
    Stdio.printf "Func-Codegen: %s\n" @@ Ast.nodeName expr;
    data, Llvm.const_int (Llvm.i1_type data.context) 0

let rec codegen_at_module_level (data : compileContext) expr =
  let open Ast in
  match expr with
  (* Extern declarations *)
  | Call
      {
        callee = Var { name = "extern"; _ };
        args = [ StringLiteral "c"; Var { name; varType = Some typ } ];
      } ->
    let func = Llvm.declare_function name (Lltype.of_type data typ) data.llmodule in
    data, func
  | Let (foo, bar) ->
    let data, ll_bar = codegen_at_module_level data bar in
    data, ll_bar
  | Func {name;ret_type;params;body} ->
    (* let data, ll_params = List.fold_map ~init:data ~f:codegen_at_module_level params in *)
    let ret_type = match ret_type with Some s -> s | None -> Coral_core.Type.Name "Void" in
    let ll_params = [| |] in
    let ll_ret_type = Lltype.of_type data ret_type in
    let ll_func_type = Llvm.function_type ll_ret_type ll_params in
    let ll_func = Llvm.define_function name ll_func_type data.llmodule in
    Llvm.position_at_end (Llvm.entry_block ll_func) data.builder;
    let data, _ = codegen_at_func_level data body in
    data, ll_func
  | expr ->
    Stdio.printf "Codegen: %s\n" @@ Ast.nodeName expr;
    Ast.recurse_unit (fun e -> codegen_at_module_level data e |> ignore) expr;
    Stdlib.flush_all ();
    data, Llvm.const_int (Llvm.i1_type data.context) 0

let print_ir (Ast.Module { name; lines }) =
  let context = Llvm.create_context () in
  let llmodule = Llvm.create_module context name in
  let llmain_func_type = Llvm.function_type (Llvm.void_type context) [||] in
  let func = Llvm.declare_function ".init" llmain_func_type llmodule in
  let builder = Llvm.builder context in
  let entry = Llvm.append_block context "entry" func in
  Llvm.position_at_end entry builder;
  let data =
    { context; llmodule; main_func = func; current_func = func; builder }
  in

  Stdio.printf "\n\n";
  List.fold_map ~init:data ~f:codegen_at_module_level lines |> ignore;
  Llvm.dump_module llmodule

(* module AstMap = Map.Make (struct
 *   type t = Ast.node
 *   let compare = compare
 * end)
 *
 * let default v = function Some x -> x | None -> v
 *
 * type codegenContext = {
 *   context : Llvm.llcontext;
 *   llmodule : Llvm.llmodule;
 *   func : Llvm.llvalue;
 *   builder : Llvm.llbuilder;
 *   mutable block : Llvm.llbasicblock;
 *   mutable isTerminated : bool;
 *   mutable llvalues : Llvm.llvalue AstMap.t;
 * }
 *
 * let rec llvmType llmodule context = function
 *   | Free -> failwith "cannot generate free type in llvm"
 *   | Type s -> (
 *       match s with
 *       | "Void" -> Llvm.void_type context
 *       | "Int8" -> Llvm.i8_type context
 *       | "Int32" -> Llvm.i32_type context
 *       | "Int64" -> Llvm.i64_type context
 *       | "Float32" -> Llvm.float_type context
 *       | "Float64" -> Llvm.double_type context
 *       | "Ptr" -> Llvm.pointer_type (Llvm.i8_type context)
 *       | "String" -> Llvm.pointer_type (Llvm.i8_type context)
 *       | "Tuple" -> Llvm.void_type context
 *       | name -> (
 *           match Llvm.type_by_name llmodule name with
 *           | Some v ->
 *               (\* Llvm.string_of_lltype v |> Printf.printf "lltype: %s\n";
 *                * flush stdout; *\)
 *               v
 *           | None -> failwith ("llvmBackend: unknown type " ^ s) ) )
 *   | Parameterized (name, params) -> (
 *       match name with
 *       | "Ptr" -> Llvm.pointer_type (llvmType llmodule context (List.hd params))
 *       | "Func" -> (
 *           match List.rev params with
 *           | ret :: rparams ->
 *               Llvm.function_type
 *                 (llvmType llmodule context ret)
 *                 ( rparams |> List.rev
 *                 |> List.map (llvmType llmodule context)
 *                 |> Array.of_list )
 *           | [] -> failwith "function: no return type!" )
 *       | _ -> failwith name )
 *   | _ -> failwith "Unknown type"
 *
 * let rec run1 llvalues = function
 *   | Module { name = module_name; lines } ->
 *       let lvContext = Llvm.global_context () in
 *       let lvModule = Llvm.create_module lvContext module_name in
 *       let rec looper llvalues = function
 *         | [] -> lvModule
 *         | Func func :: xs ->
 *             (\* Printf.printf "func: %s\n" func.name; *\)
 *             let llvalues =
 *               generate_function lvContext lvModule llvalues (Func func)
 *             in
 *             looper llvalues xs
 *         | Multifunc { name = _name; func = { contents }; _ } :: xs ->
 *             (\* Printf.printf "mf: %s\n" name; *\)
 *             let llvalues =
 *               generate_function lvContext lvModule llvalues contents
 *             in
 *             looper llvalues xs
 *             (\* let llvalues = List.fold_left
 *              *   (fun lv f -> generate_function lvContext lvModule lv !f)
 *              *   llvalues
 *              *   func_list in
 *              * looper llvalues xs *\)
 *         | (TupleDef info as _tuple) :: rest ->
 *             let llfields =
 *               info.fields
 *               |> List.map (fun (_name, ctype) ->
 *                      llvmType lvModule lvContext ctype)
 *               |> Array.of_list
 *             in
 *             let tuple_type = Llvm.named_struct_type lvContext info.name in
 *             Llvm.struct_set_body tuple_type llfields true;
 *             (\* Llvm.string_of_lltype tuple_type |> Printf.printf "tuple type: %s\n";
 *              * flush stdout; *\)
 *             looper llvalues rest
 *         | x :: _rest ->
 *             let err =
 *               "llvmBackend: unrecognized module item " ^ Ast.nodeName x
 *             in
 *             failwith err
 *       in
 *       looper llvalues lines
 *   | _ -> failwith "oops 1"
 *
 * and generate_function lvContext lvModule (llvalues : Llvm.llvalue AstMap.t) =
 *   function
 *   | Func func -> (
 *       (\* Printf.printf "defining function: %s \n" func.name; flush stdout; *\)
 *       let llret = llvmType lvModule lvContext func.ret_type in
 *       let is_vararg = function
 *         | Def { defType = Some (Type "..."); _ } -> true
 *         | _ -> false
 *       in
 *       let vararg = List.exists is_vararg func.params in
 *       let llparams =
 *         func.params
 *         |> List.filter (fun x -> not @@ is_vararg x)
 *         |> List.map (function
 *              | Def v ->
 *                  (\* HACK: why are we defaulting to int32 here? *\)
 *                  llvmType lvModule lvContext (default (Type "Int32") v.defType)
 *              | _ -> failwith "genfunc params: oops")
 *         |> Array.of_list
 *       in
 *       let func_type =
 *         if vararg then Llvm.var_arg_function_type llret llparams
 *         else Llvm.function_type llret llparams
 *       in
 *       match func.body with
 *       | Empty ->
 *           let llfunc = Llvm.declare_function func.name func_type lvModule in
 *           let llvalues_with_func = AstMap.add (Func func) llfunc llvalues in
 *           llvalues_with_func
 *       | _ ->
 *           let llfunc = Llvm.define_function func.name func_type lvModule in
 *           let llvalues_with_func = AstMap.add (Func func) llfunc llvalues in
 *           let llbuilder = Llvm.builder lvContext in
 *           let llblock = Llvm.entry_block llfunc in
 *           (\* let llblock = Llvm.append_block lvContext "entry" llfunc in *\)
 *           Llvm.position_at_end llblock llbuilder;
 *           let (_ : llvalue) =
 *             run_func
 *               {
 *                 context = lvContext;
 *                 llmodule = lvModule;
 *                 func = llfunc;
 *                 builder = llbuilder;
 *                 block = llblock;
 *                 isTerminated = false;
 *                 llvalues = llvalues_with_func;
 *               }
 *               (Func func)
 *           in
 *           llvalues_with_func )
 *   | n ->
 *       failwith (Format.sprintf "Expected function, %s found" (Ast.nodeName n))
 *
 * and run_func context = function
 *   | Func { name; ret_type = _ret_type; params; body } ->
 *       let loop i e =
 *         match e with
 *         | Def p -> (
 *             match p.defType with
 *             | None ->
 *                 failwith (Format.sprintf "%s: def [%s] has no type" name p.name)
 *             | Some def_type ->
 *                 let lltype =
 *                   llvmType context.llmodule context.context def_type
 *                 in
 *                 let lldef = Llvm.param context.func i in
 *                 let alloca = Llvm.build_alloca lltype p.name context.builder in
 *                 let _store = Llvm.build_store lldef alloca context.builder in
 *                 context.llvalues <- AstMap.add e alloca context.llvalues )
 *         | _ -> failwith "expected Def"
 *       in
 *       List.iteri loop params;
 *       run_func context body
 *   | Block body ->
 *       let rec looper = function
 *         | [] -> ()
 *         | line :: xs ->
 *             ignore (run_func context line);
 *             looper xs
 *       in
 *       looper body;
 *       Llvm.const_int (Llvm.i32_type context.context) 0
 *   | If (cond, ifbody, elsebody) ->
 *       let llIfbody = Llvm.append_block context.context "if" context.func in
 *       let llElsebody = Llvm.append_block context.context "else" context.func in
 *       let bbafter = Llvm.append_block context.context "after" context.func in
 *       let llcond = run_func context cond in
 *       let _llbr =
 *         Llvm.build_cond_br llcond llIfbody llElsebody context.builder
 *       in
 *       Llvm.position_at_end llIfbody context.builder;
 *       context.isTerminated <- false;
 *       context.block <- llIfbody;
 *       ignore (run_func context ifbody);
 *       if not context.isTerminated then
 *         ignore (Llvm.build_br bbafter context.builder);
 *       Llvm.position_at_end llElsebody context.builder;
 *       context.isTerminated <- false;
 *       context.block <- llElsebody;
 *       ignore (run_func context elsebody);
 *       if not context.isTerminated then
 *         ignore (Llvm.build_br bbafter context.builder);
 *       if context.isTerminated then Llvm.remove_block bbafter
 *       else Llvm.position_at_end bbafter context.builder;
 *       Llvm.const_int (Llvm.i32_type context.context) 0
 *   | IntLiteral n ->
 *       Llvm.const_int (Llvm.i64_type context.context) (int_of_string n)
 *   | FloatLiteral n ->
 *       Llvm.const_float (Llvm.double_type context.context) (float_of_string n)
 *   | Binop { name = op; args = [ lhs; rhs ]; _ } -> (
 *       let get_value = function node -> run_func context node in
 *       let lval = get_value lhs in
 *       let rval = get_value rhs in
 *       if Llvm.type_of lval = Llvm.pointer_type (Llvm.i8_type context.context)
 *       then
 *         match op with
 *         | "+" -> Llvm.build_gep lval [| rval |] "" context.builder
 *         | "-" ->
 *             let rval = Llvm.build_neg rval "" context.builder in
 *             Llvm.build_gep lval [| rval |] "" context.builder
 *         | _ -> failwith "bad operator on pointer"
 *       else if Llvm.type_of lval = Llvm.double_type context.context then
 *         match op with
 *         | "<" -> Llvm.build_fcmp Fcmp.Ult lval rval "" context.builder
 *         | "<=" -> Llvm.build_fcmp Fcmp.Ule lval rval "" context.builder
 *         | ">" -> Llvm.build_fcmp Fcmp.Ugt lval rval "" context.builder
 *         | ">=" -> Llvm.build_fcmp Fcmp.Uge lval rval "" context.builder
 *         | "=" -> Llvm.build_fcmp Fcmp.Ueq lval rval "" context.builder
 *         | "!=" -> Llvm.build_fcmp Fcmp.Une lval rval "" context.builder
 *         | "+" -> Llvm.build_fadd lval rval "" context.builder
 *         | "-" -> Llvm.build_fsub lval rval "" context.builder
 *         | "*" -> Llvm.build_fmul lval rval "" context.builder
 *         | "/" -> Llvm.build_fdiv lval rval "" context.builder
 *         | "%" -> Llvm.build_frem lval rval "" context.builder
 *         | _ -> failwith ("unknown operator " ^ op)
 *       else
 *         match op with
 *         | "<" -> Llvm.build_icmp Icmp.Slt lval rval "" context.builder
 *         | "<=" -> Llvm.build_icmp Icmp.Sle lval rval "" context.builder
 *         | ">" -> Llvm.build_icmp Icmp.Sgt lval rval "" context.builder
 *         | ">=" -> Llvm.build_icmp Icmp.Sge lval rval "" context.builder
 *         | "!=" -> Llvm.build_icmp Icmp.Ne lval rval "" context.builder
 *         | "=" -> Llvm.build_icmp Icmp.Eq lval rval "" context.builder
 *         | "+" -> Llvm.build_add lval rval "" context.builder
 *         | "-" -> Llvm.build_sub lval rval "" context.builder
 *         | "*" -> Llvm.build_mul lval rval "" context.builder
 *         | "/" -> Llvm.build_sdiv lval rval "" context.builder
 *         | "%" -> Llvm.build_srem lval rval "" context.builder
 *         | _ -> failwith ("unknown operator " ^ op) )
 *   | Def _d_info -> Llvm.const_int (Llvm.i32_type context.context) 0
 *   | Return v -> (
 *       context.isTerminated <- true;
 *       match v with
 *       | { node = value; coraltype = None }
 *       | { node = value; coraltype = Some (Ast.Type "Void") } ->
 *           ignore @@ run_func context value;
 *           Llvm.build_ret_void context.builder
 *       | { node = value; _ } ->
 *           Llvm.build_ret (run_func context value) context.builder )
 *   (\* Implement ADDROF: this is kind of sketchy *\)
 *   | Call
 *       {
 *         callee = Var { name = "addrof"; target = None; _ };
 *         args = [ Var arg ];
 *         _;
 *       } -> (
 *       (\* Printf.printf "var: %s of %s\n" callee.name arg.name; *\)
 *       match arg.target with
 *       | None -> failwith "(null)\n"
 *       | Some decl -> (
 *           match AstMap.find_opt decl context.llvalues with
 *           | None -> failwith "(invalid reference)\n"
 *           | Some target -> target ) )
 *   (\* Implement deref: this is kind of sketchy *\)
 *   | Call
 *       {
 *         callee = Var ({ name = "deref"; target = None; _ } as _callee);
 *         args = [ arg ];
 *         coraltype = _tt;
 *         _;
 *       } ->
 *       let llptr = run_func context arg in
 *       Llvm.build_load llptr "*" context.builder
 *   (\* Implement deref: this is kind of sketchy *\)
 *   | Call
 *       {
 *         callee = Var ({ name = "ptr_cast"; target = None; _ } as _callee);
 *         args = [ arg ];
 *         coraltype = Some (Ast.Parameterized ("%call", [ _x; y ]));
 *         _;
 *       } ->
 *       let llptr = run_func context arg in
 *       let target_type = llvmType context.llmodule context.context y in
 *       Llvm.build_pointercast llptr target_type "cast" context.builder
 *   (\* Tuple construction *\)
 *   | Call { callee = Var { target = Some (TupleDef tuple); _ }; args; _ } -> (
 *       match Llvm.type_by_name context.llmodule tuple.name with
 *       | Some lltupletype ->
 *           (\* start with Llvm.undef and fold into a fully defined struct *\)
 *           let fieldloop (i, structval) f =
 *             let value = run_func context f in
 *             (i + 1, Llvm.build_insertvalue structval value i "" context.builder)
 *           in
 *           snd @@ List.fold_left fieldloop (0, Llvm.undef lltupletype) args
 *       | None -> failwith ("unknown type" ^ tuple.name) )
 *   | Call { callee; args; _ } -> (
 *       let llcallee = run_func context callee in
 *       let llargs = List.map (run_func context) args |> Array.of_list in
 *       match Llvm.classify_value llcallee with
 *       | Llvm.ValueKind.Function ->
 *           Llvm.build_call llcallee llargs "" context.builder
 *       | _ ->
 *           Printf.printf "Failed to find function: \n";
 *           Ast.show callee;
 *           llcallee )
 *   | Var v_info -> (
 *       match v_info.target with
 *       | Some (Let (_var, _value) as letnode) -> (
 *           match AstMap.find_opt letnode context.llvalues with
 *           | Some value -> Llvm.build_load value v_info.name context.builder
 *           | None ->
 *               failwith ("llvmBackend: (let) failed to find var " ^ v_info.name)
 *           )
 *       | Some (Def _n as defnode) -> (
 *           match AstMap.find_opt defnode context.llvalues with
 *           | Some value -> Llvm.build_load value v_info.name context.builder
 *           | None ->
 *               failwith ("llvmBackend: (def) failed to find var " ^ v_info.name)
 *           )
 *       | Some (Func f) -> (
 *           match AstMap.find_opt (Func f) context.llvalues with
 *           | None -> failwith ("llvmBackend: (func) failed to find var " ^ f.name)
 *           | Some var -> var )
 *       | Some v -> (
 *           match AstMap.find_opt v context.llvalues with
 *           | Some value -> Llvm.build_load value v_info.name context.builder
 *           | None ->
 *               failwith
 *                 ("llvmBackend: (unknown type) failed to find var " ^ v_info.name)
 *           )
 *       | None ->
 *           let c = context.context in
 *           let f =
 *             Llvm.declare_function v_info.name
 *               (Llvm.function_type (Llvm.void_type c) [||])
 *               context.llmodule
 *           in
 *           f
 *           (\* else
 *            *   (Printf.printf "hm 2 %s \n" v_info.name;
 *            *   Llvm.const_int (Llvm.i32_type context.context) 0)) *\) )
 *   | StringLiteral s ->
 *       let _conststr = Llvm.const_string context.context s in
 *       let globstr = Llvm.build_global_stringptr s "" context.builder in
 *       globstr
 *   | Tuple [] | Empty -> Llvm.const_int (Llvm.i32_type context.context) 0
 *   | Let (var, value) as letnode -> (
 *       match var.varType with
 *       | None -> failwith ("allocating unknown variable type " ^ var.name)
 *       | Some vt ->
 *           let llvm_type = llvmType context.llmodule context.context vt in
 *           let alloca = Llvm.build_alloca llvm_type var.name context.builder in
 *           let llvalue = run_func context value in
 *           let _ = Llvm.build_store llvalue alloca context.builder in
 *           context.llvalues <- AstMap.add letnode alloca context.llvalues;
 *           alloca )
 *   | Member (mem : Ast.node Ast.memberInfo) ->
 *       let llbase = run_func context mem.base in
 *       Llvm.build_extractvalue llbase mem.memberIndex mem.memberName
 *         context.builder
 *   | n ->
 *       Printf.printf "Unhandled codegen: %s\n" (Ast.nodeName n);
 *       Llvm.const_int (Llvm.i32_type context.context) 0
 *
 * let jit coralModule =
 *   let llmodule = run1 AstMap.empty coralModule in
 *
 *   (\* TODO: we're generating untyped operations with implicit casts *\)
 *   ( match Llvm_analysis.verify_module llmodule with
 *   | None -> ()
 *   | Some e ->
 *       Printf.printf "%s\n" @@ Llvm.string_of_llmodule llmodule;
 *       Printf.printf "%s\n" (Ansicolor.as_color (Bold RED) e);
 *       flush stdout );
 *
 *   let passmgrbuilder = Llvm_passmgr_builder.create () in
 *   let module_passmgr = Llvm.PassManager.create () in
 *   ignore (Llvm_passmgr_builder.set_opt_level 3 passmgrbuilder);
 *   Llvm_passmgr_builder.populate_module_pass_manager module_passmgr
 *     passmgrbuilder;
 *   ignore (Llvm.PassManager.run_module llmodule module_passmgr);
 *
 *   try
 *     ignore (Llvm_executionengine.initialize ());
 *     let llengine = Llvm_executionengine.create llmodule in
 *     let ctype =
 *       Foreign.funptr (Ctypes.void @-> Ctypes.returning Ctypes.int32_t)
 *     in
 *     let init_func =
 *       Llvm_executionengine.get_function_address ".init" ctype llengine
 *     in
 *     ignore (init_func ())
 *   with exc -> Printexc.to_string exc |> print_string
 *
 * let run x = jit x *)
