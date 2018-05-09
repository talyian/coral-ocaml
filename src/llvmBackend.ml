open Llvm
open Ast
open Ctypes
open Ansicolor

module AstMap = Map.Make(struct type t = Ast.node let compare = compare end)

let default v = function | Some (x) -> x | None -> v

type codegenContext = {
    context: Llvm.llcontext;
    llmodule: Llvm.llmodule;
    func: Llvm.llvalue;
    builder: Llvm.llbuilder;
    mutable block: Llvm.llbasicblock;
    mutable isTerminated: bool;
    mutable llvalues: Llvm.llvalue AstMap.t
}

let rec llvmType llmodule context = function
  | Free -> failwith "cannot generate free type in llvm"
  | Type(s) ->
     (match s with
     | "Void" -> Llvm.void_type context
     | "Int8" -> Llvm.i8_type context
     | "Int32" -> Llvm.i32_type context
     | "Int64" -> Llvm.i64_type context
     | "Float32" -> Llvm.float_type context
     | "Float64" -> Llvm.double_type context
     | "Ptr" -> Llvm.pointer_type (Llvm.i8_type context)
     | "String" -> Llvm.pointer_type (Llvm.i8_type context)
     | "Tuple" -> Llvm.void_type context
     | name ->
        match Llvm.type_by_name llmodule name with
        | Some(v) ->
           (* Llvm.string_of_lltype v |> Printf.printf "lltype: %s\n";
            * flush stdout; *)
           v
        | None -> failwith ("llvmBackend: unknown type " ^ s))
  | Parameterized(name, params) ->
     (match name with
      | "Ptr" -> Llvm.pointer_type (llvmType llmodule context (List.hd params))
      | "Func" ->
         (match List.rev params with
          | ret :: rparams ->
             Llvm.function_type (llvmType llmodule context ret)
               (rparams
                |> List.rev
                |> List.map (llvmType llmodule context)
                |> Array.of_list)
          | [] -> failwith "function: no return type!")
      | _ -> failwith name;
     )
  | _ -> failwith "Unknown type"

let rec run1 llvalues = function
  | Module {name=module_name;lines=lines} ->
     let lvContext = Llvm.global_context() in
     let lvModule = Llvm.create_module lvContext module_name in
     let rec looper llvalues = (function
       | [] -> lvModule
       | Func func :: xs ->
          let llvalues = generate_function lvContext lvModule llvalues (Func func) in
          looper llvalues xs
       | Multifunc (name, func_list) :: xs ->
          let llvalues = List.fold_left
            (fun lv f -> generate_function lvContext lvModule lv (Func f))
            llvalues
            func_list in
          looper llvalues xs
       | TupleDef info as tuple :: rest ->
          let llfields =
            info.fields
            |> List.map (fun (name, ctype) -> llvmType lvModule lvContext ctype)
            |> Array.of_list in
          let tuple_type = Llvm.named_struct_type lvContext info.name in
          Llvm.struct_set_body tuple_type llfields true;
          (* Llvm.string_of_lltype tuple_type |> Printf.printf "tuple type: %s\n";
           * flush stdout; *)
          looper llvalues rest
       | x :: rest ->
          let err = "llvmBackend: unrecognized module item " ^ (Ast.nodeName x) in
          failwith err) in looper llvalues lines
  | _ -> failwith "oops 1"

and generate_function lvContext lvModule llvalues = function
  | Func func ->
     (* Printf.printf "defining function: %s \n" func.name; flush stdout; *)
     let llret = llvmType lvModule lvContext func.ret_type in
     let is_vararg = function | Def {defType=Some(Type("..."))} -> true | _ -> false in
     let vararg = List.exists is_vararg func.params in
     let llparams =
       func.params
       |> List.filter (fun x -> not @@ is_vararg x)
       |> List.map (function
              | Def (v) -> (* HACK: why are we defaulting to int32 here? *)
                 llvmType lvModule lvContext (default (Type "Int32") v.defType)
              | _ -> failwith "oops")
       |> Array.of_list in
     let func_type =
       if vararg then
         Llvm.var_arg_function_type llret llparams
       else
         Llvm.function_type llret llparams in
     (match func.body with
      | Empty ->
         let llfunc = Llvm.declare_function func.name func_type lvModule in
         let llvalues_with_func = AstMap.add (Func func) llfunc llvalues in
         llvalues_with_func
      | _ ->
         let llfunc = Llvm.define_function func.name func_type lvModule in
         let llvalues_with_func = AstMap.add (Func func) llfunc llvalues in
         let llbuilder = Llvm.builder lvContext in
         let llblock = Llvm.entry_block llfunc in
         (* let llblock = Llvm.append_block lvContext "entry" llfunc in *)
         Llvm.position_at_end llblock llbuilder;
         ignore (run_func {
                     context=lvContext;
                     llmodule=lvModule;
                     func=llfunc;
                     builder=llbuilder;
                     block=llblock;
                     isTerminated=false;
                     llvalues=llvalues_with_func
                   } (Func func));
         llvalues_with_func)
  | n -> failwith (Format.sprintf "Expected function, %s found" (Ast.nodeName n))

and run_func context =
  function
  | Func {name=name; ret_type=ret_type; params=params; body=body} ->
     let rec loop i (Def p) =
       match p.defType with
       | None -> failwith (Format.sprintf "%s: def [%s] has no type" name p.name)
       | Some(def_type) ->
           let lltype = llvmType context.llmodule context.context def_type in
           let lldef = Llvm.param context.func i in
           let alloca = Llvm.build_alloca lltype p.name context.builder in
           let store = Llvm.build_store lldef alloca context.builder in
           context.llvalues <- AstMap.add (Def p) alloca context.llvalues
     in List.iteri loop params;
     run_func context body
  | Block body ->
     let rec looper = function | [] -> ()
       | line :: xs ->
          run_func context line;
          looper xs in
     looper body;
     Llvm.const_int (Llvm.i32_type context.context) 0
  | If(cond, ifbody, elsebody) ->
     let llIfbody = Llvm.append_block context.context "if" context.func in
     let llElsebody = Llvm.append_block context.context "else" context.func in
     let bbafter = Llvm.append_block context.context "after" context.func in
     let llcond = run_func context cond in
     let llbr = Llvm.build_cond_br llcond llIfbody llElsebody context.builder in
     Llvm.position_at_end llIfbody context.builder;
     context.isTerminated <- false;
     context.block <- llIfbody;
     ignore (run_func context ifbody);
     if not context.isTerminated then ignore (Llvm.build_br bbafter context.builder);
     Llvm.position_at_end llElsebody context.builder;
     context.isTerminated <- false;
     context.block <- llElsebody;
     ignore (run_func context elsebody);
     if not context.isTerminated then ignore (Llvm.build_br bbafter context.builder);
     if context.isTerminated then
       Llvm.remove_block bbafter
     else
       Llvm.position_at_end bbafter context.builder;
     Llvm.const_int (Llvm.i32_type context.context) 0;
  | IntLiteral n -> Llvm.const_int (Llvm.i64_type context.context) (int_of_string n)
  | FloatLiteral n -> Llvm.const_float (Llvm.double_type context.context) (float_of_string n)
  | Binop(op, lhs, rhs) ->
     let get_value = function
       (* | Var v -> Llvm.build_load (run_func context (Var v)) v.name context.builder *)
       | node -> run_func context node in
     let lval = get_value lhs in
     let rval = get_value rhs in
     if Llvm.type_of lval = Llvm.double_type context.context then
         (match op with
         | "<" -> Llvm.build_fcmp Fcmp.Ult lval rval "" context.builder
         | "<=" -> Llvm.build_fcmp Fcmp.Ule lval rval "" context.builder
         | ">" -> Llvm.build_fcmp Fcmp.Ugt lval rval "" context.builder
         | ">=" -> Llvm.build_fcmp Fcmp.Uge lval rval "" context.builder
         | "=" -> Llvm.build_fcmp Fcmp.Ueq lval rval "" context.builder
         | "!=" -> Llvm.build_fcmp Fcmp.Une lval rval "" context.builder
         | "+" -> Llvm.build_fadd lval rval "" context.builder
         | "-" -> Llvm.build_fsub lval rval "" context.builder
         | "*" -> Llvm.build_fmul lval rval "" context.builder
         | "/" -> Llvm.build_fdiv lval rval "" context.builder
         | "%" -> Llvm.build_frem lval rval "" context.builder
         | _ -> failwith ("unknown operator " ^ op))
     else
         (match op with
         | "<" -> Llvm.build_icmp Icmp.Slt lval rval "" context.builder
         | "<=" -> Llvm.build_icmp Icmp.Sle lval rval "" context.builder
         | ">" -> Llvm.build_icmp Icmp.Sgt lval rval "" context.builder
         | ">=" -> Llvm.build_icmp Icmp.Sge lval rval "" context.builder
         | "!=" -> Llvm.build_icmp Icmp.Ne lval rval "" context.builder
         | "=" -> Llvm.build_icmp Icmp.Eq lval rval "" context.builder
         | "+" -> Llvm.build_add lval rval "" context.builder
         | "-" -> Llvm.build_sub lval rval "" context.builder
         | "*" -> Llvm.build_mul lval rval "" context.builder
         | "/" -> Llvm.build_sdiv lval rval "" context.builder
         | "%" -> Llvm.build_srem lval rval "" context.builder
         | _ -> failwith ("unknown operator " ^ op))
  | Def d_info -> Llvm.const_int (Llvm.i32_type context.context) 0;

  | Return(v) ->
     context.isTerminated <- true;
     (match v with
      | {node=value;coraltype=None} | {node=value;coraltype=Some(Ast.Type "Void")} ->
         ignore @@ run_func context value;
         Llvm.build_ret_void context.builder
      | {node=value} -> Llvm.build_ret (run_func context value) context.builder)
  | (* Implement ADDROF: this is kind of sketchy *)
    Call {callee=Var ({name="addrof"; target=None} as callee); args=[Var arg]} ->
     (* Printf.printf "var: %s of %s\n" callee.name arg.name; *)
     (match arg.target with
      | None -> failwith "(null)\n"
      | Some(decl) ->
         match AstMap.find_opt decl context.llvalues with
         | None -> failwith "(invalid reference)\n"
         | Some(target) -> target)
  | (* Tuple construction *)
    Call {callee=Var {target=Some(TupleDef tuple)}; args=args} ->
     (match Llvm.type_by_name context.llmodule tuple.name with
      | Some(lltupletype) ->
         (* start with Llvm.undef and fold into a fully defined struct *)
         let fieldloop (i, structval) f =
           let value = run_func context f in
           i + 1, Llvm.build_insertvalue structval value i "" context.builder in
         snd @@ List.fold_left fieldloop (0, Llvm.undef lltupletype) args
      | None -> failwith ("unknown type" ^ tuple.name))
  | Call {callee=callee; args=args} ->
     let llcallee = (run_func context callee) in
     let llargs = List.map (run_func context) args |> Array.of_list in
     (* Printf.printf "%s\n" (as_rgb (5, 0, 0) "calling ");
      * Ast.show callee;
      * let rec loop = function
      *   | [] -> ()
      *   | x :: xs -> print_char '('; Ast.show x; print_char ')'; loop xs in loop args; *)
     (match Llvm.classify_value llcallee with
      | Llvm.ValueKind.Function -> Llvm.build_call llcallee llargs "" context.builder
      | _ ->
         Printf.printf ("Failed to find function: \n");
         Ast.show callee;
         llcallee)
  | Var v_info ->
     (match v_info.target with
      | Some(Let (var, value) as letnode) ->
         (match AstMap.find_opt letnode context.llvalues with
          | Some(value) -> Llvm.build_load value v_info.name context.builder
          | None -> failwith ("llvmBackend: failed to find var " ^ v_info.name))
      | Some(Def n as defnode) ->
         (match AstMap.find_opt defnode context.llvalues with
          | Some(value) -> Llvm.build_load value v_info.name context.builder
          | None -> failwith ("llvmBackend: failed to find var " ^ v_info.name))
      | Some(Func f) ->
         (match AstMap.find_opt (Func f) context.llvalues with
         | None -> failwith ("llvmBackend: failed to find var " ^ f.name)
         | Some (var) -> var)
      | Some(v) ->
         (match AstMap.find_opt v context.llvalues with
          | Some(value) -> Llvm.build_load value v_info.name context.builder
          | None -> failwith ("llvmBackend: failed to find var " ^ v_info.name))
      | None ->
         let c = context.context in
         let f = (Llvm.declare_function
                    v_info.name
                    (Llvm.function_type (Llvm.void_type c) [| |]) context.llmodule) in
         f
         (* else
          *   (Printf.printf "hm 2 %s \n" v_info.name;
          *   Llvm.const_int (Llvm.i32_type context.context) 0)) *)
     )
  | StringLiteral s ->
     let conststr = Llvm.const_string context.context s in
     let globstr = Llvm.build_global_stringptr s "" context.builder in
     globstr
  | Tuple []
  | Empty -> Llvm.const_int (Llvm.i32_type context.context) 0
  | Let (var, value) as letnode ->
     (match var.varType with
      | None -> failwith ("allocating unknown variable type " ^  var.name)
      | Some(vt) ->
         let llvm_type = llvmType context.llmodule context.context vt in
         let alloca = Llvm.build_alloca llvm_type var.name context.builder in
         let llvalue = run_func context value in
         let sto = Llvm.build_store llvalue alloca context.builder in
         context.llvalues <- AstMap.add letnode alloca context.llvalues;
         alloca
     )
  | Member (mem:Ast.node Ast.memberInfo) ->
     let llbase = run_func context mem.base in
     Llvm.build_extractvalue llbase mem.memberIndex mem.memberName context.builder
  | n ->
     Printf.printf "Unhandled codegen: %s\n" (Ast.nodeName n);
     Llvm.const_int (Llvm.i32_type context.context) 0

let jit coralModule =
  let llmodule = run1 (AstMap.empty) coralModule in

  (* Llvm.string_of_llmodule llmodule |> print_endline;
   * flush stdout; *)

  (* TODO: we're generating untyped operations with implicit casts *)
  (* Llvm_analysis.assert_valid_module llmodule; *)

  let passmgrbuilder = Llvm_passmgr_builder.create () in
  let module_passmgr = Llvm.PassManager.create () in
  ignore (Llvm_passmgr_builder.set_opt_level 3 passmgrbuilder);
  Llvm_passmgr_builder.populate_module_pass_manager module_passmgr passmgrbuilder;
  ignore (Llvm.PassManager.run_module llmodule module_passmgr);

  try
    ignore (Llvm_executionengine.initialize());
    let llengine = Llvm_executionengine.create llmodule in
    let ctype = Foreign.funptr (Ctypes.void @-> Ctypes.returning Ctypes.int32_t) in
    let init_func = Llvm_executionengine.get_function_address ".init" ctype llengine in
    ignore (init_func ())
  with exc->
    Printexc.to_string exc |> print_string

let run x = jit x
