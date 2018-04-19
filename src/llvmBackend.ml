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

let rec llvmType context = function
  | Free -> failwith "cannot generate free type in llvm"
  | Type(s) ->
     (match s with
     | "Void" -> Llvm.void_type context
     | "Int8" -> Llvm.i8_type context
     | "Int32" -> Llvm.i32_type context
     | "Int64" -> Llvm.i64_type context
     | "Float64" -> Llvm.double_type context
     | "Float64" -> Llvm.double_type context
     | "Ptr" -> Llvm.pointer_type (Llvm.i8_type context)
     | "String" -> Llvm.pointer_type (Llvm.i8_type context)
     | "Tuple" -> Llvm.void_type context
     | _ -> Printf.eprintf "Unknown Type: '%s'\n" s; Llvm.i64_type context
     )
  | Parameterized(name, params) ->
     (match name with
      | "Ptr" -> Llvm.pointer_type (llvmType context (List.hd params))
      | "Func" ->
         (match List.rev params with
          | ret :: rparams ->
             Llvm.function_type (llvmType context ret)
               (rparams
                |> List.rev
                |> List.map (llvmType context)
                |> Array.of_list)
          | [] -> failwith "function: no return type!")
      | _ -> Printf.eprintf "Unknown Type: '%s'\n" (as_rgb (5, 1, 1) name); Llvm.i64_type context
     )
  | _ -> failwith "Unknown type"

let rec run1 llvalues = function
  | Module lines ->
     let lvContext = Llvm.global_context() in
     let lvModule = Llvm.create_module lvContext "module" in
     let rec looper llvalues = (function
       | [] ->
          lvModule
       | line :: xs ->
          (match line with
           | Func {name=name; ret_type=ret_type; params=params; body=body} as f ->
              let llret = llvmType lvContext ret_type in
              let vararg =
                params
                |> List.exists (function | {defType=Some(Type("..."))} -> true | _ -> false) in
              let llparams =
                params
                |> List.filter (function | {defType=Some(Type("..."))} -> false | _ -> true)
                |> List.map (fun v -> llvmType lvContext (default (Type "Int32") v.defType))
                |> Array.of_list in
              let func_type =
                if vararg then
                  Llvm.var_arg_function_type llret llparams
                else
                  Llvm.function_type llret llparams in
              let llfunc = Llvm.declare_function name func_type lvModule in
              (match body with
               | Empty -> ()
               | _ ->
                  let llbuilder = Llvm.builder lvContext in
                  let llblock = Llvm.append_block lvContext "entry" llfunc in
                  Llvm.position_at_end llblock llbuilder;
                  ignore (run_func {
                              context=lvContext;
                              llmodule=lvModule;
                              func=llfunc;
                              builder=llbuilder;
                              block=llblock;
                              isTerminated=false;
                              llvalues=(AstMap.add f llfunc llvalues)
                            } f));
              looper (AstMap.add f llfunc llvalues) xs
           | _ -> failwith "oops 2")) in looper llvalues lines
  | _ -> failwith "oops 1"

and run_func context = function
  | Func {name=name; ret_type=ret_type; params=params; body=body} as f ->
     let rec loop i p =
       match p.defType with
       | None -> failwith "def has no type"
       | Some(def_type) ->
           let lltype = llvmType context.context def_type in
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
         | "=" -> Llvm.build_fcmp Fcmp.Ueq lval rval "" context.builder
         | "+" -> Llvm.build_fadd lval rval "" context.builder
         | "-" -> Llvm.build_fsub lval rval "" context.builder
         | "*" -> Llvm.build_fmul lval rval "" context.builder
         | "/" -> Llvm.build_fdiv lval rval "" context.builder
         | "%" -> Llvm.build_frem lval rval "" context.builder
         | _ -> failwith ("unknown operator " ^ op))
     else
         (match op with
         | "<" -> Llvm.build_icmp Icmp.Slt lval rval "" context.builder
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
      | Empty -> Llvm.build_ret_void context.builder
      | Tuple([]) -> Llvm.build_ret_void context.builder
      | value -> Llvm.build_ret (run_func context value) context.builder)
  | Call (callee, args) ->
     let llcallee = (run_func context callee) in
     let llargs = List.map (run_func context) args |> Array.of_list in
     (* Llvm.build_call llcallee llargs "" context.builder *)
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
         let value = AstMap.find letnode context.llvalues in
         Llvm.build_load value v_info.name context.builder
      | Some(Def n as defnode) ->
         let value = AstMap.find defnode context.llvalues in
         Llvm.build_load value v_info.name context.builder
      | Some(v) ->
         let value = AstMap.find v context.llvalues in
         value
         (* Llvm.build_load value v_info.name context.builder *)
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
  | Empty -> Llvm.const_int (Llvm.i32_type context.context) 0
  | Let (var, value) as letnode ->
     (match var.varType with
      | None -> failwith ("allocating unknown variable type " ^  var.name)
      | Some(vt) ->
         let llvm_type = llvmType context.context vt in
         let alloca = Llvm.build_alloca llvm_type var.name context.builder in
         let llvalue = run_func context value in
         let sto = Llvm.build_store llvalue alloca context.builder in
         context.llvalues <- AstMap.add letnode alloca context.llvalues;
         alloca
     )
  | n ->
     Printf.printf "Unhandled codegen: ";
     Ast.show n;
     Llvm.const_int (Llvm.i32_type context.context) 0

let jit coralModule =
  let llmodule = run1 (AstMap.empty) coralModule in
  Llvm_analysis.assert_valid_module llmodule;

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
