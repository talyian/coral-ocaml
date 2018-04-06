open Llvm
open Ast
open Ctypes

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
        | _ -> Printf.eprintf "Unknown Type: '%s'\n" name; Llvm.i64_type context
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
           | Func (name, ret_type, params, body) as f ->
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
  | Func (name, ret_type, params, body) as f ->
     let rec loop i = function | [] -> ()
       | p :: xs ->
          let lldef = Llvm.param context.func i in
          context.llvalues <- AstMap.add (Def p) lldef context.llvalues;
          loop (i + 1) xs in
     loop 0 params;
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
     let recurse = run_func context in
     let apply f = f (recurse lhs) (recurse rhs) "" context.builder in
     (match op with
     | "<" -> apply (Llvm.build_icmp Icmp.Slt)
     | "=" -> apply (Llvm.build_icmp Icmp.Eq)
     | "+" -> apply Llvm.build_add
     | "-" -> apply Llvm.build_sub
     | "*" -> apply Llvm.build_mul
     | "/" -> apply Llvm.build_sdiv
     | "%" -> apply Llvm.build_srem
     | _ -> failwith ("unknown operator " ^ op))
  | Def d_info ->
     Llvm.const_int (Llvm.i32_type context.context) 0;
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
     (match Llvm.classify_value llcallee with
      | Llvm.ValueKind.Function -> Llvm.build_call llcallee llargs "" context.builder
      | _ ->
         Printf.printf ("Failed to find function: \n");
         Ast.show callee;
         llcallee)
  | Var v_info ->
     (match v_info.target with
      | Some(v) ->
         (try
            AstMap.find v context.llvalues
          with Not_found ->
            Printf.printf "hmmm %s \n" v_info.name;
            run_func context v)
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
  | n ->
     Printf.printf "Unhandled codegen: ";
     Ast.show n;
     Llvm.const_int (Llvm.i32_type context.context) 0

let jit coralModule =
  let llmodule = run1 (AstMap.empty) coralModule in
  Llvm_analysis.assert_valid_module llmodule;
  try
    ignore (Llvm_executionengine.initialize());
    let llengine = Llvm_executionengine.create llmodule in
    let ctype = Foreign.funptr (Ctypes.void @-> Ctypes.returning Ctypes.int32_t) in
    let init_func = Llvm_executionengine.get_function_address ".init" ctype llengine in
    ignore (init_func ());
    0
  with exc->
    Printexc.to_string exc |> print_string;
    0
let run x =
  (* run1 (AstMap.empty) x |> Llvm.string_of_llmodule |> print_string; *)
  flush stdout;
  jit x;
  0
