open Llvm
open Ast

module AstMap = Map.Make(struct type t = Ast.node let compare = compare end)

type codegenContext = {
    context: Llvm.llcontext;
    llmodule: Llvm.llmodule;
    func: Llvm.llvalue;
    builder: Llvm.llbuilder;
    block: Llvm.llbasicblock;
    mutable llvalues: Llvm.llvalue AstMap.t
}

let rec run1 llvalues = function
  | Module lines as m ->
     let lvContext = Llvm.global_context() in
     let lvModule = Llvm.create_module lvContext "module" in
     let rec looper llvalues = (function
       | [] ->
          Llvm.string_of_llmodule lvModule |> print_string
       | line :: xs ->
          (match line with
           | Func (name, ret_type, params, body) as f ->
              let i32t = Llvm.i32_type lvContext in
              let func_type = Llvm.function_type i32t [| i32t |] in
              let llfunc = Llvm.declare_function name func_type lvModule in
              let llbuilder = Llvm.builder lvContext in
              let llblock = Llvm.append_block lvContext "entry" llfunc in
              Llvm.position_at_end llblock llbuilder;
              (run_func {
                  context=lvContext;
                  llmodule=lvModule;
                  func=llfunc;
                  builder=llbuilder;
                  block=llblock;
                  llvalues=(AstMap.add f llfunc llvalues)
                 } f |> ignore);
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
     ignore (run_func context ifbody);
     Llvm.build_br bbafter context.builder;
     Llvm.position_at_end llElsebody context.builder;
     ignore (run_func context elsebody);
     Llvm.build_br bbafter context.builder;
     Llvm.position_at_end bbafter context.builder;
     Llvm.const_int (Llvm.i32_type context.context) 0;
  | IntLiteral n ->
     Llvm.const_int (Llvm.i32_type context.context) (int_of_string n)
  | Binop(op, lhs, rhs) ->
     let recurse = run_func context in
     Llvm.build_icmp Icmp.Slt (recurse lhs) (recurse rhs) "" context.builder
  | Def d_info ->
     Llvm.const_int (Llvm.i32_type context.context) 0;
  | Call (callee, args) ->
     let llcallee = (run_func context callee) in
     let llargs = List.map (run_func context) args |> Array.of_list in
     (* Llvm.build_call llcallee llargs "" context.builder *)
     (match Llvm.classify_value llcallee with
      | Llvm.ValueKind.Function ->
         Llvm.build_call llcallee llargs "" context.builder
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
         if v_info.name = "printf" then
           let f = (Llvm.declare_function
                      "printf"
                      (Llvm.function_type (Llvm.void_type c) [| |]) context.llmodule) in
           f
         else
           (Printf.printf "hm 2 %s \n" v_info.name;
           Llvm.const_int (Llvm.i32_type context.context) 0))
  | StringLiteral s -> Llvm.const_string context.context s
  | n ->
     Printf.printf "Unhandled codegen: ";
     Ast.show n;
     Llvm.const_int (Llvm.i32_type context.context) 0

let run x = run1 (AstMap.empty) x
