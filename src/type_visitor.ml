open Coral_core
open Base

(* create a type solver environment specialized to our Ast nodes *)
module Env = Coral_types.Env2 (struct
  type t = Ast.t

  let sexp_of_t t = Ast.sexp_of_t t
  let string_of_t t = Ast.show_short t
end)

let rec visit names (t : Env.t) (node : Ast.t) : Env.t * Env.term =
  let recurse = visit names in
  match Ast.Sexp_ref.( ! ) node with
  | Ast.Module {lines; _} ->
      let t, lines = List.fold_map ~init:t ~f:recurse lines in
      let t, term = Env.add_term node t in
      (* TODO: add a struct type that encapsulates every child by name scope in module *)
      (t, term)
  | Ast.Binop _ -> failwith "Binop"
  | Ast.Block _ -> failwith "Block"
  | Ast.Builtin _ -> failwith "Builtin"
  | Ast.Call {callee; args} ->
      let t, callee_term = recurse t callee in
      let t, arg_terms = List.fold_map ~f:recurse ~init:t args in
      let t, call_term = Env.add_term node t in
      (* TODO: add a call node here. *)
      (t, call_term)
  | Ast.CharLiteral _ -> failwith "CharLiteral"
  | Ast.Comment _ -> failwith "Comment"
  | Ast.Decorated _ -> failwith "Decorated"
  | Ast.Empty -> failwith "Empty"
  | Ast.Extern _ -> failwith "Extern"
  | Ast.FloatLiteral _ -> failwith "FloatLiteral"
  | Ast.Func _ -> failwith "Func"
  | Ast.If _ -> failwith "If"
  | Ast.Import {path; names} ->
      Stdio.printf "import %s - %s\n" (String.concat ~sep:"." path)
        (String.concat ~sep:", " (List.map names ~f:Ast.show_importType)) ;
      Env.add_term node t
  | Ast.Index _ -> failwith "Index"
  | Ast.IntLiteral {value; _} -> Env.const_term_for (Env.ConstInt value) t
  | Ast.Let {name; typ; value} ->
      let t = recurse t value in
      Stdio.printf "let %s\n" name ; t
  | Ast.List _ -> failwith "List"
  | Ast.Member {base; member} ->
      let t = recurse t base in
      Stdio.printf "%s.%s\n" (Ast.show_short base) member ;
      t
  | Ast.TypeDecl {name; metatype= "struct"; fields} ->
      (* create a struct term *)
      let t, term = Env.add_term node t in
      let t, field_terms = List.fold_map ~init:t ~f:recurse fields in
      let fields =
        List.map2_exn fields field_terms ~f:(fun p t ->
            match Ast.Sexp_ref.( ! ) p with
            | Ast.Param {name; _} -> (name, t)
            | _ -> failwith "expected parameter") in
      let t = Env.term_is term (Env.StructDef {name; fields}) t in
      (t, term)
  | Ast.Var {name} -> (
    match Names.deref_var names node with
    | Some reference ->
        let t = recurse t reference in
        t
    | None ->
        Stdio.printf "%s\n" (Ast.show_short node) ;
        Env.add_term node t )
  | Ast.Overload _ -> failwith "Overload"
  | Ast.Param {typ; _} -> (
      let t, param_term = Env.add_term node t in
      match typ with
      | None -> (t, param_term)
      | Some typ ->
          let t, type_term = Env.add_term typ t in
          let t = Env.term_is param_term (Term type_term) t in
          (t, param_term) )
  | Ast.Return _ | Ast.Set _ | Ast.StringLiteral _ | Ast.Tuple _ | Ast.Type _ | Ast.TypeAlias _
   |Ast.Yield _ | _ ->
      failwith (Ast.show node)

(* The public-facing version of construct that hides t *)
let construct names node =
  let env = {Env.empty with debug= true} in
  visit names env node
