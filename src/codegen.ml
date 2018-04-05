open Ast

(* Make sure we're ready to codegen *)
let rec min_verify lines = function
  | Module lines -> []
  | Func (name, ret_type, params, body) ->
     []
  | _ -> lines
and min_verify_module_line = function
  | Func (name, ret_type, params, body) -> min_verify_function_body body
  | _ -> []
and min_verify_function_body = function
  | _ -> []
