(* Here we define the behavior for builtins declared in coral_core/builtins. *)
open Coral_core
open Base

let initialize_names ~(init : 't) ~(f : string -> Coral_core.Ast.node -> 't -> 't) =
  let open Builtins in
  let builtin x = ref @@ Ast.Builtin {builtin= x; info= Ast.Info.create ()} in
  let overload name items =
    ref @@ Ast.Overload {name; items= List.map ~f:builtin items; info= Ast.Info.create ()} in
  init
  |> f "Func" (builtin FUNC)
  |> f "..." @@ builtin @@ ELLIPSIS
  |> f "Str" @@ builtin @@ STR
  |> f "Uint8" @@ builtin @@ UINT8
  |> f "Uint32" @@ builtin @@ UINT32
  |> f "Uint64" @@ builtin @@ UINT64
  |> f "Int8" @@ builtin @@ INT8
  |> f "Int32" @@ builtin @@ INT32
  |> f "Int64" @@ builtin @@ INT64
  |> f "IntSize" @@ builtin @@ INTNATIVE
  |> f "UintSize" @@ builtin @@ INTNATIVE
  |> f "Float64" @@ builtin @@ FLOAT64
  |> f "Void" @@ builtin @@ VOID
  |> f "Ptr" @@ builtin @@ PTR
  |> f "+" (overload "+" [ADD_INT; ADD_FLOAT; ADD_STR])
  |> f "-" (overload "-" [SUB_INT; SUB_FLOAT])
  |> f "*" (overload "*" [MUL_INT; MUL_FLOAT])
  |> f "/" (overload "/" [DIV_INT; DIV_FLOAT])
  |> f "%" (overload "%" [MOD_INT; MOD_FLOAT])
  |> f "=" (overload "=" [EQ_INT; EQ_FLOAT])
  |> f "!=" (overload "!=" [NEQ_INT; NEQ_FLOAT])
  |> f "<" (overload "<" [LT_INT; LT_FLOAT])
  |> f ">" (overload ">" [GT_INT; GT_FLOAT])
  |> f "<=" (overload "<=" [LTE_INT; LTE_FLOAT])
  |> f ">=" (overload ">=" [GTE_INT; GTE_FLOAT])
