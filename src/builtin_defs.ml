(* Here we define the behavior for builtins declared in coral_core/builtins. *)
open Coral_core
open Base

(* called by name resolver to set up the initial set of names *)
let initialize_names ~(init : 't) ~(f : string -> Coral_core.Ast.node -> 't -> 't) =
  let open Builtins in
  let builtin x = ref @@ Ast.Builtin {builtin= x; info= Ast.Info.create ()} in
  let overload name items =
    ref @@ Ast.Overload {name; items= List.map ~f:builtin items; info= Ast.Info.create ()} in
  let def = f in
  init
  |> def "Func" (builtin FUNC)
  |> def "..." @@ builtin @@ ELLIPSIS
  |> def "Str" @@ builtin @@ STR
  |> def "Uint8" @@ builtin @@ UINT8
  |> def "Uint32" @@ builtin @@ UINT32
  |> def "Uint64" @@ builtin @@ UINT64
  |> def "Int8" @@ builtin @@ INT8
  |> def "Int32" @@ builtin @@ INT32
  |> def "Int64" @@ builtin @@ INT64
  |> def "IntSize" @@ builtin @@ INTNATIVE
  |> def "UintSize" @@ builtin @@ INTNATIVE
  |> def "Float64" @@ builtin @@ FLOAT64
  |> def "Void" @@ builtin @@ VOID
  |> def "Ptr" @@ builtin @@ PTR
  |> def "+" (overload "+" [ADD_INT; ADD_FLOAT; ADD_STR])
  |> def "-" (overload "-" [SUB_INT; SUB_FLOAT])
  |> def "*" (overload "*" [MUL_INT; MUL_FLOAT])
  |> def "/" (overload "/" [DIV_INT; DIV_FLOAT])
  |> def "%" (overload "%" [MOD_INT; MOD_FLOAT])
  |> def "=" (overload "=" [EQ_INT; EQ_FLOAT])
  |> def "!=" (overload "!=" [NEQ_INT; NEQ_FLOAT])
  |> def "<" (overload "<" [LT_INT; LT_FLOAT])
  |> def ">" (overload ">" [GT_INT; GT_FLOAT])
  |> def "<=" (overload "<=" [LTE_INT; LTE_FLOAT])
  |> def ">=" (overload ">=" [GTE_INT; GTE_FLOAT])
  |> def "overload" @@ builtin @@ Custom "overload"
