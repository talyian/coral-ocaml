(* Here we define the behavior for builtins declared in coral_core/builtins. *)
open Coral_core
open Base

(* called by name resolver to set up the initial set of names *)
let initialize_names ~(init : 't) ~(f : string -> Coral_core.Ast.node -> 't -> 't) =
  let open Builtins in
  let builtin x = Ast.Sexp_ref.ref @@ Ast.Builtin {builtin= x} in
  let overload name items =
    Ast.Sexp_ref.ref @@ Ast.Overload {name; items= List.map ~f:builtin items} in
  let def = f in
  let custom_def name = def name (builtin (Custom name)) in
  init
  |> def "Func" (builtin FUNC)
  |> def "..." @@ builtin @@ ELLIPSIS
  |> def "Str" @@ builtin @@ STR
  |> def "Int8" @@ builtin @@ INT8
  |> def "Int32" @@ builtin @@ INT32
  |> def "Int64" @@ builtin @@ INT64
  |> def "Uint8" @@ builtin @@ UINT8
  |> def "Uint32" @@ builtin @@ UINT32
  |> def "Uint64" @@ builtin @@ UINT64
  |> def "IntSize" @@ builtin @@ INTNATIVE
  |> def "UintSize" @@ builtin @@ INTNATIVE
  |> def "Float64" @@ builtin @@ FLOAT64
  |> def "Void" @@ builtin @@ VOID
  |> def "Ptr" @@ builtin @@ PTR
  |> def "+" (overload "+" [ADD_INT; ADD_FLOAT; ADD_STR; ADD_PTR_INT])
  |> def "-" (overload "-" [SUB_INT; SUB_FLOAT; SUB_PTR_INT])
  |> def "*" (overload "*" [MUL_INT; MUL_FLOAT])
  |> def "/" (overload "/" [DIV_INT; DIV_FLOAT])
  |> def "%" (overload "%" [MOD_INT; MOD_FLOAT])
  |> def "=" (overload "=" [EQ_INT; EQ_FLOAT])
  |> def "!=" (overload "!=" [NEQ_INT; NEQ_FLOAT])
  |> def "<" (overload "<" [LT_INT; LT_FLOAT])
  |> def ">" (overload ">" [GT_INT; GT_FLOAT])
  |> def "<=" (overload "<=" [LTE_INT; LTE_FLOAT])
  |> def ">=" (overload ">=" [GTE_INT; GTE_FLOAT])
  |> custom_def "overload"
  |> def "typeof" @@ builtin @@ TYPEOF
  |> def "addrof" (builtin ADDROF)
  |> def "deref" (builtin DEREF)
  |> def "null" (builtin @@ Custom "NULL")
  |> def "i32" (builtin @@ Custom "i32")
  |> def "??" (builtin @@ Custom "??")
  |> custom_def "max" |> custom_def "yield"
