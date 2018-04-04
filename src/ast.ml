type coraltype =
  | Free
  | Simple
  | Parameterized
  | Dotted

type node =
  | Module of (node list)
  | Func of (coraltype * node list * node)
  | Comment of (string)
  | Binop of (string * node * node)
