module Type = struct
  type t = I8 | I16 | I32 | I64 | F64 | Ptr | Product of t list

  let rec size = function
    | I8 -> 1
    | I16 -> 2
    | I32 -> 4
    | I64 -> 8
    | F64 -> 8
    | Ptr -> 8
    | Product xs -> Base.List.sum (module Base.Int) ~f:size xs
    [@@deriving show]
end

module Value = struct
  type t =
    | I8 of int
    | I16 of int
    | I32 of Int32.t
    | I64 of Int64.t
    | F64 of float
    | Ptr of Nativeint.t
    | Product of t list
    | Var of Type.t
  [@@deriving_inline show]

  let _ = fun (_ : t) -> ()

  [@@@deriving.end]
end

type t
