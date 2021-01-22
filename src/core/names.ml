open Base

module Member = struct
  module M = struct type t = {expr: Ast.t; member: string} [@@deriving compare, sexp] end
  include M
  include Comparable.Make (M)
end

type t = {names: Ast.t Map.M(Ast).t; returns: Ast.t Map.M(Ast).t; members: Ast.t Map.M(Member).t}

let deref_var t key = Map.find t.names key
let deref_return t key = Map.find t.returns key
let deref_member t key = Map.find t.members key
