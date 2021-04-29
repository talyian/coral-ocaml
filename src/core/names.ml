(** the Names module stores the results of the lexical pass where we resolve identifiers, members
    and returns. *)
open Base

module Member = struct
  module M = struct type t = {expr: Ast.t; member: string} [@@deriving compare, sexp] end
  include M
  include Comparable.Make (M)
end

type t = {names: Ast.t Map.M(Ast).t; returns: Ast.t Map.M(Ast).t; members: Ast.t Map.M(Member).t}
[@@deriving sexp]

let deref_var t key = Map.find t.names key
let deref_return t key = Map.find t.returns key
let deref_member t expr member = Map.find t.members {Member.expr; member}
