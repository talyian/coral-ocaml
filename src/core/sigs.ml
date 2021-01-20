module Astnode = Ast

module type Names = sig
  type t

  val create : root:Astnode.node -> t
  val deref : Astnode.node -> t -> Astnode.node
  val deref_or_self : Astnode.node -> t -> Astnode.node
end
