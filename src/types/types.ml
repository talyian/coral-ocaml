module Term = struct type t = string ref end
module Builtin = struct type t = Func | Ptr | Type end

module Constraint = struct
  type t = Term of Term.t | Builtin of Builtin.t | HasType of t | Call of t * t list
end

module TypeSpec = struct type t = Term of Term.t | Applied of t * t list | InstanceOf of t end
module Graph = struct end

module Resolver : sig
  (* type t
   *
   * val add_named_term : t -> string -> t * Term.t
   * val solve : t -> Constraint.t -> TypeSpec.t *)
end = struct end
