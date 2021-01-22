(** Name Resolution module

    Resolves the targets for Var, Return, and Member types. Built by top-down traversal of the AST. *)
type t

(* for development; dump the state of the resolved maps *)
val show : t -> unit

(* Construct Name_resolution.t by traversing an module. Because name resolution relies on import
   statements, this necessarily depends on an Imports.t *)
val construct : Coral_frontend.Import_resolution.Imports.t -> t
val get_data : t -> Coral_core.Names.t
