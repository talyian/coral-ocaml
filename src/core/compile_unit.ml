open Base

(** A compile unit is a bundle of modules that get compiled at once. it is constructed by the
    closure of the dependencies of the main module *)
type t =
  { main: Ast.t
  ; (* all the modules in the compile unit, ordered in dependency order *)
    modules: Ast.t list
  ; (* A map from a module to all the modules it imports *)
    dependencies: Ast.t list Map.M(Ast).t
  ; (* A map from an import Ast node to the module it references *)
    import_references: Ast.t Map.M(Ast).t
  ; name_references: Ast.t Map.M(Ast).t
  ; types: Static_type.t Map.M(Ast).t }
