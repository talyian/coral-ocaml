module NodeMain = Coral_core.Ast

(* Type inference is a bit wonky with unnamed types; This creates an easy-to-reference type name so
   that the generated grammar.mli can have a solid type to declare *)
type main_type = Main of Coral_core.Ast.node
