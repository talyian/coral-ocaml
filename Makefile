run: main.native
	./$<

main.native: src/grammar.mly src/lexer.mll src/main.ml src/ast.ml src/init_func.ml \
 src/name_resolver.ml src/type_resolver.ml src/llvmBackend.ml
	ocamlbuild -use-ocamlfind -use-menhir -I src main.native -package llvm
