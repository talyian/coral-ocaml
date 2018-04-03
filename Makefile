run: main.native
	./$<

main.native: src/grammar.mly src/lexer.mll src/main.ml
	ocamlbuild -use-ocamlfind -use-menhir -I src main.native -package llvm
