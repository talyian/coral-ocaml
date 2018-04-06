run: main.native
	./$<

_build/src/cutil.o: src/cutil.cc
	clang++-5.0 -c -o $@ $+

main.native: src/grammar.mly src/lexer.mll src/main.ml src/ast.ml src/init_func.ml \
 src/return_insert.ml \
 src/name_resolver.ml src/type_resolver.ml src/llvmBackend.ml _build/src/cutil.o
	ocamlbuild -use-ocamlfind -use-menhir -I src main.native \
	-package llvm \
	-package ctypes \
	-package llvm.executionengine \
	-package llvm.analysis \
	-package str
