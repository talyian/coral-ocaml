# run_tr: type_resolver.native
# 	./$<

# run_type: test_type_graph_2.native
# 	./$<

run: main.native
	./$<

watch:
	inotifymake -w src Makefile samples

_build/src/cutil.o: src/cutil.cc
	clang++-5.0 -c -o $@ $+

test_type_graph_2.native: src/test/test_type_graph_2.ml src/type_graph_2.ml
	ocamlbuild -use-ocamlfind -I src -I src/test $@ -tag debug \
	-package str

type_resolver.native: src/type_resolver.ml src/type_graph_2.ml
	ocamlbuild -use-ocamlfind -I src -I src/test $@ -tag debug \
	-package str

main.native: src/grammar.mly src/lexer.mll src/lexerInterface.ml \
 src/main.ml src/tests.ml src/ast.ml src/init_func.ml \
 src/return_insert.ml src/ansicolor.ml src/coralModule.ml \
 src/name_resolver.ml src/multifunc.ml src/type_resolver.ml \
 src/type_graph_2.ml src/llvmBackend.ml _build/src/foobar.o
	ocamlbuild -verbose 1 -use-ocamlfind -use-menhir -I src $@ \
	-lflags -cclib,src/foobar.o \
	-tag debug \
	-package llvm \
	-package ctypes \
	-package llvm.executionengine \
	-package llvm.analysis \
	-package llvm.passmgr_builder \
	-package llvm \
	-package str \
	-package core \
	-package oUnit

_build/src/foobar.o: src/foobar.c
	${CC} -c -o $@ $+
