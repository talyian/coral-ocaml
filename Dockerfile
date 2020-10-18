FROM typhon/ocaml-env
ADD src src
ADD examples examples
ADD dune-project dune-project
USER root
RUN chown -R c src examples dune-project
USER c
RUN eval $(opam env); dune build; dune runtest