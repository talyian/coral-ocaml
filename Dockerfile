FROM typhon/ocaml-env
USER root
RUN DEBIAN_FRONTEND="noninteractive" apt-get install -y pkg-config llvm-10
USER c
ADD src src
ADD examples examples
ADD dune-project dune-project
RUN eval $(opam env); opam install -y core async ppx_inline_test ctypes ppx_deriving ppx_fields_conv ppx_let ppx_variants_conv re
RUN opam install -y llvm
RUN dune runtest