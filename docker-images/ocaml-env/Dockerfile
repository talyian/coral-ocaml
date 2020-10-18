FROM ubuntu:20.04

WORKDIR /tmp
RUN DEBIAN_FRONTEND='noninteractive' apt-get update -y && apt-get install -y make m4 gcc wget patch unzip bubblewrap cmake pkg-config
RUN DEBIAN_FRONTEND='noninteractive' libpcre2-dev llvm-10-dev clang-10 clang++-10 python2.7  
ADD https://github.com/ocaml/opam/releases/download/2.0.7/opam-2.0.7-x86_64-linux /usr/local/bin/opam
RUN update-alternatives --install /usr/bin/cc cc /usr/bin/clang-10 100
RUN update-alternatives --install /usr/bin/c++ c++ /usr/bin/clang++-10 100
RUN chmod a+x /usr/local/bin/opam

RUN useradd -ms /bin/bash c
WORKDIR /home/c
USER c
RUN opam init --disable-sandboxing
RUN opam install -y dune
RUN opam install -y core async
RUN opam install -y ctypes-foreign re
RUN opam install -y ppx_inline_test ppx_deriving ppx_fields_conv ppx_let ppx_variants_conv 
RUN opam install -y llvm
RUN opam install -y menhir 
RUN echo eval $(opam env) >> ~/.bash_profile
