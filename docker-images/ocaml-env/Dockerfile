FROM ubuntu:20.04

WORKDIR /tmp
RUN apt-get update -y && apt-get install -y make m4 gcc wget patch unzip bubblewrap 
ADD https://github.com/ocaml/opam/releases/download/2.0.7/opam-2.0.7-x86_64-linux /usr/local/bin/opam
RUN chmod a+x /usr/local/bin/opam

RUN useradd -ms /bin/bash c
WORKDIR /home/c
USER c
RUN opam init --disable-sandboxing
RUN opam install dune
RUN echo eval $(opam env) >> ~/.bash_profile
