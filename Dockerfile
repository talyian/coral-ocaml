FROM ubuntu:bionic

RUN apt-get update -y && apt-get install -y emacs-nox wget
RUN apt-get update -y && apt-get install -y opam m4 cmake 

# install llvm
RUN echo "deb http://apt.llvm.org/bionic/ llvm-toolchain-bionic-6.0 main" > /etc/apt/sources.list.d/llvm.list
RUN echo "deb-src http://apt.llvm.org/bionic/ llvm-toolchain-bionic-6.0 main" > /etc/apt/sources.list.d/llvm.list
RUN wget -O /tmp/llvmkey https://apt.llvm.org/llvm-snapshot.gpg.key; apt-key add /tmp/llvmkey
RUN apt-get update -y && apt-get install -y clang-6.0 lldb-6.0 lld-6.0

# opam setup 
RUN opam init; opam switch 4.06.1;
RUN opam config setup -a
RUN apt-get install -y pkg-config
RUN opam install ocamlbuild menhir ounit ctypes-foreign llvm core
RUN echo ". /root/.opam/opam-init/init.sh || true" >> /root/.bashrc

WORKDIR /work
ADD src /work/src
ADD test_cases /work/test_cases
ADD Makefile /work/Makefile
RUN eval `opam config env` && make
CMD ["bash", "-i"]
