# Coral-Ocaml

[![Build Status](https://travis-ci.org/talyian/coral-ocaml.svg?branch=master)](https://travis-ci.org/talyian/coral-ocaml)

A Coral compiler. 

### Example Code

Check the `test_cases` directory for more examples

    func fizzbuzz(n):
      if n % 15 = 0: printf "fizzbuzz\n"
      else if n % 3 = 0: printf "fizz "
      else if n % 5 = 0: printf "buzz "
      else: printf("%ld ", n)

### Project Status

Still in early development. 

The roadmap includes union types, pointers, importing, anonymous tuples.

### Building

Building this project requires Ocaml 4.06 and a number of OPAM modules. See the included Dockerfile for a reproducible build environment
