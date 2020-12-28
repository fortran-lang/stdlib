# Fortran Standard Library

[![Actions Status](https://github.com/fortran-lang/stdlib/workflows/CI/badge.svg)](https://github.com/fortran-lang/stdlib/actions)
[![Actions Status](https://github.com/fortran-lang/stdlib/workflows/CI_windows/badge.svg)](https://github.com/fortran-lang/stdlib/actions)


## Goals and Motivation

The Fortran Standard, as published by the ISO (https://wg5-fortran.org/), does
not have a Standard Library. The goal of this project is to provide a community
driven and agreed upon *de facto* "standard" library for Fortran, called a
Fortran Standard Library (`stdlib`). We have a rigorous process how `stdlib` is
developed as documented in our [Workflow](WORKFLOW.md). `stdlib` is both a
specification and a reference implementation. We are cooperating with the
Fortran Standards Committee (e.g., the effort
[started](https://github.com/j3-fortran/fortran_proposals/issues/104) at the J3
committee repository) and the plan is to continue working with the Committee in
the future (such as in the step 5. in the [Workflow](WORKFLOW.md) document), so
that if the Committee wants to standardize some feature already available in `stdlib`, it would
base it on `stdlib`'s implementation.

## Scope

The goal of the Fortran Standard Library is to achieve the following general scope:

* Utilities (containers, strings, files, OS/environment integration, unit
  testing & assertions, logging,  ...)
* Algorithms (searching and sorting, merging, ...)
* Mathematics (linear algebra, sparse matrices, special functions, fast Fourier
  transform, random numbers, statistics, ordinary differential equations,
  numerical integration, optimization, ...)


## Getting started

### Get the code

```sh
git clone https://github.com/fortran-lang/stdlib
cd stdlib
```

### Requirements

The preprocessor ```fypp``` (https://github.com/aradi/fypp) is needed because metaprogramming is used.
It can be installed using the command line installer ```pip```.
```sh
pip install fypp
```

### Build with CMake

```sh
cmake -B build

cmake --build build

cmake --build build --target test
```

### Build with make

Alternatively, you can build using provided Makefiles:

```
make -f Makefile.manual
```

## Limiting the maximum rank of generated procedures

Stdlib's preprocessor (fypp) by default generates specific procedures for arrays of all ranks, up to rank 15.
This can result in long compilation times and, on some computers, exceeding available memory.
If you know that you won't need all 15 ranks, you can specify the maximum rank for which the specific procedures will be generated.
For example, with CMake:

```sh
cmake -B build -DCMAKE_MAXIMUM_RANK=4
cmake --build build
￼cmake --build build --target test
```
or as follows with `make`:

```sh
make -f Makefile.manual FYPPFLAGS=-DMAXRANK=4
```
Note that currently the minimum value for maximum rank is 4.

## Documentation

Documentation is a work in progress (see issue #4) but is currently available at https://stdlib.fortran-lang.org.
This includes API documentation automatically generated from static analysis and markup comments in the source files
using the [FORD](https://github.com/Fortran-FOSS-programmers/ford/wiki) tool,
as well as a specification document or ["spec"](https://stdlib.fortran-lang.org/page/specs/index.html) for each proposed feature.

Some discussions and prototypes of proposed APIs along with a list of popular open source Fortran projects are available on the
[wiki](https://github.com/fortran-lang/stdlib/wiki).

## Contributing

* [Issues](https://github.com/fortran-lang/stdlib/issues)
* [Workflow](WORKFLOW.md)
* [Style guide](STYLE_GUIDE.md)
* [Code of conduct](CODE_OF_CONDUCT.md)
* [License](LICENSE)

## Links

* [Proposals for the Fortran Standard Committee](https://github.com/j3-fortran/fortran_proposals/)
* [US Fortran Standards Committee](https://j3-fortran.org/)
* [International Fortran Standard Committee](https://wg5-fortran.org/)
