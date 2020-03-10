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
