# Fortran Standard Library

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

```
git clone https://github.com/fortran-lang/stdlib
cd stdlib
```

### Build with CMake

```
mkdir build
cd build
cmake ..
make
ctest
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
