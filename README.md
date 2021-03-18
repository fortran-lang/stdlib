# Fortran Standard Library

[![Actions Status](https://github.com/fortran-lang/stdlib/workflows/CI/badge.svg)](https://github.com/fortran-lang/stdlib/actions)
[![Actions Status](https://github.com/fortran-lang/stdlib/workflows/CI_windows/badge.svg)](https://github.com/fortran-lang/stdlib/actions)

* [Goals and Motivation](#goals-and-motivation)
* [Scope](#scope)
* [Getting started](#getting-started)
  - [Get the code](#get-the-code)
  - [Requirements](#requirements)
  - [Supported compilers](#supported-compilers)
  - [Build with CMake](#build-with-cmake)
  - [Build with make](#build-with-make)
* [Using stdlib in your project](#using-stdlib-in-your-project)
* [Documentation](#documentation)
* [Contributing](#contributing)
* [Links](#links)

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

To build the Fortran standard library you need

- a Fortran 2008 compliant compiler, or better, a Fortran 2018 compliant compiler
  (GCC Fortran and Intel Fortran compilers are known to work for stdlib)
- CMake version 3.14 or newer (alternatively Make can be used)
- a build backend for CMake, like Make or Ninja (the latter is recommended on Windows)
- the [fypp](https://github.com/aradi/fypp) preprocessor (used as meta-programming tool)

If your system package manager does not provide the required build tools, all build dependencies can be installed with the Python command line installer ``pip``:

```sh
pip install --user fypp cmake ninja
```

Alternatively, you can install the build tools from the conda-forge channel with the conda package manager:

```sh
conda config --add channels conda-forge
conda create -n stdlib-tools fypp cmake ninja
conda activate stdlib-tools
```

You can install conda using the [miniforge installer](https://github.com/conda-forge/miniforge/releases).
Also, you can install a Fortran compiler from conda-forge by installing the ``fortran-compiler`` package, which installs GFortran.


### Supported Compilers

The following combinations are tested on the default branch of stdlib:

Name | Version | Platform | Architecture
--- | --- | --- | ---
GCC Fortran | 7, 8, 9, 10 | Ubuntu 18.04 | x86_64
GCC Fortran | 7, 8, 9, 10 | MacOS Catalina 10.15 | x86_64
GCC Fortran | 8 | Windows Server 2019 | x86_64
GCC Fortran (MSYS) | 10 | Windows Server 2019 | x86_64
GCC Fortran (MinGW) | 10 | Windows Server 2019 | x86_64, i686
Intel oneAPI classic | 2021.1 | Ubuntu 20.04 | x86_64

The following combinations are known to work, but they are not tested in the CI:

Name | Version | Platform | Architecture
--- | --- | --- | ---
GCC Fortran (MinGW) | 8.4.0, 9.3.0, 10.2.0 | Windows 10 | x86_64, i686

We try to test as many available compilers and platforms as possible.
A list of tested compilers which are currently not working and the respective issue are listed below.

Name | Version | Platform | Architecture | Status
--- | --- | --- | --- | ---
GCC Fortran | 7.4 | Windows 10 | i686 | [#296](https://github.com/fortran-lang/stdlib/issues/296)
NVIDIA HPC SDK | 20.7, 20.9, 20.11 | Manjaro Linux 20 | x86_64 | [#107](https://github.com/fortran-lang/stdlib/issues/107)
NAG | 7.0 | RHEL | x86_64 | [#108](https://github.com/fortran-lang/stdlib/issues/108)
Intel Parallel Studio XE | 16, 17, 18 | OpenSUSE | x86_64 | failed to compile

Please share your experience with successful and failing builds for compiler/platform/architecture combinations not covered above.


### Build with CMake

Configure the build with

```sh
cmake -B build
```

You can pass additional options to CMake to customize the build.
Important options are

- `-G Ninja` to use the Ninja backend instead of the default Make backend. Other build backends are available with a similar syntax.
- `-DCMAKE_INSTALL_PREFIX` is used to provide the install location for the library.
- `-DCMAKE_MAXIMUM_RANK` the maximum array rank procedures should be generated for.
  The default is 15 for Fortran 2003 compliant compilers, otherwise 7 for compilers not supporting Fortran 2003 completely yet.
  The minimum required rank to compile this project is 4.
  Compiling with maximum rank 15 can be resource intensive and requires at least 16 GB of memory to allow parallel compilation or 4 GB memory for sequential compilation.
- `-DBUILD_SHARED_LIBS` set to `on` in case you want link your application dynamically against the standard library (default: `off`).

For example, to configure a build using the Ninja backend and generating procedures up to rank 7, which is installed to your home directory use

```sh
cmake -B build -G Ninja -DCMAKE_MAXIMUM_RANK=7 -DCMAKE_INSTALL_PREFIX=$HOME/.local
```

To build the standard library run

```sh
cmake --build build
```

To test your build, run the test suite after the build has finished with

```sh
cmake --build build --target test
```

Please report failing tests on our [issue tracker](https://github.com/fortran-lang/stdlib/issues/new/choose) including details on the compiler used, the operating system and platform architecture.

To install the project to the declared prefix run

```sh
cmake --install build
```

Now you have a working version of stdlib you can use for your project.


### Build with make

Alternatively, you can build using provided Makefiles:

```sh
make -f Makefile.manual
```

You can limit the maximum rank by setting ``-DMAXRANK=<num>`` in the ``FYPPFLAGS`` environment variable:

```sh
make -f Makefile.manual FYPPFLAGS=-DMAXRANK=4
```



## Using stdlib in your project

The stdlib project exports CMake package files and pkg-config files to make stdlib usable for other projects.
The package files are located in the library directory in the installation prefix.

For CMake builds of stdlib you can find a local installation with

```cmake
find_package(fortran_stdlib REQUIRED)
...
target_link_libraries(
  ${PROJECT_NAME}
  PRIVATE
  fortran_stdlib::fortran_stdlib
)
```

To make the installed stdlib project discoverable add the stdlib directory to the ``CMAKE_PREFIX_PATH``.
The usual install localtion of the package files is ``$PREFIX/lib/cmake/fortran_stdlib``.

For non-CMake build systems (like make) you can use the exported pkg-config file by setting ``PKG_CONFIG_PATH`` to include the directory containing the exported pc-file.
The usual install location of the pc-file is ``$PREFIX/lib/pkgconfig``.
In make you can obtain the required compile and link arguments with

```make
STDLIB_CFLAGS := $(shell pkg-config --cflags fortran_stdlib)
STDLIB_LIBS := $(shell pkg-config --libs fortran_stdlib)
```

## Documentation

Documentation is a work in progress (see issue [#4](https://github.com/fortran-lang/stdlib/issues/4)) but already available at [stdlib.fortran-lang.org](https://stdlib.fortran-lang.org).
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
