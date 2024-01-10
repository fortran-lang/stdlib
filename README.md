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
  - [Build with fortran-lang/fpm](#build-with-fortran-langfpm)
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
GCC Fortran | 10, 11, 12, 13 | Ubuntu 22.04.2 LTS | x86_64
GCC Fortran | 10, 11, 12, 13 | macOS 12.6.3 (21G419) | x86_64
GCC Fortran (MSYS) | 13 | Windows Server 2022 (10.0.20348 Build 1547) | x86_64
GCC Fortran (MinGW) | 13 | Windows Server 2022 (10.0.20348 Build 1547) | x86_64, i686
Intel oneAPI LLVM | 2024.0 | Ubuntu 22.04.2 LTS | x86_64
Intel oneAPI classic | 2021.1 | macOS 12.6.3 (21G419) | x86_64

The following combinations are known to work, but they are not tested in the CI:

Name | Version | Platform | Architecture
--- | --- | --- | ---
GCC Fortran (MinGW) | 9.3.0, 10.2.0, 11.2.0 | Windows 10 | x86_64, i686

We try to test as many available compilers and platforms as possible.
A list of tested compilers which are currently not working and the respective issue are listed below.

Name | Version | Platform | Architecture | Status
--- | --- | --- | --- | ---
GCC Fortran | <9 | any | any | [#296](https://github.com/fortran-lang/stdlib/issues/296), [#430](https://github.com/fortran-lang/stdlib/pull/430)
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
- `-DCMAKE_INSTALL_PREFIX` is used to provide the install location for the library. If not provided the defaults will depend on your operating system, [see here](https://cmake.org/cmake/help/latest/variable/CMAKE_INSTALL_PREFIX.html). 
- `-DCMAKE_MAXIMUM_RANK` the maximum array rank procedures should be generated for.
  The default value is chosen as 4.
  The maximum is 15 for Fortran 2003 compliant compilers, otherwise 7 for compilers not supporting Fortran 2003 completely yet.
  The minimum required rank to compile this project is 4.
  Compiling with maximum rank 15 can be resource intensive and requires at least 16 GB of memory to allow parallel compilation or 4 GB memory for sequential compilation.
- `-DBUILD_SHARED_LIBS` set to `on` in case you want link your application dynamically against the standard library (default: `off`).
- `-DBUILD_TESTING` set to `off` in case you want to disable the stdlib tests (default: `on`).
- `-DCMAKE_VERBOSE_MAKEFILE` is by default set to `Off`, but if set to `On` will show commands used to compile the code.
- `-DCMAKE_BUILD_TYPE` is by default set to `RelWithDebInfo`, which uses compiler flags suitable for code development (but with only `-O2` optimization). Beware the compiler flags set this way will override any compiler flags specified via `FFLAGS`. To prevent this, use `-DCMAKE_BUILD_TYPE=NoConfig` in conjunction with `FFLAGS`.

For example, to configure a build using the Ninja backend while specifying compiler optimization via `FFLAGS`, generating procedures up to rank 7, installing to your home directory, using the `NoConfig` compiler flags, and printing the compiler commands, use

```sh
export FFLAGS="-O3"
cmake -B build -G Ninja -DCMAKE_MAXIMUM_RANK:String=7 -DCMAKE_INSTALL_PREFIX=$HOME/.local -DCMAKE_VERBOSE_MAKEFILE=On -DCMAKE_BUILD_TYPE=NoConfig
```

To build the standard library run

```sh
cmake --build build
```

To test your build, run the test suite and all example programs after the build has finished with

```sh
cmake --build build --target test
```

To test only the test suite, run
```sh
ctest --test-dir build/test
```

Please report failing tests on our [issue tracker](https://github.com/fortran-lang/stdlib/issues/new/choose) including details of the compiler used, the operating system and platform architecture.

To install the project to the declared prefix run

```sh
cmake --install build
```

Now you have a working version of stdlib you can use for your project.

If at some point you wish to recompile `stdlib` with different options, you might
want to delete the `build` folder. This will ensure that cached variables from
earlier builds do not affect the new build.

### Build with [fortran-lang/fpm](https://github.com/fortran-lang/fpm)

Fortran Package Manager (fpm) is a package manager and build system for Fortran.   
You can build `stdlib` using provided `fpm.toml`:

```sh
git checkout stdlib-fpm
fpm build --profile release
```

You can run the examples with `fpm` as:

```sh
fpm run --example prog
```

with `prog` being the name of the example program (e.g., `example_sort`).


To use `stdlib` within your `fpm` project, add the following lines to your `fpm.toml` file:
```toml
[dependencies]
stdlib = { git="https://github.com/fortran-lang/stdlib", branch="stdlib-fpm" }
```

> **Warning**
> 
> Fpm 0.9.0 and later implements stdlib as a *metapackage*.
> To include the standard library metapackage, change the dependency to:
> `stdlib = "*"`.
> 
> [see also](https://fpm.fortran-lang.org/spec/metapackages.html)

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
The usual install location of the package files is ``$PREFIX/lib/cmake/fortran_stdlib``.

## Documentation

Documentation is a work in progress (see issue [#4](https://github.com/fortran-lang/stdlib/issues/4)) but already available at [stdlib.fortran-lang.org](https://stdlib.fortran-lang.org).
This includes API documentation automatically generated from static analysis and markup comments in the source files
using the [FORD](https://github.com/Fortran-FOSS-programmers/ford/wiki) tool,
as well as a specification document or ["spec"](https://stdlib.fortran-lang.org/page/specs/index.html) for each proposed feature.

Some discussions and prototypes of proposed APIs along with a list of popular open source Fortran projects are available on the
[wiki](https://github.com/fortran-lang/stdlib/wiki).

## Contributing

* [Guidelines](CONTRIBUTING.md)
* [Issues](https://github.com/fortran-lang/stdlib/issues)
* [Workflow](WORKFLOW.md)
* [Style guide](STYLE_GUIDE.md)
* [Code of conduct](CODE_OF_CONDUCT.md)
* [License](LICENSE)

## Links

* [Proposals for the Fortran Standard Committee](https://github.com/j3-fortran/fortran_proposals/)
* [US Fortran Standards Committee](https://j3-fortran.org/)
* [International Fortran Standard Committee](https://wg5-fortran.org/)
