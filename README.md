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
GCC Fortran | 10, 11, 12, 13, 14 | Ubuntu 24.04.3 LTS | x86_64
GCC Fortran | 11, 12, 13, 14, 15 | macOS 14.8.2 (23J126) | Arm64
GCC Fortran (MSYS) | 13 | Windows Server 2022 (10.0.20348 Build 1547) | x86_64
GCC Fortran (MinGW) | 13 | Windows Server 2022 (10.0.20348 Build 1547) | x86_64
Intel oneAPI LLVM | 2024.1 | Ubuntu 24.04.3 LTS | x86_64
Intel oneAPI classic | 2021.10 | Ubuntu 22.04.5 LTS | x86_64

The following combinations are known to work, but they are not tested in the CI:

Name | Version | Platform | Architecture
--- | --- | --- | ---
GCC Fortran (MinGW) | 9.3.0, 10.2.0, 11.2.0 | Windows 10 | x86_64

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
- `-DFIND_BLAS` set to `off` in case you want to disable finding the external BLAS/LAPACK dependency (default: `on`).

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

**Option 1**: From root folder

As `fpm` does not currently support `fypp` natively, `stdlib` now proposes a python script to preprocess and build it.
This script enables modification of the different `fypp` macros available in `stdlib`. The preprocessed files will be dumped at `<current_folder>/temp/*.f90` or `*.F90`.

Make sure to install the dependencies from the `requirement.txt`
```sh
pip install --upgrade -r config/requirements.txt
```

To build, you can use the following command line:

```sh
python config/fypp_deployment.py
fpm build --profile release
```

or the short-cut

```sh
python config/fypp_deployment.py --build
```

To modify the `maxrank` macro for instance:
```sh
python config/fypp_deployment.py --maxrank 7 --build
```

To see all the options:
```sh
python config/fypp_deployment.py --help
```

**Note**: If you use a compiler different than GNU compilers, the script will try to catch it from the environment variables `FPM_FC`, `FPM_CC`, `FPM_CXX`.

**Option 2**: From the `stdlib-fpm` branch which has already been preprocessed with default macros:
```sh
git checkout stdlib-fpm
fpm build --profile release
```


#### Installing with fpm

Either option you chose for building the `stdlib`, you can install it with:
```sh
fpm install --profile release
```
The command above will install the following files:
- `libstdlib.a` into `~/.local/lib/` (Unix) or `C:\Users\<username>\AppData\Roaming\local\lib\` (Windows)
- all the `.[s]mod` files produced by the compiler into `~/.local/include/` (Unix) or `C:\Users\<username>\AppData\Roaming\local\include\` (Windows)

You can change the installation path by setting the prefix option to `fpm`:
```sh
fpm install --profile release --prefix /my/custom/installation/path/
```

You can use the `stdlib` by adding the `-lstdlib` flag to your compiler.
If your prefix is a non standard path, add also:
- `-L/my/custom/installation/path/lib`
- `-I/my/custom/installation/path/include`

#### Running the examples
You can run the examples with `fpm` as:

```sh
fpm run --example prog
```

with `prog` being the name of the example program (e.g., `example_sort`).


### Preprocessing macros and flags

`stdlib` uses two preprocessing steps:

- *fypp* for meta-programming (templating and feature selection)
- *C* preprocessing (compiler flag `-cpp`/`-fpp`) for conditional compilation

*fypp* preprocessing macros and flags are supported only through `CMake` and the
`python` script `config/fypp_deployment.sh`.
*C* preprocessing macros and flags are supported through `CMake` and `fpm`.

The table below lists all *fypp* preprocessing macros and flags currently used by `stdlib`:

| Macro/flag Name | Comments |
| --- | --- |
| `MAXRANK` |  Maximum array rank generated by templates. Set via CMake `-DCMAKE_MAXIMUM_RANK=<n>` (passed to fypp as `-DMAXRANK=<n>`), or via `python config/fypp_deployment.py --maxrank <n>`. |
| `VERSION90` |  Defines the default maximum rank generated when `MAXRANK` is not defined. If defined, the maximum rank generated is 7; otherwise it is 15. Can be passed to fypp as `-DVERSION90` (CMake sets this automatically in some configurations). |
| `WITH_CBOOL` |  Enables `c_bool` logical support if available. CMake auto-detects this and passes it to fypp; can be overridden at configure time with `-DWITH_CBOOL=ON/OFF`. |
| `WITH_QP` |  Enables quadruple precision code generation (`real(qp)`, `complex(qp)`). CMake auto-detects this and passes it to fypp; can be overridden at configure time with `-DWITH_QP=ON/OFF`; fypp deployment script: `--with_qp`. |
| `WITH_XDP` |  Enables extended double precision code generation (`real(xdp)`, `complex(xdp)`). CMake auto-detects this and passes it to fypp; can be overridden at configure time with `-DWITH_XDP=ON/OFF`; fypp deployment script: `--with_xdp`. |
| `WITH_ILP64` |  Enables generation of 64-bit integer (ILP64) size interfaces for BLAS and LAPACK (in addition to the default 32-bit interface). Set via CMake `-DWITH_ILP64=True` or via  `python config/fypp_deployment.py --with_ilp64`. |
| `PROJECT_VERSION_MAJOR` |  Part of the version string passed into fypp templates. Set automatically by CMake from the file `VERSION`. Can be overridden by passing `-DPROJECT_VERSION_MAJOR=<n>`. |
| `PROJECT_VERSION_MINOR` |  See `PROJECT_VERSION_MAJOR`. |
| `PROJECT_VERSION_PATCH` |  See `PROJECT_VERSION_MAJOR`. |


The table below lists all *C preprocessing* macros and flags currently used by `stdlib`:

| Macro/flag Name | Comments |
| --- | --- |
| `STDLIB_NO_BITSET` |  Disables compilation of the bitsets component. Set via CMake `-DSTDLIB_NO_BITSET=ON` or `fpm` `preprocess.cpp.macros=["STDLIB_NO_BITSET"]`; or define for the compiler preprocessor (`-DSTDLIB_NO_BITSET`). This makes `STDLIB_BITSET` evaluate to 0 via `include/macros.inc`. |
| `STDLIB_BITSET` |  Internal numeric macro (0/1) derived from `STDLIB_NO_BITSET` in `include/macros.inc`. Prefer setting `STDLIB_NO_BITSET` rather than defining `STDLIB_BITSET` directly. |
| `STDLIB_NO_STATS` |  Disables compilation of the statistics component. Set via CMake: `-DSTDLIB_NO_STATS=ON` or `fpm` `preprocess.cpp.macros=["STDLIB_NO_STATS"]`; or define for the compiler preprocessor (`-DSTDLIB_NO_STATS`). This makes `STDLIB_STATS` evaluate to 0 via `include/macros.inc`. |
| `STDLIB_STATS` |  Internal numeric macro (0/1) derived from `STDLIB_NO_STATS` in `include/macros.inc`. Prefer setting `STDLIB_NO_STATS` rather than defining `STDLIB_STATS` directly. |
| `STDLIB_EXTERNAL_BLAS` |  Links against an external BLAS (32-bit integer interface). Set automatically by CMake when external BLAS/LAPACK are found, or manually via `add_compile_definitions(STDLIB_EXTERNAL_BLAS)`. In fpm: `preprocess.cpp.macros=["STDLIB_EXTERNAL_BLAS"]`. |
| `STDLIB_EXTERNAL_LAPACK` |  Links against an external LAPACK (32-bit integer interface). Usually paired with `STDLIB_EXTERNAL_BLAS`. |
| `STDLIB_EXTERNAL_BLAS_I64` |  Links against an external BLAS with ILP64 (64-bit integer) interfaces. Usually paired with `STDLIB_EXTERNAL_LAPACK_I64`. |
| `STDLIB_EXTERNAL_LAPACK_I64` |  Links against an external LAPACK with ILP64 (64-bit integer) interfaces. |


## Using stdlib in your project

### Using stdlib with CMake

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

### Using stdlib with fpm

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

### Using stdlib with a regular Makefile

After the library has been built, it can be included in a regular Makefile.
The recommended way to do this is using the [pkg-config](https://www.freedesktop.org/wiki/Software/pkg-config/) tool, for which an example is shown below.
```make
# Necessary if the installation directory is not in PKG_CONFIG_PATH
install_dir := path/to/install_dir
export PKG_CONFIG_PATH := $(install_dir)/lib/pkgconfig:$(PKG_CONFIG_PATH)

STDLIB_CFLAGS := `pkg-config --cflags fortran_stdlib`
STDLIB_LIBS := `pkg-config --libs fortran_stdlib`

# Example definition of Fortran compiler and flags
FC := gfortran
FFLAGS := -O2 -Wall -g

# Definition of targets etc.
...

# Example rule to compile object files from .f90 files
%.o: %.f90
    $(FC) -c -o $@ $< $(FFLAGS) $(STDLIB_CFLAGS)

# Example rule to link an executable from object files
%: %.o
    $(FC) -o $@ $^ $(FFLAGS) $(STDLIB_LIBS)

```

The same can also be achieved without pkg-config.
If the library has been installed in a directory inside the compiler's search path,
only a flag `-lfortran_stdlib` is required.
If the installation directory is not in the compiler's search path, one can add for example
```make
install_dir := path/to/install_dir
libdir := $(install_dir)/lib
moduledir := $(install_dir)/include/fortran_stdlib/<compiler name and version>
```
The linker should then look for libraries in `libdir` (using e.g.`-L$(libdir)`) and the compiler should look for module files in `moduledir` (using e.g. `-I$(moduledir)`).
Alternatively, the library can also be included from a build directory without installation with
```make
build_dir := path/to/build_dir
libdir := $(build_dir)/src
moduledir := $(build_dir)/src/mod_files
```

## Documentation

Documentation is a work in progress (see issue [#4](https://github.com/fortran-lang/stdlib/issues/4)) but already available at [stdlib.fortran-lang.org](https://stdlib.fortran-lang.org).
This includes API documentation automatically generated from static analysis and markup comments in the source files
using the [FORD](https://github.com/Fortran-FOSS-programmers/ford/wiki) tool,
as well as a specification document or ["spec"](https://stdlib.fortran-lang.org/page/specs/index.html) for each proposed feature.

Some discussions and prototypes of proposed APIs along with a list of popular open source Fortran projects are available on the
[wiki](https://github.com/fortran-lang/stdlib/wiki).

## BLAS and LAPACK

`stdlib` ships full versions of BLAS and LAPACK, for all `real` and `complex` kinds, through generalized interface modules `stdlib_linalg_blas` and `stdlib_linalg_lapack`.
The 32- and 64-bit implementations may be replaced by external optimized libraries if available, which may allow for faster code.
When linking against external BLAS/LAPACK libraries, the user should define macros `STDLIB_EXTERNAL_BLAS` and `STDLIB_EXTERNAL_LAPACK`, 
to ensure that the external library version is used instead of the internal implementation. 

- In case of a CMake build, the necessary configuration can be added by ensuring both macros are defined:
  ```
  add_compile_definitions(STDLIB_EXTERNAL_BLAS STDLIB_EXTERNAL_LAPACK)
  ```
- In case of an `fpm` build, the stdlib dependency should be set as follows:
  ```toml
  [dependencies]
  stdlib = { git="https://github.com/fortran-lang/stdlib", branch="stdlib-fpm", preprocess.cpp.macros=["STDLIB_EXTERNAL_BLAS", "STDLIB_EXTERNAL_LAPACK"] }
  ```

Support for 64-bit integer size interfaces of all BLAS and LAPACK procedures may also be enabled 
by setting the CMake flag `-DWITH_ILP64=True`. The 64-bit integer version is always built in addition to 
the 32-bit integer version, that is always available. Additional macros `STDLIB_EXTERNAL_BLAS_I64` and `STDLIB_EXTERNAL_LAPACK_I64`  
may be defined to link against an external 64-bit integer library, such as Intel MKL. 

- In case of an `fpm` build, 64-bit integer linear algebra support is given via branch `stdlib-fpm-ilp64`:
  ```toml
  [dependencies]
  stdlib = { git="https://github.com/fortran-lang/stdlib", branch="stdlib-fpm-ilp64", preprocess.cpp.macros=["STDLIB_EXTERNAL_BLAS_I64", "STDLIB_EXTERNAL_LAPACK"] }
  ```

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
