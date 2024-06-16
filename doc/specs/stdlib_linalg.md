---
title: linalg
---

# Linear Algebra

[TOC]

The `stdlib` linear algebra library provides high-level APIs for dealing with common linear algebra operations.

## BLAS and LAPACK

### Status

Experimental

### Description

`BLAS` and `LAPACK` backends provide efficient low level implementations of many linear algebra algorithms, and are employed for non-trivial operators. 
A Modern Fortran version of the [Reference-LAPACK 3.10.1](http://github.com/reference-LAPACK) implementation is provided as a backend. 
Modern Fortran modules with full explicit typing features are provided after an 
[automated conversion](https://github.com/perazz/fortran-lapack/blob/main/scripts/modularize_blas.py) 
of the legacy codes: 
- [stdlib_linalg_blas(module)], [stdlib_linalg_lapack(module)] provide kind-agnostic interfaces to all functions.
- Both libraries are available for 32- (`sp`), 64- (`dp`) and 128-bit (`qp`) `real` and `complex` numbers (the latter if available in the current build)
- Free format, lower-case style
- `implicit none(type, external)` applied to all procedures and modules
- `intent` added and all `pure` procedures where possible
- `stdlib` provides all procedures in two different flavors: (a) original BLAS/LAPACK names with a prefix `stdlib_?<name>` (ex: `stdlib_dgemv`, `stdlib_sgemv`); (b) A generic, kind agnostic `<name>`, i.e. `gemv`. 
- F77-style `parameter`s removed, and all numeric constants have been generalized with KIND-dependent Fortran intrinsics. 
- preprocessor-based OpenMP directives retained.
The single-source module structure hopefully allows for cross-procedural inlining which is otherwise impossible without link-time optimization.

When available, highly optimized libraries that take advantage of specialized processor instructions should be preferred over the `stdlib` implementation. 
Examples of such libraries are: OpenBLAS, MKL (TM), Accelerate, and ATLAS. In order to enable their usage, simply ensure that the following pre-processor macros are defined: 

- `STDLIB_EXTERNAL_BLAS`   wraps all BLAS procedures (except for the 128-bit ones) to an external library
- `STDLIB_EXTERNAL_LAPACK` wraps all LAPACK procedures (except for the 128-bit ones) to an external library

These can be enabled during the build process. For example, with CMake, one can enable these preprocessor directives using `add_compile_definitions(STDLIB_EXTERNAL_BLAS STDLIB_EXTERNAL_LAPACK)`.
The same is possible from the `fpm` branch, where the `cpp` preprocessor is enabled by default. For example, the macros can be added to the project's manifest:

```toml
# Link against appropriate external BLAS and LAPACK libraries, if necessary
[build]
link = ["blas", "lapack"]  

[dependencies]
stdlib="*"

# Macros are only needed if using an external library
[preprocess]
[preprocess.cpp]
macros = ["STDLIB_EXTERNAL_BLAS", "STDLIB_EXTERNAL_LAPACK"]
```

or directly via compiler flags: 

`fpm build --flag "-DSTDLIB_EXTERNAL_BLAS -DSTDLIB_EXTERNAL_LAPACK -lblas -llapack"`.

### Syntax 

All procedures in the `BLAS` and `LAPACK` backends follow the standard interfaces from the 
[Reference LAPACK](https://www.netlib.org/lapack/). So, the online [Users Guide](https://www.netlib.org/lapack/explore-html/)
should be consulted for the full API and descriptions of procedure arguments and their usage. 

The `stdlib` implementation makes both kind-agnostic and specific procedure interfaces available via modules
[stdlib_linalg_blas(module)] and [stdlib_linalg_lapack(module)]. Because all procedures start with a letter 
[that indicates the base datatype](https://www.netlib.org/lapack/lug/node24.html), the `stdlib` generic
interface drops the heading letter and contains all kind-dependent implementations. For example, the generic 
interface to the `axpy` function looks like: 

```fortran  
!> AXPY: constant times a vector plus a vector.
interface axpy
    module procedure stdlib_saxpy
    module procedure stdlib_daxpy
    module procedure stdlib_qaxpy
    module procedure stdlib_caxpy
    module procedure stdlib_zaxpy
    module procedure stdlib_waxpy
end interface axpy
```

The generic interface is the endpoint for using an external library. Whenever the latter is used, references
to the internal `module procedure`s are replaced with interfaces to the external library, 
for example: 

```fortran  
!> AXPY: constant times a vector plus a vector.
interface axpy
    pure subroutine caxpy(n,ca,cx,incx,cy,incy)
        import sp,dp,qp,ilp,lk 
        implicit none(type,external) 
        complex(sp), intent(in) :: ca,cx(*)
        integer(ilp), intent(in) :: incx,incy,n
        complex(sp), intent(inout) :: cy(*)
    end subroutine caxpy
    ! [....]
    module procedure stdlib_qaxpy
end interface axpy
```

Note that the 128-bit functions are only provided by `stdlib` and always point to the internal implementation. 
Because 128-bit precision is identified as [stdlib_kinds(module):qp], initials for 128-bit procedures were 
labelled as `q` (quadruple-precision reals) and `w` ("wide" or quadruple-precision complex numbers). 
Extended precision ([stdlib_kinds(module):xdp]) calculations are currently not supported.

### Example

```fortran
{!example/linalg/example_blas_gemv.f90!}
```

```fortran
{!example/linalg/example_lapack_getrf.f90!}
```

### Licensing

The Fortran Standard Library is distributed under the MIT License. `LAPACK` and its contained `BLAS` are a 
freely-available software package. They are available from [netlib](https://www.netlib.org/lapack/) via anonymous 
ftp and the World Wide Web. Thus, they can be included in commercial software packages (and have been). 
The license used for the `BLAS` and `LAPACK` backends is the [modified BSD license](https://www.netlib.org/lapack/LICENSE.txt).

The header of the `LICENSE.txt` file has as its licensing requirements:

    Copyright (c) 1992-2013 The University of Tennessee and The University
                            of Tennessee Research Foundation.  All rights
                            reserved.
    Copyright (c) 2000-2013 The University of California Berkeley. All
                            rights reserved.
    Copyright (c) 2006-2013 The University of Colorado Denver.  All rights
                            reserved.

    $COPYRIGHT$

    Additional copyrights may follow

    $HEADER$

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

    - Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    - Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer listed
      in this license in the documentation and/or other materials
      provided with the distribution.

    - Neither the name of the copyright holders nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

    The copyright holders provide no reassurances that the source code
    provided does not infringe any patent, copyright, or any other
    intellectual property rights of third parties.  The copyright holders
    disclaim any liability to any recipient for claims brought against
    recipient by any third party for infringement of that parties
    intellectual property rights.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

So the license for the `LICENSE.txt` code is compatible with the use of
modified versions of the code in the Fortran Standard Library under the MIT license.    
Credit for the `BLAS`, `LAPACK` libraries should be given to the [LAPACK authors](https://www.netlib.org/lapack/contributor-list.html).
According to the original license, we also changed the name of the routines and commented the changes made 
to the original.
    
## `diag` - Create a diagonal array or extract the diagonal elements of an array

### Status

Experimental

### Description

Create a diagonal array or extract the diagonal elements of an array

### Syntax

`d = ` [[stdlib_linalg(module):diag(interface)]] `(a [, k])`

### Arguments

`a`: Shall be a rank-1 or or rank-2 array. If `a` is a rank-1 array (i.e. a vector) then `diag` returns a rank-2 array with the elements of `a` on the diagonal. If `a` is a rank-2 array (i.e. a matrix) then `diag` returns a rank-1 array of the diagonal elements.

`k` (optional): Shall be a scalar of type `integer` and specifies the diagonal. The default `k = 0` represents the main diagonal, `k > 0` are diagonals above the main diagonal, `k < 0` are diagonals below the main diagonal.

### Return value

Returns a diagonal array or a vector with the extracted diagonal elements.

### Example

```fortran
{!example/linalg/example_diag1.f90!}
```

```fortran
{!example/linalg/example_diag2.f90!}
```

```fortran
{!example/linalg/example_diag3.f90!}
```

```fortran
{!example/linalg/example_diag4.f90!}
```

```fortran
{!example/linalg/example_diag5.f90!}
```

## `eye` - Construct the identity matrix

### Status

Experimental

### Class

Pure function.

### Description

Construct the identity matrix.

### Syntax

`I = ` [[stdlib_linalg(module):eye(function)]] `(dim1 [, dim2])`

### Arguments

`dim1`: Shall be a scalar of default type `integer`.
This is an `intent(in)` argument. 

`dim2`: Shall be a scalar of default type `integer`.
This is an `intent(in)` and `optional` argument. 

### Return value

Return the identity matrix, i.e. a matrix with ones on the main diagonal and zeros elsewhere. The return value is of type `integer(int8)`.
The use of `int8` was suggested to save storage.

#### Warning

Since the result of `eye` is of `integer(int8)` type, one should be careful about using it in arithmetic expressions. For example:
```fortran
!> Be careful
A = eye(2,2)/2     !! A == 0.0
!> Recommend
A = eye(2,2)/2.0   !! A == diag([0.5, 0.5])
```

### Example

```fortran
{!example/linalg/example_eye1.f90!}
```

```fortran
{!example/linalg/example_eye2.f90!}
```

## `trace` - Trace of a matrix

### Status

Experimental

### Description

Trace of a matrix (rank-2 array)

### Syntax

`result = ` [[stdlib_linalg(module):trace(interface)]] `(A)`

### Arguments

`A`: Shall be a rank-2 array. If `A` is not square, then `trace(A)` will return the sum of diagonal values from the square sub-section of `A`.

### Return value

Returns the trace of the matrix, i.e. the sum of diagonal elements.

### Example
```fortran
{!example/linalg/example_trace.f90!}
```

## `outer_product` - Computes the outer product of two vectors

### Status

Experimental

### Description

Computes the outer product of two vectors

### Syntax

`d = ` [[stdlib_linalg(module):outer_product(interface)]] `(u, v)`

### Arguments

`u`: Shall be a rank-1 array

`v`: Shall be a rank-1 array

### Return value

Returns a rank-2 array equal to `u v^T` (where `u, v` are considered column vectors). The shape of the returned array is `[size(u), size(v)]`.

### Example

```fortran
{!example/linalg/example_outer_product.f90!}
```

## `kronecker_product` - Computes the Kronecker product of two rank-2 arrays

### Status

Experimental

### Description

Computes the Kronecker product of two rank-2 arrays

### Syntax

`C = ` [[stdlib_linalg(module):kronecker_product(interface)]] `(A, B)`

### Arguments

`A`: Shall be a rank-2 array with dimensions M1, N1

`B`: Shall be a rank-2 array with dimensions M2, N2

### Return value

Returns a rank-2 array equal to `A \otimes B`. The shape of the returned array is `[M1*M2, N1*N2]`.

### Example

```fortran
{!example/linalg/example_kronecker_product.f90!}
```


## `cross_product` - Computes the cross product of two vectors

### Status

Experimental

### Description

Computes the cross product of two vectors

### Syntax

`c = ` [[stdlib_linalg(module):cross_product(interface)]] `(a, b)`

### Arguments

`a`: Shall be a rank-1 and size-3 array

`b`: Shall be a rank-1 and size-3 array

### Return value

Returns a rank-1 and size-3 array which is perpendicular to both `a` and `b`.

### Example

```fortran
{!example/linalg/example_cross_product.f90!}
```

## `is_square` - Checks if a matrix is square

### Status

Experimental

### Description

Checks if a matrix is square

### Syntax

`d = ` [[stdlib_linalg(module):is_square(interface)]] `(A)`

### Arguments

`A`: Shall be a rank-2 array

### Return value

Returns a `logical` scalar that is `.true.` if the input matrix is square, and `.false.` otherwise.

### Example

```fortran
{!example/linalg/example_is_square.f90!}
```

## `is_diagonal` - Checks if a matrix is diagonal

### Status

Experimental

### Description

Checks if a matrix is diagonal

### Syntax

`d = ` [[stdlib_linalg(module):is_diagonal(interface)]] `(A)`

### Arguments

`A`: Shall be a rank-2 array

### Return value

Returns a `logical` scalar that is `.true.` if the input matrix is diagonal, and `.false.` otherwise.
Note that nonsquare matrices may be diagonal, so long as `a_ij = 0` when `i /= j`.

### Example

```fortran
{!example/linalg/example_is_diagonal.f90!}
```

## `is_symmetric` - Checks if a matrix is symmetric

### Status

Experimental

### Description

Checks if a matrix is symmetric

### Syntax

`d = ` [[stdlib_linalg(module):is_symmetric(interface)]] `(A)`

### Arguments

`A`: Shall be a rank-2 array

### Return value

Returns a `logical` scalar that is `.true.` if the input matrix is symmetric, and `.false.` otherwise.

### Example

```fortran
{!example/linalg/example_is_symmetric.f90!}
```

## `is_skew_symmetric` - Checks if a matrix is skew-symmetric

### Status

Experimental

### Description

Checks if a matrix is skew-symmetric

### Syntax

`d = ` [[stdlib_linalg(module):is_skew_symmetric(interface)]] `(A)`

### Arguments

`A`: Shall be a rank-2 array

### Return value

Returns a `logical` scalar that is `.true.` if the input matrix is skew-symmetric, and `.false.` otherwise.

### Example

```fortran
{!example/linalg/example_is_skew_symmetric.f90!}
```

## `is_hermitian` - Checks if a matrix is Hermitian

### Status

Experimental

### Description

Checks if a matrix is Hermitian

### Syntax

`d = ` [[stdlib_linalg(module):is_hermitian(interface)]] `(A)`

### Arguments

`A`: Shall be a rank-2 array

### Return value

Returns a `logical` scalar that is `.true.` if the input matrix is Hermitian, and `.false.` otherwise.

### Example

```fortran
{!example/linalg/example_is_hermitian.f90!}
```

## `is_triangular` - Checks if a matrix is triangular

### Status

Experimental

### Description

Checks if a matrix is triangular

### Syntax

`d = ` [[stdlib_linalg(module):is_triangular(interface)]] `(A,uplo)`

### Arguments

`A`: Shall be a rank-2 array

`uplo`: Shall be a single character from `{'u','U','l','L'}`

### Return value

Returns a `logical` scalar that is `.true.` if the input matrix is the type of triangular specified by `uplo` (upper or lower), and `.false.` otherwise.
Note that the definition of triangular used in this implementation allows nonsquare matrices to be triangular.
Specifically, upper triangular matrices satisfy `a_ij = 0` when `j < i`, and lower triangular matrices satisfy `a_ij = 0` when `j > i`.

### Example

```fortran
{!example/linalg/example_is_triangular.f90!}
```

## `is_hessenberg` - Checks if a matrix is hessenberg

### Status

Experimental

### Description

Checks if a matrix is Hessenberg

### Syntax

`d = ` [[stdlib_linalg(module):is_hessenberg(interface)]] `(A,uplo)`

### Arguments

`A`: Shall be a rank-2 array

`uplo`: Shall be a single character from `{'u','U','l','L'}`

### Return value

Returns a `logical` scalar that is `.true.` if the input matrix is the type of Hessenberg specified by `uplo` (upper or lower), and `.false.` otherwise.
Note that the definition of Hessenberg used in this implementation allows nonsquare matrices to be Hessenberg.
Specifically, upper Hessenberg matrices satisfy `a_ij = 0` when `j < i-1`, and lower Hessenberg matrices satisfy `a_ij = 0` when `j > i+1`.

### Example

```fortran
{!example/linalg/example_is_hessenberg.f90!}
```

## `solve` - Solves a linear matrix equation or a linear system of equations. 

### Status

Experimental

### Description

This function computes the solution to a linear matrix equation \( A \cdot x = b \), where \( A \) is a square, full-rank, `real` or `complex` matrix.

Result vector or array `x` returns the exact solution to within numerical precision, provided that the matrix is not ill-conditioned. 
An error is returned if the matrix is rank-deficient or singular to working precision. 
The solver is based on LAPACK's `*GESV` backends.

### Syntax

`Pure` interface:

`x = ` [[stdlib_linalg(module):solve(interface)]] `(a, b)`

Expert interface:

`x = ` [[stdlib_linalg(module):solve(interface)]] `(a, b [, overwrite_a], err)`

### Arguments
 
`a`: Shall be a rank-2 `real` or `complex` square array containing the coefficient matrix. It is normally an `intent(in)` argument. If `overwrite_a=.true.`, it is an `intent(inout)` argument and is destroyed by the call. 

`b`: Shall be a rank-1 or rank-2 array of the same kind as `a`, containing the right-hand-side vector(s). It is an `intent(in)` argument.

`overwrite_a` (optional): Shall be an input logical flag. if `.true.`, input matrix `a` will be used as temporary storage and overwritten. This avoids internal data allocation. This is an `intent(in)` argument.

`err` (optional): Shall be a `type(linalg_state_type)` value. This is an `intent(out)` argument. The function is not `pure` if this argument is provided.

### Return value

For a full-rank matrix, returns an array value that represents the solution to the linear system of equations.

Raises `LINALG_ERROR` if the matrix is singular to working precision.
Raises `LINALG_VALUE_ERROR` if the matrix and rhs vectors have invalid/incompatible sizes.
If `err` is not present, exceptions trigger an `error stop`.

### Example

```fortran
{!example/linalg/example_solve1.f90!}

{!example/linalg/example_solve2.f90!}
```

## `solve_lu` - Solves a linear matrix equation or a linear system of equations (subroutine interface). 

### Status

Experimental

### Description

This subroutine computes the solution to a linear matrix equation \( A \cdot x = b \), where \( A \) is a square, full-rank, `real` or `complex` matrix.

Result vector or array `x` returns the exact solution to within numerical precision, provided that the matrix is not ill-conditioned. 
An error is returned if the matrix is rank-deficient or singular to working precision. 
If all optional arrays are provided by the user, no internal allocations take place.
The solver is based on LAPACK's `*GESV` backends.

### Syntax

Simple (`Pure`) interface:

`call ` [[stdlib_linalg(module):solve_lu(interface)]] `(a, b, x)`

Expert (`Pure`) interface:

`call ` [[stdlib_linalg(module):solve_lu(interface)]] `(a, b, x [, pivot, overwrite_a, err])`

### Arguments

`a`: Shall be a rank-2 `real` or `complex` square array containing the coefficient matrix. It is normally an `intent(in)` argument. If `overwrite_a=.true.`, it is an `intent(inout)` argument and is destroyed by the call. 

`b`: Shall be a rank-1 or rank-2 array of the same kind as `a`, containing the right-hand-side vector(s). It is an `intent(in)` argument.

`x`: Shall be a rank-1 or rank-2 array of the same kind and size as `b`, that returns the solution(s) to the system. It is an `intent(inout)` argument, and must have the `contiguous` property. 

`pivot` (optional): Shall be a rank-1 array of the same kind and matrix dimension as `a`, providing storage for the diagonal pivot indices. It is an `intent(inout)` arguments, and returns the diagonal pivot indices. 

`overwrite_a` (optional): Shall be an input logical flag. if `.true.`, input matrix `a` will be used as temporary storage and overwritten. This avoids internal data allocation. This is an `intent(in)` argument.

### Return value

For a full-rank matrix, returns an array value that represents the solution to the linear system of equations.

Raises `LINALG_ERROR` if the matrix is singular to working precision.
Raises `LINALG_VALUE_ERROR` if the matrix and rhs vectors have invalid/incompatible sizes.
If `err` is not present, exceptions trigger an `error stop`.

### Example

```fortran
{!example/linalg/example_solve3.f90!}
```

## `lstsq` - Computes the least squares solution to a linear matrix equation. 

### Status

Experimental

### Description

This function computes the least-squares solution to a linear matrix equation \( A \cdot x = b \).

Result vector `x` returns the approximate solution that minimizes the 2-norm \( || A \cdot x - b ||_2 \), i.e., it contains the least-squares solution to the problem. Matrix `A` may be full-rank, over-determined, or under-determined. The solver is based on LAPACK's `*GELSD` backends.

### Syntax

`x = ` [[stdlib_linalg(module):lstsq(interface)]] `(a, b, [, cond, overwrite_a, rank, err])`

### Arguments

`a`: Shall be a rank-2 `real` or `complex` array containing the coefficient matrix. It is an `intent(inout)` argument.

`b`: Shall be a rank-1 or rank-2 array of the same kind as `a`, containing one or more right-hand-side vector(s), each in its leading dimension. It is an `intent(in)` argument. 

`cond` (optional): Shall be a scalar `real` value cut-off threshold for rank evaluation: `s_i >= cond*maxval(s), i=1:rank`. Shall be a scalar, `intent(in)` argument.

`overwrite_a` (optional): Shall be an input `logical` flag. If `.true.`, input matrix `A` will be used as temporary storage and overwritten. This avoids internal data allocation. This is an `intent(in)` argument.

`rank` (optional): Shall be an `integer` scalar value, that contains the rank of input matrix `A`. This is an `intent(out)` argument.

`err` (optional): Shall be a `type(linalg_state_type)` value. This is an `intent(out)` argument.

### Return value

Returns an array value of the same kind and rank as `b`, containing the solution(s) to the least squares system. 

Raises `LINALG_ERROR` if the underlying Singular Value Decomposition process did not converge.
Raises `LINALG_VALUE_ERROR` if the matrix and right-hand-side vector have invalid/incompatible sizes.
Exceptions trigger an `error stop`.

### Example

```fortran
{!example/linalg/example_lstsq1.f90!}
```

## `solve_lstsq` - Compute the least squares solution to a linear matrix equation (subroutine interface). 

### Status

Experimental

### Description

This subroutine computes the least-squares solution to a linear matrix equation \( A \cdot x = b \).

Result vector `x` returns the approximate solution that minimizes the 2-norm \( || A \cdot x - b ||_2 \), i.e., it contains the least-squares solution to the problem. Matrix `A` may be full-rank, over-determined, or under-determined. The solver is based on LAPACK's `*GELSD` backends.

### Syntax

`call ` [[stdlib_linalg(module):solve_lstsq(interface)]] `(a, b, x, [, real_storage, int_storage, [cmpl_storage, ] cond, singvals, overwrite_a, rank, err])`

### Arguments

`a`: Shall be a rank-2 `real` or `complex` array containing the coefficient matrix. It is an `intent(inout)` argument.

`b`: Shall be a rank-1 or rank-2 array of the same kind as `a`, containing one or more right-hand-side vector(s), each in its leading dimension. It is an `intent(in)` argument. 

`x`: Shall be an array of same kind and rank as `b`, and leading dimension of at least `n`, containing the solution(s) to the least squares system. It is an `intent(inout)` argument.

`real_storage` (optional): Shall be a `real` rank-1 array of the same kind `a`, providing working storage for the solver. It minimum size can be determined with a call to [[stdlib_linalg(module):lstsq_space(interface)]]. It is an `intent(inout)` argument.

`int_storage` (optional): Shall be an `integer` rank-1 array, providing working storage for the solver. It minimum size can be determined with a call to [[stdlib_linalg(module):lstsq_space(interface)]]. It is an `intent(inout)` argument.

`cmpl_storage` (optional): For `complex` systems, it shall be a `complex` rank-1 array, providing working storage for the solver. It minimum size can be determined with a call to [[stdlib_linalg(module):lstsq_space(interface)]]. It is an `intent(inout)` argument.

`cond` (optional): Shall be a scalar `real` value cut-off threshold for rank evaluation: `s_i >= cond*maxval(s), i=1:rank`. Shall be a scalar, `intent(in)` argument.

`singvals` (optional): Shall be a `real` rank-1 array of the same kind `a` and size at least `minval(shape(a))`, returning the list of singular values `s(i)>=cond*maxval(s)`, in descending order of magnitude. It is an `intent(out)` argument.

`overwrite_a` (optional): Shall be an input `logical` flag. If `.true.`, input matrix `A` will be used as temporary storage and overwritten. This avoids internal data allocation. This is an `intent(in)` argument.

`rank` (optional): Shall be an `integer` scalar value, that contains the rank of input matrix `A`. This is an `intent(out)` argument.

`err` (optional): Shall be a `type(linalg_state_type)` value. This is an `intent(out)` argument.

### Return value

Returns an array value that represents the solution to the least squares system.

Raises `LINALG_ERROR` if the underlying Singular Value Decomposition process did not converge.
Raises `LINALG_VALUE_ERROR` if the matrix and right-hand-side vector have invalid/incompatible sizes.
Exceptions trigger an `error stop`.

### Example

```fortran
{!example/linalg/example_lstsq2.f90!}
```

## `lstsq_space` - Compute internal working space requirements for the least squares solver.

### Status

Experimental

### Description

This subroutine computes the internal working space requirements for the least-squares solver, [[stdlib_linalg(module):solve_lstsq(interface)]] .

### Syntax

`call ` [[stdlib_linalg(module):lstsq_space(interface)]] `(a, b, lrwork, liwork [, lcwork])`

### Arguments

`a`: Shall be a rank-2 `real` or `complex` array containing the linear system coefficient matrix. It is an `intent(in)` argument.

`b`: Shall be a rank-1 or rank-2 array of the same kind as `a`, containing the system's right-hand-side vector(s). It is an `intent(in)` argument. 

`lrwork`: Shall be an `integer` scalar, that returns the minimum array size required for the `real` working storage to this system.

`liwork`: Shall be an `integer` scalar, that returns the minimum array size required for the `integer` working storage to this system.

`lcwork` (`complex` `a`, `b`): For a `complex` system, shall be an `integer` scalar, that returns the minimum array size required for the `complex` working storage to this system.

## `det` - Computes the determinant of a square matrix

### Status

Experimental

### Description

This function computes the determinant of a `real` or `complex` square matrix.

This interface comes with a `pure` version `det(a)`, and a non-pure version `det(a,overwrite_a,err)` that
allows for more expert control.

### Syntax

`c = ` [[stdlib_linalg(module):det(interface)]] `(a [, overwrite_a, err])`

### Arguments

`a`: Shall be a rank-2 square array

`overwrite_a` (optional): Shall be an input `logical` flag. if `.true.`, input matrix `a` will be used as temporary storage and overwritten. This avoids internal data allocation.
 This is an `intent(in)` argument.

`err` (optional): Shall be a `type(linalg_state_type)` value.  This is an `intent(out)` argument.

### Return value

Returns a `real` scalar value of the same kind of `a` that represents the determinant of the matrix.

Raises `LINALG_ERROR` if the matrix is singular.
Raises `LINALG_VALUE_ERROR` if the matrix is non-square.
Exceptions are returned to the `err` argument if provided; an `error stop` is triggered otherwise.

### Example

```fortran
{!example/linalg/example_determinant.f90!}
```

## `.det.` - Determinant operator of a square matrix

### Status

Experimental

### Description

This operator returns the determinant of a real square matrix.

This interface is equivalent to the `pure` version of determinant [[stdlib_linalg(module):det(interface)]]. 

### Syntax

`c = ` [[stdlib_linalg(module):operator(.det.)(interface)]] `(a)`

### Arguments

`a`: Shall be a rank-2 square array of any `real` or `complex` kinds. It is an `intent(in)` argument.

### Return value

Returns a real scalar value that represents the determinnt of the matrix.

Raises `LINALG_ERROR` if the matrix is singular.
Raises `LINALG_VALUE_ERROR` if the matrix is non-square.
Exceptions trigger an `error stop`.

### Example

```fortran
{!example/linalg/example_determinant2.f90!}
```

## `svd` - Compute the singular value decomposition of a rank-2 array (matrix).

### Status

Experimental

### Description

This subroutine computes the singular value decomposition of a `real` or `complex` rank-2 array (matrix) \( A = U \cdot S \cdot \V^T \).
The solver is based on LAPACK's `*GESDD` backends.

Result vector `s` returns the array of singular values on the diagonal of \( S \). 
If requested, `u` contains the left singular vectors, as columns of \( U \).
If requested, `vt` contains the right singular vectors, as rows of \( V^T \).
 
### Syntax

`call ` [[stdlib_linalg(module):svd(interface)]] `(a, s, [, u, vt, overwrite_a, full_matrices, err])`

### Class
Subroutine

### Arguments

`a`: Shall be a rank-2 `real` or `complex` array containing the coefficient matrix of size `[m,n]`. It is an `intent(inout)` argument, but returns unchanged unless `overwrite_a=.true.`.

`s`: Shall be a rank-1 `real` array, returning the list of `k = min(m,n)` singular values. It is an `intent(out)` argument. 

`u` (optional): Shall be a rank-2 array of same kind as `a`, returning the left singular vectors of `a` as columns. Its size should be `[m,m]` unless `full_matrices=.false.`, in which case, it can be `[m,min(m,n)]`. It is an `intent(out)` argument.

`vt` (optional): Shall be a rank-2 array of same kind as `a`, returning the right singular vectors of `a` as rows. Its size should be `[n,n]` unless `full_matrices=.false.`, in which case, it can be `[min(m,n),n]`. It is an `intent(out)` argument.

`overwrite_a` (optional): Shall be an input `logical` flag. If `.true.`, input matrix `A` will be used as temporary storage and overwritten. This avoids internal data allocation. By default, `overwrite_a=.false.`. It is an `intent(in)` argument.

`full_matrices` (optional): Shall be an input `logical` flag. If `.true.` (default), matrices `u` and `vt` shall be full-sized. Otherwise, their secondary dimension can be resized to `min(m,n)`. See `u`, `v` for details.

`err` (optional): Shall be a `type(linalg_state_type)` value. This is an `intent(out)` argument.

### Return values

Returns an array `s` that contains the list of singular values of matrix `a`.
If requested, returns a rank-2 array `u` that contains the left singular vectors of `a` along its columns.
If requested, returns a rank-2 array `vt` that contains the right singular vectors of `a` along its rows.

Raises `LINALG_ERROR` if the underlying Singular Value Decomposition process did not converge.
Raises `LINALG_VALUE_ERROR` if the matrix or any of the output arrays invalid/incompatible sizes.
Exceptions trigger an `error stop`, unless argument `err` is present.

### Example

```fortran
{!example/linalg/example_svd.f90!}
```

## `svdvals` - Compute the singular values of a rank-2 array (matrix).

### Status

Experimental

### Description

This subroutine computes the singular values of a `real` or `complex` rank-2 array (matrix) from its singular 
value decomposition \( A = U \cdot S \cdot \V^T \). The solver is based on LAPACK's `*GESDD` backends.

Result vector `s` returns the array of singular values on the diagonal of \( S \). 
 
### Syntax

`s = ` [[stdlib_linalg(module):svdvals(interface)]] `(a [, err])`

### Arguments

`a`: Shall be a rank-2 `real` or `complex` array containing the coefficient matrix of size `[m,n]`. It is an `intent(in)` argument.

`err` (optional): Shall be a `type(linalg_state_type)` value. This is an `intent(out)` argument.

### Return values

Returns an array `s` that contains the list of singular values of matrix `a`.

Raises `LINALG_ERROR` if the underlying Singular Value Decomposition process did not converge.
Raises `LINALG_VALUE_ERROR` if the matrix or any of the output arrays invalid/incompatible sizes.
Exceptions trigger an `error stop`, unless argument `err` is present.

### Example

```fortran
{!example/linalg/example_svdvals.f90!}
```

