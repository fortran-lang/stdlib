---
title: specialmatrices
---

# The `stdlib_specialmatrices` module

[TOC]

## Introduction

The `stdlib_specialmatrices` module provides derived types and specialized drivers for highly structured matrices often encountered in scientific computing as well as control and signal processing applications.
These include:

- Tridiagonal matrices
- Symmetric Tridiagonal matrices
- Hermitian Tridiagonal matrices
- Circulant matrices (not yet supported)
- Toeplitz matrices (not yet supported)
- Hankel matrices (not yet supported)

In addition, it also provides a `Poisson2D` matrix type (not yet supported) corresponding to the sparse block tridiagonal matrix obtained from discretizing the Laplace operator on a 2D grid with the standard second-order accurate central finite-difference scheme.

## List of derived types for special matrices

Below is a list of the currently supported derived types corresponding to different special matrices.
Note that this module is under active development and this list will eventually grow.

### Tridiagonal matrices {#Tridiagonal}

#### Status

Experimental

#### Description

Tridiagonal matrices are ubiquituous in scientific computing and often appear when discretizing 1D differential operators.
A generic tridiagonal matrix has the following structure:
$$
    A
    =
    \begin{bmatrix}
        a_1 &  b_1  \\
        c_1 &  a_2      &  b_2  \\
            &  \ddots   &  \ddots   &  \ddots   \\
            &           &  c_{n-2} &  a_{n-1}  &  b_{n-1} \\
            &           &           &  c_{n-1} &  a_n
    \end{bmatrix}.
$$
Hence, only one vector of size `n` and two of size `n-1` need to be stored to fully represent the matrix.
This particular structure also lends itself to specialized implementations for many linear algebra tasks.
Interfaces to the most common ones will soon be provided by `stdlib_specialmatrices`.
Tridiagonal matrices are available with all supported data types as `tridiagonal_<kind>_type`, for example:

- `tridiagonal_sp_type`   : Tridiagonal matrix of size `n` with `real`/`single precision` data.
- `tridiagonal_dp_type`   : Tridiagonal matrix of size `n` with `real`/`double precision` data.
- `tridiagonal_xdp_type`  : Tridiagonal matrix of size `n` with `real`/`extended precision` data.
- `tridiagonal_qp_type`   : Tridiagonal matrix of size `n` with `real`/`quadruple precision` data.
- `tridiagonal_csp_type`  : Tridiagonal matrix of size `n` with `complex`/`single precision` data.
- `tridiagonal_cdp_type`  : Tridiagonal matrix of size `n` with `complex`/`double precision` data.
- `tridiagonal_cxdp_type` : Tridiagonal matrix of size `n` with `complex`/`extended precision` data.
- `tridiagonal_cqp_type`  : Tridiagonal matrix of size `n` with `complex`/`quadruple precision` data.


#### Syntax

- To construct a tridiagonal matrix from already allocated arrays `dl` (lower diagonal, size `n-1`), `dv` (main diagonal, size `n`) and `du` (upper diagonal, size `n-1`):

`A = ` [[stdlib_specialmatrices(module):tridiagonal(interface)]] `(dl, dv, du)`

- To construct a tridiagonal matrix of size `n x n` with constant diagonal elements `dl`, `dv`, and `du`:

`A = ` [[stdlib_specialmatrices(module):tridiagonal(interface)]] `(dl, dv, du, n)`

#### Example

```fortran
{!example/specialmatrices/example_tridiagonal_dp_type.f90!}
```

### Symmetric tridiagonal matrices {#SymTridiagonal}

#### Status

Experimental

#### Description

Symmetric tridiagonal matrices are ubiquituous in scientific computing and often appear when discretizing 1D differential operators.
A generic symmetric tridiagonal matrix has the following structure:
$$
    A
    =
    \begin{bmatrix}
        a_1 &  b_1  \\
        b_1 &  a_2      &  b_2  \\
            &  \ddots   &  \ddots   &  \ddots   \\
            &           &  b_{n-2} &  a_{n-1}  &  b_{n-1} \\
            &           &           &  b_{n-1} &  a_n
    \end{bmatrix}.
$$
Hence, only one vector of size `n` and two of size `n-1` need to be stored to fully represent the matrix.
This particular structure also lends itself to specialized implementations for many linear algebra tasks.
Interfaces to the most common ones will soon be provided by `stdlib_specialmatrices`.
Symmetric tridiagonal matrices are available with all supported data types as `symtridiagonal_<kind>_type`, for example:

- `symtridiagonal_sp_type`   : Symmetric tridiagonal matrix of size `n` with `real`/`single precision` data.
- `symtridiagonal_dp_type`   : Symmetric tridiagonal matrix of size `n` with `real`/`double precision` data.
- `symtridiagonal_xdp_type`  : Symmetric tridiagonal matrix of size `n` with `real`/`extended precision` data.
- `symtridiagonal_qp_type`   : Symmetric tridiagonal matrix of size `n` with `real`/`quadruple precision` data.
- `symtridiagonal_csp_type`  : Symmetric tridiagonal matrix of size `n` with `complex`/`single precision` data.
- `symtridiagonal_cdp_type`  : Symmetric tridiagonal matrix of size `n` with `complex`/`double precision` data.
- `symtridiagonal_cxdp_type` : Symmetric tridiagonal matrix of size `n` with `complex`/`extended precision` data.
- `symtridiagonal_cqp_type`  : Symmetric tridiagonal matrix of size `n` with `complex`/`quadruple precision` data.

#### Syntax

- To construct a symmetric tridiagonal matrix from already allocated arrays `dv` (main diagonal, size `n`, only its real part is being referenced) and `ev` (upper diagonal, size `n-1`):

`A = ` [[stdlib_specialmatrices(module):symtridiagonal(interface)]] `(dv, ev)`

- To construct a symmetric tridiagonal matrix of size `n x n` with constant diagonal elements `dv`, and `ev`:

`A = ` [[stdlib_specialmatrices(module):symtridiagonal(interface)]] `(dv, ev, n)`

#### Example

```fortran
{!example/specialmatrices/example_symtridiagonal_dp_type.f90!}
```

### Hermitian tridiagonal matrices {#HermTridiagonal}

#### Status

Experimental

#### Description

Hermitian tridiagonal matrices are ubiquituous in scientific computing.
A generic hermitian tridiagonal matrix has the following structure:
$$
    A
    =
    \begin{bmatrix}
        a_1 &  b_1  \\
        \bar{b}_1 &  a_2      &  b_2  \\
            &  \ddots   &  \ddots   &  \ddots   \\
            &           &  \bar{b}_{n-2} &  a_{n-1}  &  b_{n-1} \\
            &           &           &  \bar{b}_{n-1} &  a_n
    \end{bmatrix},
$$
where \( a_i \in \mathbb{R} \), \( b_i \in \mathbb{C} \) and the overbar denotes the complex conjugate operation.
Hence, only one vector of size `n` and two of size `n-1` need to be stored to fully represent the matrix.
This particular structure also lends itself to specialized implementations for many linear algebra tasks.
Interfaces to the most common ones will soon be provided by `stdlib_specialmatrices`.
Hermitian tridiagonal matrices are available with all supported data types as `hermtridiagonal_<kind>_type`, for example:

- `hermtridiagonal_csp_type`  : Hermitian tridiagonal matrix of size `n` with `complex`/`single precision` data.
- `hermtridiagonal_cdp_type`  : Hermitian tridiagonal matrix of size `n` with `complex`/`double precision` data.
- `hermtridiagonal_cxdp_type` : Hermitian tridiagonal matrix of size `n` with `complex`/`extended precision` data.
- `hermtridiagonal_cqp_type`  : Hermitian tridiagonal matrix of size `n` with `complex`/`quadruple precision` data.

#### Syntax

- To construct a hermitian tridiagonal matrix from already allocated arrays `dv` (main diagonal, size `n`) and `ev` (upper diagonal, size `n-1`):

`A = ` [[stdlib_specialmatrices(module):hermtridiagonal(interface)]] `(dv, ev)`

- To construct a hermitian tridiagonal matrix of size `n x n` with constant diagonal elements `dv`, and `ev`:

`A = ` [[stdlib_specialmatrices(module):hermtridiagonal(interface)]] `(dv, ev, n)`

Note that only the real parts of the diagonal elements `dv` are being used to construct the corresponding Hermitian matrix.

#### Example

```fortran
{!example/specialmatrices/example_hermtridiagonal_cdp_type.f90!}
```

## Specialized drivers for linear algebra tasks

Below is a list of all the specialized drivers for linear algebra tasks currently provided by the `stdlib_specialmatrices` module.

### Matrix-vector products with `spmv` {#spmv}

#### Status

Experimental

#### Description

With the exception of `extended precision` and `quadruple precision`, all the types provided by `stdlib_specialmatrices` benefit from specialized kernels for matrix-vector products accessible via the common `spmv` interface.

- For `tridiagonal` matrices, the LAPACK `lagtm` backend is being used.

#### Syntax

`call ` [[stdlib_specialmatrices(module):spmv(interface)]] `(A, x, y [, alpha, beta, op])`

#### Arguments

- `A` : Shall be a matrix of one of the types provided by `stdlib_specialmatrices`. It is an `intent(in)` argument.

- `x` : Shall be a rank-1 or rank-2 array with the same kind as `A`. It is an `intent(in)` argument.

- `y` : Shall be a rank-1 or rank-2 array with the same kind as `A`. It is an `intent(inout)` argument.

- `alpha` (optional) : Scalar value of the same type as `x`. It is an `intent(in)` argument. By default, `alpha = 1`.

- `beta` (optional) : Scalar value of the same type as `y`. It is an `intent(in)` argument. By default `beta = 0`.

- `op` (optional) : In-place operator identifier. Shall be a character(1) argument. It can have any of the following values: `N`: no transpose, `T`: transpose, `H`: hermitian or complex transpose.

@warning
Due to limitations of the underlying `lapack` driver, currently `alpha` and `beta` can only take one of the values `[-1, 0, 1]` for `tridiagonal`, `symtridiagonal` and `hermtridiagonal` matrices. See `lagtm` for more details.
@endwarning

#### Examples

```fortran
{!example/specialmatrices/example_specialmatrices_dp_spmv.f90!}
```

## Utility functions

### `dense` : converting a special matrix to a standard Fortran array {#dense}

#### Status

Experimental

#### Description

Utility function to convert all the matrix types provided by `stdlib_specialmatrices` to a standard rank-2 array of the appropriate kind.

#### Syntax

`B = ` [[stdlib_specialmatrices(module):dense(interface)]] `(A)`

#### Arguments

- `A` : Shall be a matrix of one of the types provided by `stdlib_specialmatrices`. It is an `intent(in)` argument.

- `B` : Shall be a rank-2 allocatable array of the appropriate `real` or `complex` kind.

### `transpose` : Transposition of a special matrix {#transpose}

#### Status

Experimental

#### Description

Utility function returning the transpose of a special matrix. The returned matrix is of the same type and kind as the input one.

#### Syntax

`B = ` [[stdlib_specialmatrices(module):transpose(interface)]] `(A)`

#### Arguments

- `A` : Shall be a matrix of one of the types provided by `stdlib_specialmatrices`. It is an `intent(in)` argument.

- `B` : Shall be a matrix of one of the same type and kind as `A`.

### `hermitian` : Complex-conjugate transpose of a special matrix {#hermitian}

#### Status

Experimental

#### Description

Utility function returning the complex-conjugate transpose of a special matrix. The returned matrix is of the same type and kind as the input one. For real-valued matrices, `hermitian` is equivalent to `transpose`.

#### Syntax

`B = ` [[stdlib_specialmatrices(module):hermitian(interface)]] `(A)`

#### Arguments

- `A` : Shall be a matrix of one of the types provided by `stdlib_specialmatrices`. It is an `intent(in)` argument.

- `B` : Shall be a matrix of one of the same type and kind as `A`.

### Operator overloading (`+`, `-`, `*`) {#operators}

#### Status

Experimental

#### Description

The definition of all standard artihmetic operators have been overloaded to be applicable for the matrix types defined by `stdlib_specialmatrices`:

- Overloading the `+` operator for adding two matrices of the same class and kind.
- Overloading the `-` operator for subtracting two matrices of the same class and kind.
- Overloading the `*` for scalar-matrix multiplication.

#### Syntax

- Adding two matrices of the same class:

`C = A` [[stdlib_specialmatrices(module):operator(+)(interface)]] `B`

- Subtracting two matrices of the same class:

`C = A` [[stdlib_specialmatrices(module):operator(-)(interface)]] `B`

- Scalar multiplication

`B = alpha` [[stdlib_specialmatrices(module):operator(*)(interface)]] `A`

@note
For addition (`+`) and subtraction (`-`), matrices `A`, and `B` need to be of the same class and kind. For scalar multiplication (`*`), `A` and `B` need to be of the same class and kind, while `alpha` is either `real` or `complex` (with the same kind again) depending on the type being used.
@endnote
