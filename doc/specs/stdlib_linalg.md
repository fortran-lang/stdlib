---
title: linalg
---

# Linear Algebra

[TOC]

## `diag` - Create a diagonal array or extract the diagonal elements of an array

### Status

Experimental

### Description

Create a diagonal array or extract the diagonal elements of an array

### Syntax

`d = [[stdlib_linalg(module):diag(interface)]](a [, k])`

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

`I = [[stdlib_linalg(module):eye(function)]](dim1 [, dim2])`

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

`result = [[stdlib_linalg(module):trace(interface)]](A)`

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

`d = [[stdlib_linalg(module):outer_product(interface)]](u, v)`

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

`C = [[stdlib_linalg(module):kronecker_product(interface)]](A, B)`

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

`c = [[stdlib_linalg(module):cross_product(interface)]](a, b)`

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

`d = [[stdlib_linalg(module):is_square(interface)]](A)`

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

`d = [[stdlib_linalg(module):is_diagonal(interface)]](A)`

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

`d = [[stdlib_linalg(module):is_symmetric(interface)]](A)`

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

`d = [[stdlib_linalg(module):is_skew_symmetric(interface)]](A)`

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

`d = [[stdlib_linalg(module):is_hermitian(interface)]](A)`

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

`d = [[stdlib_linalg(module):is_triangular(interface)]](A,uplo)`

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

`d = [[stdlib_linalg(module):is_hessenberg(interface)]](A,uplo)`

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
