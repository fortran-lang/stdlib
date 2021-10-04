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
program demo_diag1
    use stdlib_linalg, only: diag
    implicit none
    real, allocatable :: A(:,:)
    integer :: i
    A = diag([(1,i=1,10)]) ! creates a 10 by 10 identity matrix
end program demo_diag1
```

```fortran
program demo_diag2
    use stdlib_linalg, only: diag
    implicit none
    real :: v(:)
    real, allocatable :: A(:,:)
    integer :: i
    v = [1,2,3,4,5]
    A = diag(v) ! creates a 5 by 5 matrix with elements of v on the diagonal
end program demo_diag2
```

```fortran
program demo_diag3
    use stdlib_linalg, only: diag
    implicit none
    integer, parameter :: n = 10
    real :: c(n), ul(n-1)
    real :: A(n,n)
    integer :: i
    c = 2
    ul = -1
    A = diag(ul,-1) + diag(c) + diag(ul,1) ! Gil Strang's favorite matrix
end program demo_diag3
```

```fortran
program demo_diag4
    use stdlib_linalg, only: diag
    implicit none
    integer, parameter :: n = 12
    real :: A(n,n)
    real :: v(n)
    integer :: i
    call random_number(A)
    v = diag(A) ! v contains diagonal elements of A
end program demo_diag4
```

```fortran
program demo_diag5
    use stdlib_linalg, only: diag
    implicit none
    integer, parameter :: n = 3
    real :: A(n,n)
    real, allocatable :: v(:)
    integer :: i
    A = reshape([1,2,3,4,5,6,7,8,9],[n,n])
    v = diag(A,-1) ! v is [2,6]
    v = diag(A,1)  ! v is [4,8]
end program demo_diag5
```

## `eye` - Construct the identity matrix

### Status

Experimental

### Description

Construct the identity matrix

### Syntax

`I = [[stdlib_linalg(module):eye(function)]](n)`

### Arguments

`n`: Shall be a scalar of default type `integer`. 

### Return value

Returns the identity matrix, i.e. a square matrix with ones on the main diagonal and zeros elsewhere. The return value is of type `integer(int8)`.

### Example

```fortran
program demo_eye1
    use stdlib_linalg, only: eye
    implicit none
    real :: a(3,3)
    A = eye(3)
end program demo_eye1
```

```fortran
program demo_eye2
    use stdlib_linalg, only: eye, diag
    implicit none
    print *, all(eye(4) == diag([1,1,1,1])) ! prints .true.
end program demo_eye2
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
program demo_trace
    use stdlib_linalg, only: trace
    implicit none
    real :: A(3,3)
    A = reshape([1,2,3,4,5,6,7,8,9],[3,3])
    print *, trace(A) ! 1 + 5 + 9
end program demo_trace
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
program demo_outer_product
    use stdlib_linalg, only: outer_product
    implicit none
    real, allocatable :: A(:,:), u(:), v(:)
    u = [1., 2., 3. ]
    v = [3., 4.]
    A = outer_product(u,v)
    !A = reshape([3., 6., 9., 4., 8., 12.], [3,2])
end program demo_outer_product
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
program demo_is_square
    use stdlib_linalg, only: is_square
    implicit none
    real :: A(2,2), B(3,2)
    logical :: res
    A = reshape([1., 2., 3., 4.], shape(A))
    B = reshape([1., 2., 3., 4., 5., 6.], shape(B))
    res = is_square(A) ! returns .true.
    res = is_square(B) ! returns .false.
end program demo_is_square
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
program demo_is_diagonal
    use stdlib_linalg, only: is_diagonal
    implicit none
    real :: A(2,2), B(2,2)
    logical :: res
    A = reshape([1., 0., 0., 4.], shape(A))
    B = reshape([1., 0., 3., 4.], shape(B))
    res = is_diagonal(A) ! returns .true.
    res = is_diagonal(B) ! returns .false.
end program demo_is_diagonal
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
program demo_is_symmetric
    use stdlib_linalg, only: is_symmetric
    implicit none
    real :: A(2,2), B(2,2)
    logical :: res
    A = reshape([1., 3., 3., 4.], shape(A))
    B = reshape([1., 0., 3., 4.], shape(B))
    res = is_symmetric(A) ! returns .true.
    res = is_symmetric(B) ! returns .false.
end program demo_is_symmetric
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
program demo_is_skew_symmetric
    use stdlib_linalg, only: is_skew_symmetric
    implicit none
    real :: A(2,2), B(2,2)
    logical :: res
    A = reshape([0., -3., 3., 0.], shape(A))
    B = reshape([0., 3., 3., 0.], shape(B))
    res = is_skew_symmetric(A) ! returns .true.
    res = is_skew_symmetric(B) ! returns .false.
end program demo_is_skew_symmetric
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
program demo_is_hermitian
    use stdlib_linalg, only: is_hermitian
    implicit none
    complex :: A(2,2), B(2,2)
    logical :: res
    A = reshape([cmplx(1.,0.), cmplx(3.,-1.), cmplx(3.,1.), cmplx(4.,0.)], shape(A))
    B = reshape([cmplx(1.,0.), cmplx(3.,1.), cmplx(3.,1.), cmplx(4.,0.)], shape(B))
    res = is_hermitian(A) ! returns .true.
    res = is_hermitian(B) ! returns .false.
end program demo_is_hermitian
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
program demo_is_triangular
    use stdlib_linalg, only: is_triangular
    implicit none
    real :: A(3,3), B(3,3)
    logical :: res
    A = reshape([1., 0., 0., 4., 5., 0., 7., 8., 9.], shape(A))
    B = reshape([1., 0., 3., 4., 5., 0., 7., 8., 9.], shape(B))
    res = is_triangular(A,'u') ! returns .true.
    res = is_triangular(B,'u') ! returns .false.
end program demo_is_triangular
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
program demo_is_hessenberg
    use stdlib_linalg, only: is_hessenberg
    implicit none
    real :: A(3,3), B(3,3)
    logical :: res
    A = reshape([1., 2., 0., 4., 5., 6., 7., 8., 9.], shape(A))
    B = reshape([1., 2., 3., 4., 5., 6., 7., 8., 9.], shape(B))
    res = is_hessenberg(A,'u') ! returns .true.
    res = is_hessenberg(B,'u') ! returns .false.
end program demo_is_hessenberg
```
