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
real :: A(:,:)
!> Be careful
A = eye(2,2)/2     !! A == 0.0
!> Recommend
A = eye(2,2)/2.0   !! A == diag([0.5, 0.5])
```

### Example

```fortran
program demo_eye1
    use stdlib_linalg, only: eye
    implicit none
    integer :: i(2,2)
    real :: a(3,3)
    real :: b(2,3)  !! Matrix is non-square.
    complex :: c(2,2)
    I = eye(2)              !! [1,0; 0,1]
    A = eye(3)              !! [1.0,0.0,0.0; 0.0,1.0,0.0; 0.0,0.0,1.0]
    A = eye(3,3)            !! [1.0,0.0,0.0; 0.0,1.0,0.0; 0.0,0.0,1.0]
    B = eye(2,3)            !! [1.0,0.0,0.0; 0.0,1.0,0.0]
    C = eye(2,2)            !! [(1.0,0.0),(0.0,0.0); (0.0,0.0),(1.0,0.0)]
    C = (1.0,1.0)*eye(2,2)  !! [(1.0,1.0),(0.0,0.0); (0.0,0.0),(1.0,1.0)]
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

`result = [stdlib_linalg(module):trace(interface)](A)`

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
