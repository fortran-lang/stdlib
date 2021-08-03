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

## `empty` - Create a new vector or matrix of `integer/real/complex` type and given shape, without initializing values.

### Status

Experimental

### Class

Pure function.

### Description

`empty` creates a new vector or matrix of `integer/real/complex` type and given shape, without initializing values.

`empty`, unlike `zeros`, does not set the array values to zero, and may therefore be marginally faster. On the other hand, it requires the user to manually set all the values in the array, and should be used with caution.

### Syntax

For vector:
`result = [[stdlib_linalg(module):empty(interface)]](dim)`

For matrix:
`result = [[stdlib_linalg(module):empty(interface)]](dim1, dim2)`

### Arguments

`dim/dim1`: Shall be an `integer` scalar.
This is an `intent(in)` argument.

`dim2`: Shall be an `integer` scalar.
This is an `intent(in)` argument.

#### Note

Because of `huge(integer :: i) == 2147483647`, the dimensional maximum length of array created by the `empty` function is `2147483647`. 

### Return value

Returns a new `vector` or `matrix` of `integer` type and given shape, without initializing values.

#### Note
If the receiving `array` of the return value of the `empty` function is of a `real/complex` type, conversion from `integer` type to `real/complex` type will occur. 

### Example

```fortran
program demo_linlag_empty_1
    
    use stdlib_linlag, only: empty
    implicit none
    integer, allocatable :: i(:,:)

    print *, empty(2, 1)
    print *, 0.0*empty(2)                               !! 0.00000000       0.00000000
    print *, 0.0*empty(2) + 1.0*empty(2)
    print *, (0.1, 0.1)*empty(2) + (0.2, 0.2)*empty(2)

    i = empty(2,2)
    print *, i

end program demo_linalg_empty_1
```

```fortran
program demo_linlag_empty_2
    
    use stdlib_linlag, only: empty
    implicit none
    integer, allocatable :: i(:,:)
    real, allocatable :: r(:,:)
    complex, allocatable :: c(:,:)
    integer :: j(2)

    i = empty(2,1)
    r = empty(2,1)
    c = empty(2,1)
    j = empty(2)
    print *, i, r, c, j

end program demo_linalg_empty_2
```