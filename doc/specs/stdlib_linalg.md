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

## `zeros/ones`

### Description

`zeros` creates a rank-1 or rank-2 `array` of the given shape, filled completely with `0` `integer` type values.  
`ones` creates a rank-1 or rank-2 `array` of the given shape, filled completely with `1` `integer` type values.

### Status

Experimental

### Class

Pure function.

### Syntax

For rank-1 array:  
`result = [[stdlib_linalg(module):zeros(interface)]](dim)`  
`result = [[stdlib_linalg(module):ones(interface)]](dim)`

For rank-2 array:  
`result = [[stdlib_linalg(module):zeros(interface)]](dim1, dim2)`  
`result = [[stdlib_linalg(module):ones(interface)]](dim1, dim2)`


### Arguments

`dim/dim1`: Shall be an `integer` type.
This is an `intent(in)` argument.

`dim2`: Shall be an `integer` type.
This is an `intent(in)` argument.

### Return value

Returns a rank-1 or rank-2 `array` of the given shape, filled completely with either `0` or `1` `integer` type values.

#### Warning

Since the result of `ones` is of `integer` type, one should be careful about using it in arithmetic expressions. For example:
```fortran
real :: A(:,:)

!> Be careful
A = ones(2,2)/2     !! A = 1/2 = 0.0

!> Recommend
A = ones(2,2)/2.0   !! A = 1/2.0 = 0.5
```

### Example

```fortran
program demo
    use stdlib_linalg, only: zeros, ones
    implicit none
    real, allocatable :: A(:,:)
    integer :: iA(2)
    complex :: cA(2), cB(2,3)
    
    A = zeros(2,2)          !! [0.0,0.0; 0.0,0.0] (Same as `reshape(spread(0,1,2*2),[2,2])`)
    A = ones(4,4)           !! [1.0,1.0,1.0,1.0; 1.0,1.0,1.0,1.0; 1.0,1.0,1.0,1.0; 1.0,1.0,1.0 1.0]
    A = 2.0*ones(2,2)       !! [2.0,2.0; 2.0,2.0]

    print *, reshape(ones(2*3*4),[2,3,4])    !! Same as `reshape(spread(1,1,2*3*4),[2,3,4])`

    iA = ones(2)            !! [1,1] (Same as `spread(1,1,2)`)
    cA = ones(2)            !! [(1.0,0.0),(1.0,0.0)]
    cA = (1.0,1.0)*ones(2)  !! [(1.0,1.0),(1.0,1.0)]
    cB = ones(2,3)          !! [(1.0,0.0),(1.0,0.0),(1.0,0.0); (1.0,0.0),(1.0,0.0),(1.0,0.0)]

end program demo
```
