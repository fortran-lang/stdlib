---
title: sparse
---

# The `stdlib_sparse` module

[TOC]

## Introduction

The `stdlib_sparse` module provides several derived types defining known sparse matrix data structures. It also provides basic sparse kernels such as sparse matrix vector and conversion between matrix types.

## Derived types provided

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `sparse_type` abstract derived type
#### Status

Experimental

#### Description
The `sparse_type` is defined as an abstract derived type holding the basic common meta data needed to define a sparse matrix. All other sparse types falvors are derived from the `sparse_type`.

```Fortran
type, public, abstract :: sparse_type
    integer :: nrows !> number of rows
    integer :: ncols !> number of columns
    integer :: nnz   !> number of non-zero values
    integer :: sym   !> assumed storage symmetry
    integer :: base  !> index base = 0 for (C) or 1 (Fortran)
end type
```

The symmetry integer laber should be assigned from the module's internal enumerator containing the following three enums:

```Fortran
enum, bind(C)
    enumerator :: sparse_full !> Full Sparse matrix (no symmetry considerations)
    enumerator :: sparse_lower  !> Symmetric Sparse matrix with triangular inferior storage
    enumerator :: sparse_upper  !> Symmetric Sparse matrix with triangular supperior storage
end enum
```
In the following, all sparse kinds will be presented in two main flavors: a data-less type `<matrix>_type` useful for topological graph operations. And real/complex valued types `<matrix>_<kind>` containing the `data` buffer for the matrix values.

$$ M = \begin{bmatrix} 
    9 & 0 & 0  & 0 & -3 \\
    4 & 7 & 0  & 0 & 0 \\
    0 & 8 & -1 & 8 & 0 \\
    4 & 0 & 5  & 6 & 0 \\
  \end{bmatrix} $$
<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `COO`: The COOrdinates compressed sparse format
#### Status

Experimental

#### Description
The `COO`, triplet or `ijv` format defines all non-zero elements of the matrix by explicitly allocating the `i,j` index and the value of the matrix. 

```Fortran
type(COO_sp) :: COO
call COO%malloc(4,5,10)
COO%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6])
COO%index(1:2,1)  = [1,1]
COO%index(1:2,2)  = [1,5]
COO%index(1:2,3)  = [2,1]
COO%index(1:2,4)  = [2,2]
COO%index(1:2,5)  = [3,2]
COO%index(1:2,6)  = [3,3]
COO%index(1:2,7)  = [3,4]
COO%index(1:2,8)  = [4,1]
COO%index(1:2,9)  = [4,3]
COO%index(1:2,10) = [4,4]
```
<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `CSR`: The Compressed Sparse Row or Yale format
#### Status

Experimental

#### Description
The Compressed Sparse Row or Yale format `CSR` stores the matrix index by compressing the row indeces with a counter pointer `rowptr` enabling to know the first and last non-zero colum index `col` of the given row. 

```Fortran
type(CSR_sp) :: CSR
call CSR%malloc(4,5,10)
CSR%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6])
CSR%col(:)    = [1,5,1,2,2,3,4,1,3,4]
CSR%rowptr(:) = [1,3,5,8,11]
```
<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `CSC`: The Compressed Sparse Column format
#### Status

Experimental

#### Description
The Compressed Sparse Colum `CSC` is similar to the `CSR` format but values are accesed first by colum, thus an index counter is given by `colptr` which enables accessing the start and ending rows of a given colum in the `row` index table. 

```Fortran
type(CSC_sp) :: CSC
call CSC%malloc(4,5,10)
CSC%data(:)   = real([9,4,4,7,8,-1,5,8,6,-3])
CSC%row(:)    = [1,2,4,2,3,3,4,3,4,1]
CSC%colptr(:) = [1,4,6,8,10,11]
```
<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `ELLPACK`: ELL-pack storage format
#### Status

Experimental

#### Description
The `ELL` format stores the data in a dense matrix of $nrows \times K$ in column major order. By imposing a constant number of zeros per row $K$, this format will incure in additional zeros being stored, but it enables efficient vectorization as memory acces are carried out by constant sized strides. 

```Fortran
type(ELL_sp) :: ELL
call ELL%malloc(num_rows=4,num_cols=5,num_nz_row=3)
ELL%data(1,1:3)   = real([9,-3,0])
ELL%data(2,1:3)   = real([4,7,0])
ELL%data(3,1:3)   = real([8,-1,8])
ELL%data(4,1:3)   = real([4,5,6])

ELL%index(1,1:3) = [1,5,0]
ELL%index(2,1:3) = [1,2,0]
ELL%index(3,1:3) = [2,3,4]
ELL%index(4,1:3) = [1,3,4]
```
<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `SELL-C`: The Sliced ELLPACK with Constant blocks format
#### Status

Experimental

#### Description
The Sliced ELLPACK format `SELLC` is a variation of the `ELLPACK` format. This modification reduces the storage size compared to the `ELLPACK` format but maintaining its efficient data access scheme. It can be seen as an intermediate format between `CSR` and `ELLPACK`. For more details read [here](https://arxiv.org/pdf/1307.6209v1)

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
## `spmv` - Sparse Matrix-Vector product

### Status

Experimental

### Description

Provide sparse matrix-vector product kernels for the current supported sparse matrix types.

$$y=\alpha*M*x+\beta*y$$

### Syntax

`call ` [[stdlib_sparse_spmv(module):spmv(interface)]] `(matrix,vec_x,vec_y [,alpha,beta])`

### Arguments

`matrix`, `intent(in)`: Shall be a `real` or `complex` sparse type matrix.

`vec_x`, `intent(in)`: Shall be a rank-1 or rank-2 array of `real` or `complex` type array.

`vec_y`, `intent(inout)`: Shall be a rank-1 or rank-2 array of `real` or `complex` type array.

`alpha`, `intent(in)`, `optional` : Shall be a scalar value of the same type as `vec_x`. Default value `alpha=1`.

`beta`, `intent(in)`, `optional` : Shall be a scalar value of the same type as `vec_x`. Default value `beta=0`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
## `sparse_conversion` - Sparse matrix to matrix conversions

### Status

Experimental

### Description

This module provides facility functions for converting between storage formats.

### Syntax

`call ` [[stdlib_sparse_conversion(module):dense2coo(interface)]] `(dense,coo)`

### Arguments

`dense`, `intent(in)`: Shall be a rank-2 array of `real` or `complex` type.

`coo`, `intent(inout)`: Shall be a `COO` type of `real` or `complex` type.

### Syntax

`call ` [[stdlib_sparse_conversion(module):coo2dense(interface)]] `(coo,dense)`

### Arguments

`coo`, `intent(in)`: Shall be a `COO` type of `real` or `complex` type.

`dense`, `intent(inout)`: Shall be a rank-2 array of `real` or `complex` type.

### Syntax

`call ` [[stdlib_sparse_conversion(module):coo2csr(interface)]] `(coo,csr)`

### Arguments

`coo`, `intent(in)`: Shall be a `COO` type of `real` or `complex` type.

`csr`, `intent(inout)`: Shall be a `CSR` type of `real` or `complex` type.

### Syntax

`call ` [[stdlib_sparse_conversion(module):csr2coo(interface)]] `(csr,coo)`

### Arguments

`csr`, `intent(in)`: Shall be a `CSR` type of `real` or `complex` type.

`coo`, `intent(inout)`: Shall be a `COO` type of `real` or `complex` type.

### Syntax

`call ` [[stdlib_sparse_conversion(module):csr2sellc(interface)]] `(csr,sellc[,chunk])`

### Arguments

`csr`, `intent(in)`: Shall be a `CSR` type of `real` or `complex` type.

`sellc`, `intent(inout)`: Shall be a `SELLC` type of `real` or `complex` type.

`chunk`, `intent(in)`, `optional`: chunk size for the Sliced ELLPACK format.