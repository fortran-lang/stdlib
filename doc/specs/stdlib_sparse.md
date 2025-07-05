---
title: sparse
---

# The `stdlib_sparse` module

[TOC]

## Introduction

The `stdlib_sparse` module provides derived types for standard sparse matrix data structures. It also provides math kernels such as sparse matrix-vector product and conversion between matrix types.

## Sparse matrix derived types

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `sparse_type` abstract derived type
#### Status

Experimental

#### Description
The parent `sparse_type` is as an abstract derived type holding the basic common meta data needed to define a sparse matrix, as well as shared APIs. All sparse matrix flavors are extended from the `sparse_type`.

```Fortran
type, public, abstract :: sparse_type
    integer :: nrows   !! number of rows
    integer :: ncols   !! number of columns
    integer :: nnz     !! number of non-zero values
    integer :: storage !! assumed storage symmetry
end type
```

The storage integer label should be assigned from the module's internal enumerator containing the following three enums:

```Fortran
enum, bind(C)
    enumerator :: sparse_full  !! Full Sparse matrix (no symmetry considerations)
    enumerator :: sparse_lower !! Symmetric Sparse matrix with triangular inferior storage
    enumerator :: sparse_upper !! Symmetric Sparse matrix with triangular supperior storage
end enum
```
In the following, all sparse kinds will be presented in two main flavors: a data-less type `<matrix>_type` useful for topological graph operations. And real/complex valued types `<matrix>_<kind>_type` containing the `data` buffer for the matrix values. The following rectangular matrix will be used to showcase how each sparse matrix holds the data internally:

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
The `COO`, triplet or `ijv` format defines all non-zero elements of the matrix by explicitly allocating the `i,j` index and the value of the matrix. While some implementations use separate `row` and `col` arrays for the index, here we use a 2D array in order to promote fast memory acces to `ij`.

```Fortran
type(COO_sp_type) :: COO
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
The Compressed Sparse Row or Yale format `CSR` stores the matrix structure by compressing the row indices with a counter pointer `rowptr` enabling to know the first and last non-zero column index `col` of the given row. 

```Fortran
type(CSR_sp_type) :: CSR
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
The Compressed Sparse Colum `CSC` is similar to the `CSR` format but values are accesed first by column, thus an index counter is given by `colptr` which enables to know the first and last non-zero row index of a given colum. 

```Fortran
type(CSC_sp_type) :: CSC
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
The `ELL` format stores data in a dense matrix of $nrows \times K$ in column major order. By imposing a constant number of elements per row $K$, this format will incur in additional zeros being stored, but it enables efficient vectorization as memory acces is carried out by constant sized strides. 

```Fortran
type(ELL_sp_type) :: ELL
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
The Sliced ELLPACK format `SELLC` is a variation of the `ELLPACK` format. This modification reduces the storage size compared to the `ELLPACK` format but maintaining its efficient data access scheme. It can be seen as an intermediate format between `CSR` and `ELLPACK`. For more details read [the reference](https://arxiv.org/pdf/1307.6209v1)

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
## `add`- sparse matrix data accessors

### Status

Experimental

### Description
Type-bound procedures to enable adding data in a sparse matrix.

### Syntax

`call matrix%add(i,j,v)` or
`call matrix%add(i(:),j(:),v(:,:))`

### Arguments

`i`: Shall be an integer value or rank-1 array. It is an `intent(in)` argument.

`j`: Shall be an integer value or rank-1 array. It is an `intent(in)` argument.

`v`: Shall be a `real` or `complex` value or rank-2 array. The type shall be in accordance to the declared sparse matrix object. It is an `intent(in)` argument.

## `at`- sparse matrix data accessors

### Status

Experimental

### Description
Type-bound procedures to enable requesting data from a sparse matrix.

### Syntax

`v = matrix%at(i,j)`

### Arguments

`i` : Shall be an integer value. It is an `intent(in)` argument.

`j` : Shall be an integer value. It is an `intent(in)` argument.

`v` : Shall be a `real` or `complex` value in accordance to the declared sparse matrix object. If the `ij` tuple is within the sparse pattern, `v` contains the value in the data buffer. If the `ij` tuple is outside the sparse pattern, `v` is equal `0`. If the `ij` tuple is outside the matrix pattern `(nrows,ncols)`, `v` is `NaN`.

### Example
```fortran
{!example/linalg/example_sparse_data_accessors.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
## `spmv` - Sparse Matrix-Vector product

### Status

Experimental

### Description

Provide sparse matrix-vector product kernels for the current supported sparse matrix types.

$$y=\alpha*op(M)*x+\beta*y$$

### Syntax

`call ` [[stdlib_sparse_spmv(module):spmv(interface)]] `(matrix,vec_x,vec_y [,alpha,beta,op])`

### Arguments

`matrix`: Shall be a `real` or `complex` sparse type matrix. It is an `intent(in)` argument.

`vec_x`: Shall be a rank-1 or rank-2 array of `real` or `complex` type array. It is an `intent(in)` argument.

`vec_y`: Shall be a rank-1 or rank-2 array of `real` or `complex` type array. . It is an `intent(inout)` argument.

`alpha`, `optional` : Shall be a scalar value of the same type as `vec_x`. Default value `alpha=1`. It is an `intent(in)` argument.

`beta`, `optional` : Shall be a scalar value of the same type as `vec_x`. Default value `beta=0`. It is an `intent(in)` argument.

`op`, `optional`: In-place operator identifier. Shall be a `character(1)` argument. It can have any of the following values: `N`: no transpose, `T`: transpose, `H`: hermitian or complex transpose. These values are provided as constants by the `stdlib_sparse` module: `sparse_op_none`, `sparse_op_transpose`, `sparse_op_hermitian`

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
## Sparse matrix to matrix conversions

### Status

Experimental

### Description

This module provides facility functions for converting between storage formats.

### Syntax

`call ` [[stdlib_sparse_conversion(module):coo2ordered(interface)]] `(coo[,sort_data])`

### Arguments

`COO` : Shall be any `COO` type. The same object will be returned with the arrays reallocated to the correct size after removing duplicates. It is an `intent(inout)` argument.

`sort_data`, `optional` : Shall be a `logical` argument to determine whether data in the COO graph should be sorted while sorting the index array, default `.false.`. It is an `intent(in)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):from_ijv(interface)]] `(sparse,row,col[,data,nrows,ncols,num_nz_rows,chunk])`

### Arguments

`sparse` : Shall be a `COO`, `CSR`, `ELL` or `SELLC` type. The graph object will be returned with a canonical shape after sorting and removing duplicates from the `(row,col,data)` triplet. If the graph is `COO_type` no data buffer is allowed. It is an `intent(inout)` argument.

`row` : rows index array. It is an `intent(in)` argument.

`col` : columns index array. It is an `intent(in)` argument.

`data`, `optional`: `real` or `complex` data array. It is an `intent(in)` argument.

`nrows`, `optional`: number of rows, if not given it will be computed from the `row` array. It is an `intent(in)` argument.

`ncols`, `optional`: number of columns, if not given it will be computed from the `col` array. It is an `intent(in)` argument.

`num_nz_rows`, `optional`: number of non zeros per row, only valid in the case of an `ELL` matrix, by default it will computed from the largest row. It is an `intent(in)` argument.

`chunk`, `optional`: chunk size, only valid in the case of a `SELLC` matrix, by default it will be taken from the `SELLC` default attribute chunk size. It is an `intent(in)` argument.

### Example
```fortran
{!example/linalg/example_sparse_from_ijv.f90!}
```
### Syntax

`call ` [[stdlib_sparse_conversion(module):diag(interface)]] `(matrix,diagonal)`

### Arguments

`matrix` : Shall be a `dense`, `COO`, `CSR` or `ELL` type. It is an `intent(in)` argument.

`diagonal` : A rank-1 array of the same type as the `matrix`. It is an `intent(inout)` and `allocatable` argument.

#### Note
If the `diagonal` array has not been previously allocated, the `diag` subroutine will allocate it using the `nrows` of the `matrix`.

### Syntax

`call ` [[stdlib_sparse_conversion(module):dense2coo(interface)]] `(dense,coo)`

### Arguments

`dense` : Shall be a rank-2 array of `real` or `complex` type. It is an `intent(in)` argument.

`coo` : Shall be a `COO` type of `real` or `complex` type. It is an `intent(out)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):coo2dense(interface)]] `(coo,dense)`

### Arguments

`coo` : Shall be a `COO` type of `real` or `complex` type. It is an `intent(in)` argument.

`dense` : Shall be a rank-2 array of `real` or `complex` type. It is an `intent(out)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):coo2csr(interface)]] `(coo,csr)`

### Arguments

`coo` : Shall be a `COO` type of `real` or `complex` type. It is an `intent(in)` argument.

`csr` : Shall be a `CSR` type of `real` or `complex` type. It is an `intent(out)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):coo2csc(interface)]] `(coo,csc)`

### Arguments

`coo` : Shall be a `COO` type of `real` or `complex` type. It is an `intent(in)` argument.

`csc` : Shall be a `CSC` type of `real` or `complex` type. It is an `intent(out)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):csr2coo(interface)]] `(csr,coo)`

### Arguments

`csr` : Shall be a `CSR` type of `real` or `complex` type. It is an `intent(in)` argument.

`coo` : Shall be a `COO` type of `real` or `complex` type. It is an `intent(out)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):csr2sellc(interface)]] `(csr,sellc[,chunk])`

### Arguments

`csr` : Shall be a `CSR` type of `real` or `complex` type. It is an `intent(in)` argument.

`sellc` : Shall be a `SELLC` type of `real` or `complex` type. It is an `intent(out)` argument.

`chunk`, `optional`: chunk size for the Sliced ELLPACK format. It is an `intent(in)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):csr2sellc(interface)]] `(csr,ell[,num_nz_rows])`

### Arguments

`csr` : Shall be a `CSR` type of `real` or `complex` type. It is an `intent(in)` argument.

`ell` : Shall be a `ELL` type of `real` or `complex` type. It is an `intent(out)` argument.

`num_nz_rows`, `optional`: number of non zeros per row. If not give, it will correspond to the size of the longest row in the `CSR` matrix. It is an `intent(in)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):csc2coo(interface)]] `(csc,coo)`

### Arguments

`csc` : Shall be a `CSC` type of `real` or `complex` type. It is an `intent(in)` argument.

`coo` : Shall be a `COO` type of `real` or `complex` type. It is an `intent(out)` argument.

### Example
```fortran
{!example/linalg/example_sparse_spmv.f90!}
```