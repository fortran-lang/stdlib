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
The Compressed Sparse Colum `CSC` is similar to the `CSR` format but values are accessed first by column, thus an index counter is given by `colptr` which enables to know the first and last non-zero row index of a given colum.

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

* Add single value
`call matrix%add(i,j,v)` 

* Add a block of values
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
## `spmm` - Sparse Matrix-Matrix product

### Status

Experimental

### Description

Multiply sparse/dense factors, or two sparse factors of the same storage format
and numeric kind. COO, CSR, CSC, ELL and SELLC are supported for the configured
real and complex kinds. Sparse results remain in that same format.

Dense results compute `C = alpha * op(S) * D + beta * C` or
`C = alpha * D * op(S) + beta * C`; their operation and storage behavior follows
`spmv`. Sparse results compute `C = alpha * op(A) * op(B)` using `sparse_full`
storage. Both factors can independently be used normally, transposed, or
conjugate-transposed.

### Syntax

`call ` [[stdlib_sparse_spmm(module):spmm(interface)]] `(sparse,dense,result [,alpha,beta,op])`

`call ` [[stdlib_sparse_spmm(module):spmm(interface)]] `(dense,sparse,result [,alpha,beta,op])`

`call ` [[stdlib_sparse_spmm(module):spmm(interface)]] `(a,b,c [,alpha,op_a,op_b,allow_resize,work,stat])`

### Arguments

`a`, `b`: Same-format, same-kind sparse `intent(in)` factors. Their storage must
be valid and use one-based indices with `sparse_full`. The inner dimensions of
`op(a)` and `op(b)` must agree. Stored zeros and duplicate input coordinates
participate in the product structure.

`c`: Same-format, same-kind sparse `intent(inout)` result, with the shape of
`op(a)*op(b)`. Its values are overwritten. It must not alias either input.

`alpha`, optional: Scalar product scale of the factors' numeric type, default 1.
With zero `alpha`, the result values are cleared without reading input values.

`op_a`, `op_b`, optional: Independent operation characters, default `N`.
`N` uses the factor normally, `T` transposes it, and `H` conjugate-transposes it.
For real data `H` is equivalent to `T`. The high-level procedures also accept
lowercase characters. These arguments apply to sparse-by-sparse products;
dense-result overloads retain the single `op` argument and optional `beta`.

`allow_resize`, optional: Logical `intent(in)`, default true. True calls
`spmm_prepare` before numerical multiplication. False skips preparation and
updates the existing result values without reallocating its data or index
buffers. There is no persistent plan object.

When preparation is skipped, **the caller must ensure that C already contains
every position required by the structural product**. A C constructed by
`spmm_prepare` is suitable while the input structures and operations remain
compatible. A valid precomputed superset of the product pattern is also allowed;
extra stored result positions receive zero. COO results must have unique,
row-ordered coordinates (`is_sorted` true); other result formats require unique
stored positions apart from their normal padding. Changes to input values alone
do not require preparation. After changing indices or operations, prepare again
unless the existing result pattern is known to remain sufficient.

`work`, optional: Contiguous `integer(ilp)` `intent(inout)` scratch array. Its
size must be at least the number of rows of C for CSC, or columns of C for the
other formats. Its contents are unspecified on return. If omitted, the wrapper
allocates this scratch array locally. Each concurrent call requires its own
workspace and result. Non-transposed CSR, CSC, ELL and SELLC calls with supplied
workspace perform no heap allocation. COO and transposed products construct
transient integer views of input indices, so they may allocate workspace even
when `work` is supplied. No input numerical values are copied into those views.

`stat`, optional: Integer `intent(out)` error status: `spmm_success` (0) or
`spmm_invalid_input` (1). The wrapper checks operation characters, high-level
dimensions, full-storage flags, basic buffer extents and workspace length.
It does not compare structural snapshots or scan all index values on each call.
Invalid sparse storage or an insufficient result pattern violates the calling
contract and is not diagnosed by these checks. Without `stat`, detected errors
terminate through `stdlib_error:error_stop`.

## `spmm_prepare` - Prepare a sparse product structure

### Status

Experimental

### Syntax

`call ` [[stdlib_sparse_spmm(module):spmm_prepare(interface)]] `(a,b,c [,op_a,op_b,stat])`

### Description

Build the structural product of `op(a)` and `op(b)` and allocate C, independently
of numerical values. The result values are initially zero. Stored zeros and
numerical cancellation do not remove positions. No floating-point products are
evaluated, and no plan or input/output structure snapshots are retained.

The factors, result, operations and status have the meanings given above.
Detected shape/storage errors leave C unchanged. CSR and CSC results have sorted
indices; COO results are row-ordered with unique coordinates. ELL and SELLC add
padding when necessary. SELLC retains the left factor's chunk size, and input
padding with a valid column index is conservatively included in the structure.

## `spmm_kernel` - Non-object-oriented sparse matrix-matrix kernels

### Status

Experimental

### Syntax

The five public array interfaces share `op_a,op_b,alpha,shape_a,shape_b` as their
first five arguments. `shape_a` and `shape_b` are `integer(ilp)` arrays of length
2 containing the original factors' `[nrows,ncols]`, before applying operations.
Their remaining arguments are:

| Interface | Remaining arguments |
|---|---|
| [[stdlib_sparse_spmm(module):spmm_kernel_coo(interface)]] | `a_data,a_index,b_data,b_index,c_data,c_index,work` |
| [[stdlib_sparse_spmm(module):spmm_kernel_csr(interface)]] | `a_data,a_rowptr,a_col,b_data,b_rowptr,b_col,c_data,c_rowptr,c_col,work` |
| [[stdlib_sparse_spmm(module):spmm_kernel_csc(interface)]] | `a_data,a_colptr,a_row,b_data,b_colptr,b_row,c_data,c_colptr,c_row,work` |
| [[stdlib_sparse_spmm(module):spmm_kernel_ell(interface)]] | `a_data,a_index,b_data,b_index,c_data,c_index,work` |
| [[stdlib_sparse_spmm(module):spmm_kernel_sellc(interface)]] | `a_data,a_rowptr,a_col,b_data,b_rowptr,b_col,c_data,c_rowptr,c_col,work` |

### Description

Apply numerical multiplication directly to conforming arrays. The kernels
accept no matrix objects or plans, preserve all result indices, and overwrite
only `c_data` and the caller's integer workspace. All arguments are required.
Input and result data have one numeric type/kind, and all array arguments are
contiguous. Use uppercase `N`, `T` or `H` for the operations.

Data arrays are rank 1 for COO/CSR/CSC and rank 2 for ELL/SELLC. Index layouts
match the existing sparse matrix types. Pass only the used prefix for COO/CSR/CSC
when backing arrays have extra capacity. SELLC chunk sizes are inferred from
the first dimension of each column-index array, including C's own chunk size.
`c_data` has `intent(inout)`; input data and every index array have `intent(in)`;
`work` has `intent(inout)`. C and workspace must not overlap input arrays.

The caller guarantees valid one-based storage, conforming shapes, enough
workspace, and the complete result pattern described above. The raw kernels do
not perform those checks. Preparation and result allocation occur outside the
kernels. COO grouping and transpose handling may allocate temporary integer
views, as described for `work`; the non-transposed CSR/CSC/ELL/SELLC paths operate
directly on the supplied storage.

### Examples

{!example/linalg/example_sparse_spmm.f90!}

The complex example also prepares and reuses a sparse conjugate-transpose product:

{!example/linalg/example_sparse_spmm_complex.f90!}

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
## `spmv_kernel` - Non-object-oriented sparse matrix-vector product

### Status

Experimental

### Description

Provide non-object-oriented sparse matrix-vector product kernels for the current supported sparse matrix types.

$$y=\alpha*op(M)*x+\beta*y$$

### Syntax

`call ` [[stdlib_sparse_spmv(module):spmv_kernel_coo(interface)]] `(op,alpha,data,index,storage,vec_x,beta,vec_y)`
`call ` [[stdlib_sparse_spmv(module):spmv_kernel_csc(interface)]] `(op,alpha,data,colptr,row,storage,vec_x,beta,vec_y)`
`call ` [[stdlib_sparse_spmv(module):spmv_kernel_csr(interface)]] `(op,alpha,data,col,rowptr,storage,vec_x,beta,vec_y)`
`call ` [[stdlib_sparse_spmv(module):spmv_kernel_ell(interface)]] `(op,alpha,data,index,storage,vec_x,beta,vec_y)`
`call ` [[stdlib_sparse_spmv(module):spmv_kernel_sellc(interface)]] `(op,alpha,data,ia,ja,storage,vec_x,beta,vec_y)`

### Arguments

Common arguments for all formats

`op`: In-place operator identifier. Shall be a `character(1)` argument. It can have any of the following values: `N`: no transpose, `T`: transpose, `H`: hermitian or complex transpose. These values are provided as constants by the `stdlib_sparse` module: `sparse_op_none`, `sparse_op_transpose`, `sparse_op_hermitian`

`alpha`: Shall be a scalar value of the same type as `vec_x`. Default value `alpha=1`. It is an `intent(in)` argument.

`storage`: Shall be a scalar of `integer` type. It is an `intent(in)` argument. It defines the symmetry storage of the sparse matrix and must be one of `sparse_full`, `sparse_lower`, or `sparse_upper`.

`vec_x`: Shall be a rank-1 or rank-2 array of `real` or `complex` type array. It is an `intent(in)` argument.

`beta`: Shall be a scalar value of the same type as `vec_x`. Default value `beta=0`. It is an `intent(in)` argument.

`vec_y`: Shall be a rank-1 or rank-2 array of `real` or `complex` type array. . It is an `intent(inout)` argument.

For the `COO` format:

`data`: Shall be a rank-1 array of `real` or `complex` type. It is an `intent(in)` argument.

`index`: Shall be a rank-2 array of `integer(ilp)` type. It is an `intent(in)` argument.

For the `CSC` format:

`data`: Shall be a rank-1 array of `real` or `complex` type. It is an `intent(in)` argument.

`colptr`: Shall be a rank-1 array of `integer(ilp)` type. It is an `intent(in)` argument.

`row`: Shall be a rank-1 array of `integer(ilp)` type. It is an `intent(in)` argument.


For the `CSR` format:

`data`: Shall be a rank-1 array of `real` or `complex` type array. It is an `intent(in)` argument.

`col`: Shall be a rank-1 array of `integer(ilp)` type. It is an `intent(in)` argument.

`rowptr`: Shall be a rank-1 array of `integer(ilp)` type. It is an `intent(in)` argument.

For the `ELL` format:

`data`: Shall be a rank-2 array of `real` or `complex` type. It is an `intent(in)` argument.

`index`: Shall be a rank-2 array of `integer(ilp)` type. It is an `intent(in)` argument.

For the `SELLC` format:

`data`: Shall be a rank-2 array of `real` or `complex` type array. It is an `intent(in)` argument.

`ia`: Shall be a rank-1 array of `integer(ilp)` type. It is an `intent(in)` argument.

`ja`: Shall be a rank-2 array of `integer(ilp)` type. It is an `intent(in)` argument.


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

`call ` [[stdlib_sparse_conversion(module):csc2dense(interface)]] `(csc,dense)`

### Arguments

`csc` : Shall be a `CSC` type of `real` or `complex` type. It is an `intent(in)` argument.

`dense` : Shall be a rank-2 array of `real` or `complex` type. It is an `intent(out)` argument.

### Syntax

`call ` [[stdlib_sparse_conversion(module):coo2csr(interface)]] `(coo,csr[,sort_data])`

### Arguments

`coo` : Shall be a `COO` type of `real` or `complex` type. It is an `intent(in)` argument.

`csr` : Shall be a `CSR` type of `real` or `complex` type. It is an `intent(out)` argument.

`sort_data`, `optional` : Shall be a `logical` argument to determine whether data in the COO graph should be sorted before obtaining the CSR representation. The transformation from COO to CSR depends on the former being sorted in row-major order and not having duplicate pairs. Using this boolean will call a sorting routine at the cost of extra runtime, default `.false.`. It is an `intent(in)` argument.

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

`call ` [[stdlib_sparse_conversion(module):csr2ell(interface)]] `(csr,ell[,num_nz_rows])`

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

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
## Operator overloading (`+`, `-`, `*`, `/`) {#operators}

### Status

Experimental

### Description

The definition of all standard arithmetic operators have been overloaded to be applicable for the matrix types defined by `stdlib_sparse`. The operators have been overloaded to support the following tuple combinations of the left-hand-side of the operation: same type and kind matrix-matrix, matrix-scalar and scalar-matrix.

### Syntax

- Matrix-matrix operators :

`C = A + B`

`C = A - B`

`C = A * B`

`C = A / B`

- Matrix scalar operators : 

`B = A + alpha` or `B = alpha + A` 

`B = A - alpha` or `B = alpha - A`

`B = A * alpha` or `B = alpha * A`

`B = A / alpha` or `B = alpha / A`

*Note*: scalar addition and subtraction operators perform element-wise operations only on the stored (non-zero) values, not on the full mathematical matrix. Meaning, the sparsity pattern is preserved.
