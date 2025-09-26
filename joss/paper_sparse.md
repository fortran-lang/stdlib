---
title: 'stdlib sparse: A high level Fortran sparse matrix library'
tags:
  - Fortran
  - linear algebra
  - sparse matrix
authors:
  - name: José Alves
    corresponding: true
    orcid: 0000-0001-9448-0145
    affiliation: 1
  - name: Ivan Pribec
    affiliation: 2
    orcid: 0000-0001-8436-882X
  - name: Jeremie Vandenplas
    affiliation: 3
    orcid: 0000-0002-2554-072X
  - name: Federico Perini
    affiliation: 4
    orcid: 0000-0001-8017-1747
affiliations:
 - name: Transvalor S.A., France
   index: 1
 - name: Leibniz Centre of Supercomputing, Germany
   index: 2
 - name: Wageningen University and Research, The Netherlands
   index: 3
 - name: Wisconsin Engine Research Consultants, USA
   index: 4
date: 17 September 2025
bibliography: paper_sparse.bib
---

# Summary

Sparse matrices are a core building block in scientific computing, particularly in fields such as computational physics, engineering, and graph analysis. Despite Fortran’s long tradition in numerical computing, its ecosystem lacks a canonical, modern, high-level library for sparse data structures.

We present stdlib_sparse, a sparse matrix library implemented in modern Fortran as part of the (community) Fortran Standard Library (stdlib). It provides well-defined sparse storage formats, conversion routines, and core operations such as sparse matrix–vector multiplication. This library aims to improve reproducibility, interoperability, and performance across Fortran applications by offering standardized, extensible data structures.

# Statement of need

Many scientific applications require sparse linear algebra routines for efficient storage and computation with large, structured matrices. Fortran users have traditionally relied on external libraries (e.g. SPARSKIT, MUMPS, SuiteSparse) or custom implementations. This fragmentation leads to challenges in portability, maintainability, and discoverability.

The stdlib_sparse library addresses this gap by offering:

* A consistent set of sparse matrix formats (COO, CSR, CSC, ELLPACK, SELL-C).
* Format conversion routines to enable interoperability.
* Standardized operations such as sparse matrix–vector multiplication (SpMV).
* A unified API, following modern Fortran practices, as part of the official Fortran stdlib project.

By integrating directly into stdlib, stdlib_sparse lowers the barrier for Fortran developers to adopt sparse methods, reduces code duplication, and promotes best practices for numerical software development.

# Related work

Several sparse libraries exist in Fortran and other languages:

* SPARSKIT (Fortran 77, Saad 1994) — influential, but outdated syntax and limited interoperability [@saad2003iterative].
* MUMPS [@MUMPS:1] and PETSc [@petsc-web-page] — high-performance solvers written in Fortran/C with broad functionality, but heavy dependencies and steeper learning curve.
* SciPy.sparse (Python) [@2020SciPy-NMeth] and Eigen (C++) — modern high-level APIs in other ecosystems, demonstrating the value of standardized interfaces.

Compared to these, stdlib_sparse focuses on providing a lightweight, modern Fortran interface integrated into the stdlib, emphasizing portability and extensibility rather than complete solver functionality.

# Design and implementation
## Data structures

All sparse formats extend an abstract base type sparse_type, which holds metadata such as number of rows, columns, and nonzeros. Implementations include:

* COO: coordinate (triplet) format.
* CSR: compressed sparse row (Yale) format.
* CSC: compressed sparse column format.
* ELLPACK: fixed number of nonzeros per row, suited for vectorization.
* SELL-C: sliced ELLPACK, balancing CSR and ELLPACK trade-offs [@anzt2014implementing].

## Core functionality

* Construction: from triplet (`i,j,v`) arrays or dense matrices.
* Data accessors: `add` (insert/update entries) and `at` (element access with management zero/NaN handling of missing entries).
* Operations: sparse matrix–vector multiplication (`spmv`), with optional transpose and hermitian variants:
$$ y = \alpha op(A) * x + \beta * y$$
* Conversions: between sparse formats and dense matrices.
* Utilities: diagonal extraction, symmetry flags, duplicate entry handling.

## Implementation details

Before introducing stdlib_sparse, the core structure and API was crafted under a stand-aline project, FSPARSE [@fsparse2024]. This enabled testing and refinement of the library before integration into stdlib.

The module is designed with the following key features:

* Generic procedures support both real and complex kinds.
* Memory is allocated dynamically with type-bound malloc routines.
* Conversions handle duplicate entries, with options for summation or overwriting.

# Example usage

```fortran
program main
    use stdlib_linalg_constants, only: dp
    use stdlib_sparse
    implicit none

    integer, parameter :: m = 4, n = 2
    real(dp) :: A(m,n), x(n)
    real(dp) :: y_dense(m), y_coo(m), y_csr(m)
    real(dp) :: alpha, beta
    type(COO_dp_type) :: COO
    type(CSR_dp_type) :: CSR

    call random_number(A)
    ! Convert from dense to COO and CSR matrices
    call dense2coo( A , COO )
    call coo2csr( COO , CSR )

    ! Initialize vectors
    x       = 1._dp
    y_dense = 2._dp
    y_coo   = y_dense
    y_csr   = y_dense

    ! Perform matrix-vector product
    alpha = 3._dp; beta = 2._dp
    y_dense = alpha * matmul(A,x) + beta * y_dense
    call spmv( COO , x , y_coo , alpha = alpha, beta = beta )
    call spmv( CSR , x , y_csr , alpha = alpha, beta = beta )

    print *, 'dense :', y_dense
    print *, 'coo   :', y_coo
    print *, 'csr   :', y_csr

end program main
```

# Performance and limitations

Sparse matrix–vector multiplication has been implemented for all formats. Preliminary tests confirm correctness and scalability to moderately large problems. However:

* No sparse matrix–matrix multiplication or factorizations are yet implemented.
* For data-parallelism (multi-processing with MPI or coarrays) the `spmv` kernel can be used as basis within each process. Multi-threading or GPU acceleration is not currently supported.
* Interfaces are subject to change while the module remains experimental.

Future work will address these limitations by adding additional kernels, improving performance portability, and expanding supported formats.

# Acknowledgements

This work is part of the Fortran-lang community project. We thank the contributors to stdlib and the Fortran community for discussions, reviews, and development efforts.

# References