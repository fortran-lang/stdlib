# `stdlib_stats_decomposition`

```{contents}
:local:
:depth: 2
```

## `pca` - Principal Component Analysis

### Status

Experimental

### Description

Performs Principal Component Analysis (PCA) on a 2D array of observations and features.
The input matrix `x` has shape `(m, n)`, where `m` is the number of observations and `n` is the number of features.
The subroutine computes the principal components (loadings), the singular values, and optionally the feature means.

Two methods are supported:
- `"svd"`: (Default) Computes PCA via Singular Value Decomposition of the centered data. This is generally more numerically stable.
- `"eig"` or `"cov"`: Computes PCA via Eigendecomposition of the covariance matrix.

### Syntax

`call ` {f:subr}`pca` `(x, components, singular_values [, x_mean, method, overwrite_x, err])`

### Class

Generic subroutine

### Arguments

`x`: Shall be a rank-2 real array with shape `(m, n)`. It is an `intent(inout)` argument. If `overwrite_x` is `.true.`, `x` may be modified during computation.

`components`: Shall be a rank-2 real array with shape `(n_components, n)`. It stores the principal components as rows. It is an `intent(out)` argument.

`singular_values`: Shall be a rank-1 real array with shape `(n_components)`. It stores the singular values in descending order. It is an `intent(out)` argument.

`x_mean` (optional): Shall be a rank-1 real array with shape `(n)`. It stores the mean of each feature (column). It is an `intent(out)` argument.

`method` (optional): Shall be a character string. Either `"svd"` or `"eig"`/`"cov"`. It is an `intent(in)` argument.

`overwrite_x` (optional): Shall be a scalar of type `logical`. If `.true.`, the input matrix `x` can be used as a workspace and modified. It is an `intent(in)` argument.

`err` (optional): Shall be of type `linalg_state_type`. It is an `intent(out)` argument.

### Example

```{literalinclude} ../../example/stats/example_pca.f90
:language: fortran
```

## `pca_transform` - Projects data into principal component space

### Status

Experimental

### Description

Projects the input data `x` into the reduced dimensional space defined by the provided principal components.
The transformation is defined as `x_transformed = (x - x_mean) * components^T`.

### Syntax

`call ` {f:subr}`pca_transform` `(x, components, x_transformed [, x_mean])`

### Class

Generic subroutine

### Arguments

`x`: Shall be a rank-2 real array with shape `(m, n)`. It is an `intent(in)` argument.

`components`: Shall be a rank-2 real array with shape `(nc, n)`. It stores the principal components as rows. It is an `intent(in)` argument.

`x_transformed`: Shall be a rank-2 real array with shape `(m, nc)`. It stores the projected data. It is an `intent(out)` argument.

`x_mean` (optional): Shall be a rank-1 real array with shape `(n)`. It stores the feature means to subtract. It is an `intent(in)` argument.

## `pca_inverse_transform` - Reconstructs original data from principal component space

### Status

Experimental

### Description

Reconstructs the original data representation from the principal component space.
The reconstruction is defined as `x_reconstructed = x_reduced * components + x_mean`.

### Syntax

`call ` {f:subr}`pca_inverse_transform` `(x_reduced, components, x_reconstructed [, x_mean])`

### Class

Generic subroutine

### Arguments

`x_reduced`: Shall be a rank-2 real array with shape `(m, nc)`. It is an `intent(in)` argument.

`components`: Shall be a rank-2 real array with shape `(nc, n)`. It stores the principal components as rows. It is an `intent(in)` argument.

`x_reconstructed`: Shall be a rank-2 real array with shape `(m, n)`. It stores the reconstructed data. It is an `intent(out)` argument.

`x_mean` (optional): Shall be a rank-1 real array with shape `(n)`. It stores the feature means to add back. It is an `intent(in)` argument.
