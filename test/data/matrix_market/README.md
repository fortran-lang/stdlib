# Test Data: Matrix Market

This directory contains matrices used for testing purposes. These matrices are obtained from the SuiteSparse Matrix Collection (formerly the University of Florida Sparse Matrix Collection):

https://sparse.tamu.edu

## License

The matrices in this directory are distributed under the CC-BY 4.0 license:

https://creativecommons.org/licenses/by/4.0/

## Attribution

- Kolodziej et al., (2019). The SuiteSparse Matrix Collection Website Interface. Journal of Open Source Software, 4(35), 1244. DOI: https://doi.org/10.21105/joss.01244

- Timothy A. Davis and Yifan Hu. 2011. The University of Florida Sparse Matrix Collection. ACM Transactions on Mathematical Software 38, 1, Article 1 (December 2011), 25 pages. DOI: https://doi.org/10.1145/2049662.2049663

## Matrix Market Metadata

The matrices stored in the Matrix Market (`.mtx`) files include metadata in their headers, which may contain additional citations specific to individual matrices.

These headers have been preserved and must not be removed.

## Matrix Sources

The matrix market files used are listed below:
- https://sparse.tamu.edu/HB/ash85
- https://sparse.tamu.edu/HB/bcsstk01
- https://sparse.tamu.edu/Mallya/lhr01

## About .npy files

The `.npy` files in this directory are derived from the corresponding Matrix Market (`.mtx`) files using SciPy and NumPy. 

### Storage format

For coordinate (COO) matrices:

- Non-pattern matrices:
  - `*_data.npy` contains the nonzero values
  - `*_indices.npy` contains the indices in a flattened format:
    [row_1, row_2, ..., row_n, col_1, col_2, ..., col_n]

- Pattern matrices:
  - `*_indices.npy` contains the indices in the same flattened format

For array-type matrices:
- `*_data.npy` contains the matrix entries

### Generation of .npy files

The following Python snippet was used to generate the `.npy` files:

```python
import numpy as np
from scipy.io import mmread

FILE_NAME = "mm_file_name.mtx"

loaded = mmread(FILE_NAME)
indices = np.concatenate((loaded.row, loaded.col)) # not applicable for array-type matrices

np.save(FILE_NAME + "_indices.npy", indices) # not applicable for array-type matrices
np.save(FILE_NAME + "_data.npy", loaded.data) # not applicable for pattern matrices
```

## Notes

- The matrices included are solely for testing purposes.
- The matrices in the Matrix Market (`.mtx`) files are included without modification from their original source.
- The stdlib project itself remains licensed under the MIT License.
- The CC-BY 4.0 license applies only to the Matrix Market (`.mtx`) files in this directory.
- No warranties are provided by the original authors.

© The original authors of the Matrix Market (`.mtx`) files, as listed in the SuiteSparse Matrix Collection.