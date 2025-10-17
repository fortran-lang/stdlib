module stdlib_sparse_constants
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    use stdlib_constants
    implicit none
    public

    enum, bind(C)
        enumerator :: sparse_full  !! Full Sparse matrix (no symmetry considerations)
        enumerator :: sparse_lower !! Symmetric Sparse matrix with triangular inferior storage
        enumerator :: sparse_upper !! Symmetric Sparse matrix with triangular supperior storage
    end enum

    character(1), parameter :: sparse_op_none = 'N' !! no transpose
    character(1), parameter :: sparse_op_transpose = 'T' !! transpose
    character(1), parameter :: sparse_op_hermitian = 'H' !! conjugate or hermitian transpose
    
    ! Integer size support for ILP64 builds should be done here
    integer, parameter :: ilp = int32

end module stdlib_sparse_constants
