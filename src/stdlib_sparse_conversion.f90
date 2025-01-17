!! The `stdlib_sparse_conversion` submodule provides sparse to sparse matrix conversion utilities.
!!
! This code was modified from https://github.com/jalvesz/FSPARSE by its author: Alves Jose
module stdlib_sparse_conversion
    use stdlib_sorting, only: sort
    use stdlib_sparse_constants
    use stdlib_sparse_kinds
    implicit none
    private
    !! Sort arrays of a COO matrix
    !! 
    interface sort_coo
        module procedure :: sort_coo_unique
        module procedure :: sort_coo_unique_sp
        module procedure :: sort_coo_unique_dp
        module procedure :: sort_coo_unique_csp
        module procedure :: sort_coo_unique_cdp
    end interface

    !! version: experimental
    !!
    !! Conversion from dense to coo
    !! Enables extracting the non-zero elements of a dense 2D matrix and
    !! storing those values in a COO format. The coo matrix is (re)allocated on the fly.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface dense2coo
        module procedure :: dense2coo_sp
        module procedure :: dense2coo_dp
        module procedure :: dense2coo_csp
        module procedure :: dense2coo_cdp
    end interface
    public :: dense2coo

    !! version: experimental
    !!
    !! Conversion from coo to dense
    !! Enables creating a dense 2D matrix from the non-zero values stored in a COO format
    !! The dense matrix can be allocated on the fly if not pre-allocated by the user.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface coo2dense
        module procedure :: coo2dense_sp
        module procedure :: coo2dense_dp
        module procedure :: coo2dense_csp
        module procedure :: coo2dense_cdp
    end interface
    public :: coo2dense

    !! version: experimental
    !!
    !! Conversion from coo to csr
    !! Enables transferring data from a COO matrix to a CSR matrix
    !! under the hypothesis that the COO is already ordered.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface coo2csr
        module procedure :: coo2csr_sp
        module procedure :: coo2csr_dp
        module procedure :: coo2csr_csp
        module procedure :: coo2csr_cdp
    end interface
    public :: coo2csr

    !! version: experimental
    !!
    !! Conversion from coo to csc
    !! Enables transferring data from a COO matrix to a CSC matrix
    !! under the hypothesis that the COO is already ordered.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface coo2csc
        module procedure :: coo2csc_sp
        module procedure :: coo2csc_dp
        module procedure :: coo2csc_csp
        module procedure :: coo2csc_cdp
    end interface
    public :: coo2csc
    
    !! version: experimental
    !!
    !! Conversion from csr to dense
    !! Enables creating a dense 2D matrix from the non-zero values stored in a CSR format
    !! The dense matrix can be allocated on the fly if not pre-allocated by the user.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface csr2dense
        module procedure :: csr2dense_sp
        module procedure :: csr2dense_dp
        module procedure :: csr2dense_csp
        module procedure :: csr2dense_cdp
    end interface
    public :: csr2dense

    !! version: experimental
    !!
    !! Conversion from csr to coo
    !! Enables transferring data from a CSR matrix to a COO matrix
    !! under the hypothesis that the CSR is already ordered.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface csr2coo
        module procedure :: csr2coo_sp
        module procedure :: csr2coo_dp
        module procedure :: csr2coo_csp
        module procedure :: csr2coo_cdp
    end interface
    public :: csr2coo

    !! version: experimental
    !!
    !! Conversion from csr to ell
    !! Enables transferring data from a CSR matrix to a ELL matrix
    !! under the hypothesis that the CSR is already ordered.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface csr2ell
        module procedure :: csr2ell_sp
        module procedure :: csr2ell_dp
        module procedure :: csr2ell_csp
        module procedure :: csr2ell_cdp
    end interface
    public :: csr2ell

    !! version: experimental
    !!
    !! Conversion from csr to SELL-C
    !! Enables transferring data from a CSR matrix to a SELL-C matrix
    !! It takes an optional parameter to decide the chunck size 4, 8 or 16
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface csr2sellc
        module procedure :: csr2sellc_sp
        module procedure :: csr2sellc_dp
        module procedure :: csr2sellc_csp
        module procedure :: csr2sellc_cdp
    end interface
    public :: csr2sellc

    !! version: experimental
    !!
    !! Conversion from csc to coo
    !! Enables transferring data from a CSC matrix to a COO matrix
    !! under the hypothesis that the CSC is already ordered.
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface csc2coo
        module procedure :: csc2coo_sp
        module procedure :: csc2coo_dp
        module procedure :: csc2coo_csp
        module procedure :: csc2coo_cdp
    end interface
    public :: csc2coo

    !! version: experimental
    !!
    !! Extraction of diagonal values
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface diag
        module procedure :: dense2diagonal_sp
        module procedure :: coo2diagonal_sp
        module procedure :: csr2diagonal_sp
        module procedure :: csc2diagonal_sp
        module procedure :: ell2diagonal_sp
        module procedure :: dense2diagonal_dp
        module procedure :: coo2diagonal_dp
        module procedure :: csr2diagonal_dp
        module procedure :: csc2diagonal_dp
        module procedure :: ell2diagonal_dp
        module procedure :: dense2diagonal_csp
        module procedure :: coo2diagonal_csp
        module procedure :: csr2diagonal_csp
        module procedure :: csc2diagonal_csp
        module procedure :: ell2diagonal_csp
        module procedure :: dense2diagonal_cdp
        module procedure :: coo2diagonal_cdp
        module procedure :: csr2diagonal_cdp
        module procedure :: csc2diagonal_cdp
        module procedure :: ell2diagonal_cdp
    end interface
    public :: diag

    !! version: experimental
    !!
    !! Enable creating a sparse matrix from ijv (row,col,data) triplet
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    interface from_ijv
        module procedure :: coo_from_ijv_type
        module procedure :: coo_from_ijv_sp
        module procedure :: csr_from_ijv_sp
        module procedure :: ell_from_ijv_sp
        module procedure :: sellc_from_ijv_sp
        module procedure :: coo_from_ijv_dp
        module procedure :: csr_from_ijv_dp
        module procedure :: ell_from_ijv_dp
        module procedure :: sellc_from_ijv_dp
        module procedure :: coo_from_ijv_csp
        module procedure :: csr_from_ijv_csp
        module procedure :: ell_from_ijv_csp
        module procedure :: sellc_from_ijv_csp
        module procedure :: coo_from_ijv_cdp
        module procedure :: csr_from_ijv_cdp
        module procedure :: ell_from_ijv_cdp
        module procedure :: sellc_from_ijv_cdp
    end interface
    public :: from_ijv

    public :: coo2ordered

contains

    subroutine dense2coo_sp(dense,COO)
        real(sp), intent(in) :: dense(:,:)
        type(COO_sp_type), intent(out) :: COO
        integer(ilp) :: num_rows, num_cols, nnz
        integer(ilp) :: i, j, idx

        num_rows = size(dense,dim=1)
        num_cols = size(dense,dim=2)
        nnz      = count( abs(dense) > tiny(1._sp) )

        call COO%malloc(num_rows,num_cols,nnz)

        idx = 1
        do i = 1, num_rows
            do j = 1, num_cols
                if(abs(dense(i,j)) < tiny(1._sp)) cycle
                COO%index(1,idx) = i
                COO%index(2,idx) = j
                COO%data(idx) = dense(i,j)
                idx = idx + 1
            end do
        end do
        COO%is_sorted = .true.
    end subroutine

    subroutine dense2coo_dp(dense,COO)
        real(dp), intent(in) :: dense(:,:)
        type(COO_dp_type), intent(out) :: COO
        integer(ilp) :: num_rows, num_cols, nnz
        integer(ilp) :: i, j, idx

        num_rows = size(dense,dim=1)
        num_cols = size(dense,dim=2)
        nnz      = count( abs(dense) > tiny(1._dp) )

        call COO%malloc(num_rows,num_cols,nnz)

        idx = 1
        do i = 1, num_rows
            do j = 1, num_cols
                if(abs(dense(i,j)) < tiny(1._dp)) cycle
                COO%index(1,idx) = i
                COO%index(2,idx) = j
                COO%data(idx) = dense(i,j)
                idx = idx + 1
            end do
        end do
        COO%is_sorted = .true.
    end subroutine

    subroutine dense2coo_csp(dense,COO)
        complex(sp), intent(in) :: dense(:,:)
        type(COO_csp_type), intent(out) :: COO
        integer(ilp) :: num_rows, num_cols, nnz
        integer(ilp) :: i, j, idx

        num_rows = size(dense,dim=1)
        num_cols = size(dense,dim=2)
        nnz      = count( abs(dense) > tiny(1._sp) )

        call COO%malloc(num_rows,num_cols,nnz)

        idx = 1
        do i = 1, num_rows
            do j = 1, num_cols
                if(abs(dense(i,j)) < tiny(1._sp)) cycle
                COO%index(1,idx) = i
                COO%index(2,idx) = j
                COO%data(idx) = dense(i,j)
                idx = idx + 1
            end do
        end do
        COO%is_sorted = .true.
    end subroutine

    subroutine dense2coo_cdp(dense,COO)
        complex(dp), intent(in) :: dense(:,:)
        type(COO_cdp_type), intent(out) :: COO
        integer(ilp) :: num_rows, num_cols, nnz
        integer(ilp) :: i, j, idx

        num_rows = size(dense,dim=1)
        num_cols = size(dense,dim=2)
        nnz      = count( abs(dense) > tiny(1._dp) )

        call COO%malloc(num_rows,num_cols,nnz)

        idx = 1
        do i = 1, num_rows
            do j = 1, num_cols
                if(abs(dense(i,j)) < tiny(1._dp)) cycle
                COO%index(1,idx) = i
                COO%index(2,idx) = j
                COO%data(idx) = dense(i,j)
                idx = idx + 1
            end do
        end do
        COO%is_sorted = .true.
    end subroutine


    subroutine coo2dense_sp(COO,dense)
        type(COO_sp_type), intent(in) :: COO
        real(sp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: idx

        if(.not.allocated(dense)) allocate(dense(COO%nrows,COO%nrows),source=zero_sp)
        do concurrent(idx = 1:COO%nnz)
            dense( COO%index(1,idx) , COO%index(2,idx) ) = COO%data(idx)
        end do
    end subroutine

    subroutine coo2dense_dp(COO,dense)
        type(COO_dp_type), intent(in) :: COO
        real(dp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: idx

        if(.not.allocated(dense)) allocate(dense(COO%nrows,COO%nrows),source=zero_dp)
        do concurrent(idx = 1:COO%nnz)
            dense( COO%index(1,idx) , COO%index(2,idx) ) = COO%data(idx)
        end do
    end subroutine

    subroutine coo2dense_csp(COO,dense)
        type(COO_csp_type), intent(in) :: COO
        complex(sp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: idx

        if(.not.allocated(dense)) allocate(dense(COO%nrows,COO%nrows),source=zero_csp)
        do concurrent(idx = 1:COO%nnz)
            dense( COO%index(1,idx) , COO%index(2,idx) ) = COO%data(idx)
        end do
    end subroutine

    subroutine coo2dense_cdp(COO,dense)
        type(COO_cdp_type), intent(in) :: COO
        complex(dp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: idx

        if(.not.allocated(dense)) allocate(dense(COO%nrows,COO%nrows),source=zero_cdp)
        do concurrent(idx = 1:COO%nnz)
            dense( COO%index(1,idx) , COO%index(2,idx) ) = COO%data(idx)
        end do
    end subroutine


    subroutine coo2csr_sp(COO,CSR)
        type(COO_sp_type), intent(in)  :: COO
        type(CSR_sp_type), intent(out) :: CSR
        integer(ilp) :: i

        CSR%nnz = COO%nnz; CSR%nrows = COO%nrows; CSR%ncols = COO%ncols
        CSR%storage = COO%storage

        if( allocated(CSR%col) ) then
            CSR%col(1:COO%nnz)  = COO%index(2,1:COO%nnz)
            CSR%rowptr(1:CSR%nrows) = 0
            CSR%data(1:CSR%nnz) = COO%data(1:COO%nnz)
        else 
            allocate( CSR%col(CSR%nnz)  , source = COO%index(2,1:COO%nnz) )
            allocate( CSR%rowptr(CSR%nrows+1) , source = 0 )
            allocate( CSR%data(CSR%nnz) , source = COO%data(1:COO%nnz) )
        end if

        CSR%rowptr(1) = 1
        do i = 1, COO%nnz
            CSR%rowptr( COO%index(1,i)+1 ) = CSR%rowptr( COO%index(1,i)+1 ) + 1
        end do
        do i = 1, CSR%nrows
            CSR%rowptr( i+1 ) = CSR%rowptr( i+1 ) + CSR%rowptr( i )
        end do
    end subroutine

    subroutine coo2csr_dp(COO,CSR)
        type(COO_dp_type), intent(in)  :: COO
        type(CSR_dp_type), intent(out) :: CSR
        integer(ilp) :: i

        CSR%nnz = COO%nnz; CSR%nrows = COO%nrows; CSR%ncols = COO%ncols
        CSR%storage = COO%storage

        if( allocated(CSR%col) ) then
            CSR%col(1:COO%nnz)  = COO%index(2,1:COO%nnz)
            CSR%rowptr(1:CSR%nrows) = 0
            CSR%data(1:CSR%nnz) = COO%data(1:COO%nnz)
        else 
            allocate( CSR%col(CSR%nnz)  , source = COO%index(2,1:COO%nnz) )
            allocate( CSR%rowptr(CSR%nrows+1) , source = 0 )
            allocate( CSR%data(CSR%nnz) , source = COO%data(1:COO%nnz) )
        end if

        CSR%rowptr(1) = 1
        do i = 1, COO%nnz
            CSR%rowptr( COO%index(1,i)+1 ) = CSR%rowptr( COO%index(1,i)+1 ) + 1
        end do
        do i = 1, CSR%nrows
            CSR%rowptr( i+1 ) = CSR%rowptr( i+1 ) + CSR%rowptr( i )
        end do
    end subroutine

    subroutine coo2csr_csp(COO,CSR)
        type(COO_csp_type), intent(in)  :: COO
        type(CSR_csp_type), intent(out) :: CSR
        integer(ilp) :: i

        CSR%nnz = COO%nnz; CSR%nrows = COO%nrows; CSR%ncols = COO%ncols
        CSR%storage = COO%storage

        if( allocated(CSR%col) ) then
            CSR%col(1:COO%nnz)  = COO%index(2,1:COO%nnz)
            CSR%rowptr(1:CSR%nrows) = 0
            CSR%data(1:CSR%nnz) = COO%data(1:COO%nnz)
        else 
            allocate( CSR%col(CSR%nnz)  , source = COO%index(2,1:COO%nnz) )
            allocate( CSR%rowptr(CSR%nrows+1) , source = 0 )
            allocate( CSR%data(CSR%nnz) , source = COO%data(1:COO%nnz) )
        end if

        CSR%rowptr(1) = 1
        do i = 1, COO%nnz
            CSR%rowptr( COO%index(1,i)+1 ) = CSR%rowptr( COO%index(1,i)+1 ) + 1
        end do
        do i = 1, CSR%nrows
            CSR%rowptr( i+1 ) = CSR%rowptr( i+1 ) + CSR%rowptr( i )
        end do
    end subroutine

    subroutine coo2csr_cdp(COO,CSR)
        type(COO_cdp_type), intent(in)  :: COO
        type(CSR_cdp_type), intent(out) :: CSR
        integer(ilp) :: i

        CSR%nnz = COO%nnz; CSR%nrows = COO%nrows; CSR%ncols = COO%ncols
        CSR%storage = COO%storage

        if( allocated(CSR%col) ) then
            CSR%col(1:COO%nnz)  = COO%index(2,1:COO%nnz)
            CSR%rowptr(1:CSR%nrows) = 0
            CSR%data(1:CSR%nnz) = COO%data(1:COO%nnz)
        else 
            allocate( CSR%col(CSR%nnz)  , source = COO%index(2,1:COO%nnz) )
            allocate( CSR%rowptr(CSR%nrows+1) , source = 0 )
            allocate( CSR%data(CSR%nnz) , source = COO%data(1:COO%nnz) )
        end if

        CSR%rowptr(1) = 1
        do i = 1, COO%nnz
            CSR%rowptr( COO%index(1,i)+1 ) = CSR%rowptr( COO%index(1,i)+1 ) + 1
        end do
        do i = 1, CSR%nrows
            CSR%rowptr( i+1 ) = CSR%rowptr( i+1 ) + CSR%rowptr( i )
        end do
    end subroutine


    subroutine coo2csc_sp(COO,CSC)
        type(COO_sp_type), intent(in)  :: COO
        type(CSC_sp_type), intent(out) :: CSC
        real(sp), allocatable :: data(:)
        integer(ilp), allocatable :: temp(:,:)
        integer(ilp) :: i, nnz

        CSC%nnz = COO%nnz; CSC%nrows = COO%nrows; CSC%ncols = COO%ncols
        CSC%storage = COO%storage

        allocate(temp(2,COO%nnz))
        temp(1,1:COO%nnz) = COO%index(2,1:COO%nnz)
        temp(2,1:COO%nnz) = COO%index(1,1:COO%nnz)
        allocate(data, source = COO%data )
        nnz = COO%nnz
        call sort_coo_unique_sp( temp, data, nnz, COO%nrows, COO%ncols )

        if( allocated(CSC%row) ) then
            CSC%row(1:COO%nnz)  = temp(2,1:COO%nnz)
            CSC%colptr(1:CSC%ncols) = 0
            CSC%data(1:CSC%nnz) = data(1:COO%nnz)
        else 
            allocate( CSC%row(CSC%nnz)  , source = temp(2,1:COO%nnz) )
            allocate( CSC%colptr(CSC%ncols+1) , source = 0 )
            allocate( CSC%data(CSC%nnz) , source = data(1:COO%nnz) )
        end if

        CSC%colptr(1) = 1
        do i = 1, COO%nnz
            CSC%colptr( temp(1,i)+1 ) = CSC%colptr( temp(1,i)+1 ) + 1
        end do
        do i = 1, CSC%ncols
            CSC%colptr( i+1 ) = CSC%colptr( i+1 ) + CSC%colptr( i )
        end do
    end subroutine

    subroutine coo2csc_dp(COO,CSC)
        type(COO_dp_type), intent(in)  :: COO
        type(CSC_dp_type), intent(out) :: CSC
        real(dp), allocatable :: data(:)
        integer(ilp), allocatable :: temp(:,:)
        integer(ilp) :: i, nnz

        CSC%nnz = COO%nnz; CSC%nrows = COO%nrows; CSC%ncols = COO%ncols
        CSC%storage = COO%storage

        allocate(temp(2,COO%nnz))
        temp(1,1:COO%nnz) = COO%index(2,1:COO%nnz)
        temp(2,1:COO%nnz) = COO%index(1,1:COO%nnz)
        allocate(data, source = COO%data )
        nnz = COO%nnz
        call sort_coo_unique_dp( temp, data, nnz, COO%nrows, COO%ncols )

        if( allocated(CSC%row) ) then
            CSC%row(1:COO%nnz)  = temp(2,1:COO%nnz)
            CSC%colptr(1:CSC%ncols) = 0
            CSC%data(1:CSC%nnz) = data(1:COO%nnz)
        else 
            allocate( CSC%row(CSC%nnz)  , source = temp(2,1:COO%nnz) )
            allocate( CSC%colptr(CSC%ncols+1) , source = 0 )
            allocate( CSC%data(CSC%nnz) , source = data(1:COO%nnz) )
        end if

        CSC%colptr(1) = 1
        do i = 1, COO%nnz
            CSC%colptr( temp(1,i)+1 ) = CSC%colptr( temp(1,i)+1 ) + 1
        end do
        do i = 1, CSC%ncols
            CSC%colptr( i+1 ) = CSC%colptr( i+1 ) + CSC%colptr( i )
        end do
    end subroutine

    subroutine coo2csc_csp(COO,CSC)
        type(COO_csp_type), intent(in)  :: COO
        type(CSC_csp_type), intent(out) :: CSC
        complex(sp), allocatable :: data(:)
        integer(ilp), allocatable :: temp(:,:)
        integer(ilp) :: i, nnz

        CSC%nnz = COO%nnz; CSC%nrows = COO%nrows; CSC%ncols = COO%ncols
        CSC%storage = COO%storage

        allocate(temp(2,COO%nnz))
        temp(1,1:COO%nnz) = COO%index(2,1:COO%nnz)
        temp(2,1:COO%nnz) = COO%index(1,1:COO%nnz)
        allocate(data, source = COO%data )
        nnz = COO%nnz
        call sort_coo_unique_csp( temp, data, nnz, COO%nrows, COO%ncols )

        if( allocated(CSC%row) ) then
            CSC%row(1:COO%nnz)  = temp(2,1:COO%nnz)
            CSC%colptr(1:CSC%ncols) = 0
            CSC%data(1:CSC%nnz) = data(1:COO%nnz)
        else 
            allocate( CSC%row(CSC%nnz)  , source = temp(2,1:COO%nnz) )
            allocate( CSC%colptr(CSC%ncols+1) , source = 0 )
            allocate( CSC%data(CSC%nnz) , source = data(1:COO%nnz) )
        end if

        CSC%colptr(1) = 1
        do i = 1, COO%nnz
            CSC%colptr( temp(1,i)+1 ) = CSC%colptr( temp(1,i)+1 ) + 1
        end do
        do i = 1, CSC%ncols
            CSC%colptr( i+1 ) = CSC%colptr( i+1 ) + CSC%colptr( i )
        end do
    end subroutine

    subroutine coo2csc_cdp(COO,CSC)
        type(COO_cdp_type), intent(in)  :: COO
        type(CSC_cdp_type), intent(out) :: CSC
        complex(dp), allocatable :: data(:)
        integer(ilp), allocatable :: temp(:,:)
        integer(ilp) :: i, nnz

        CSC%nnz = COO%nnz; CSC%nrows = COO%nrows; CSC%ncols = COO%ncols
        CSC%storage = COO%storage

        allocate(temp(2,COO%nnz))
        temp(1,1:COO%nnz) = COO%index(2,1:COO%nnz)
        temp(2,1:COO%nnz) = COO%index(1,1:COO%nnz)
        allocate(data, source = COO%data )
        nnz = COO%nnz
        call sort_coo_unique_cdp( temp, data, nnz, COO%nrows, COO%ncols )

        if( allocated(CSC%row) ) then
            CSC%row(1:COO%nnz)  = temp(2,1:COO%nnz)
            CSC%colptr(1:CSC%ncols) = 0
            CSC%data(1:CSC%nnz) = data(1:COO%nnz)
        else 
            allocate( CSC%row(CSC%nnz)  , source = temp(2,1:COO%nnz) )
            allocate( CSC%colptr(CSC%ncols+1) , source = 0 )
            allocate( CSC%data(CSC%nnz) , source = data(1:COO%nnz) )
        end if

        CSC%colptr(1) = 1
        do i = 1, COO%nnz
            CSC%colptr( temp(1,i)+1 ) = CSC%colptr( temp(1,i)+1 ) + 1
        end do
        do i = 1, CSC%ncols
            CSC%colptr( i+1 ) = CSC%colptr( i+1 ) + CSC%colptr( i )
        end do
    end subroutine


    subroutine csr2dense_sp(CSR,dense)
        type(CSR_sp_type), intent(in) :: CSR
        real(sp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: i, j

        if(.not.allocated(dense)) allocate(dense(CSR%nrows,CSR%nrows),source=zero_sp)
        if( CSR%storage == sparse_full) then
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                end do
            end do
        else
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                    if( i == CSR%col(j) ) cycle
                    dense(CSR%col(j),i) = CSR%data(j)
                end do
            end do
        end if
    end subroutine

    subroutine csr2dense_dp(CSR,dense)
        type(CSR_dp_type), intent(in) :: CSR
        real(dp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: i, j

        if(.not.allocated(dense)) allocate(dense(CSR%nrows,CSR%nrows),source=zero_dp)
        if( CSR%storage == sparse_full) then
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                end do
            end do
        else
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                    if( i == CSR%col(j) ) cycle
                    dense(CSR%col(j),i) = CSR%data(j)
                end do
            end do
        end if
    end subroutine

    subroutine csr2dense_csp(CSR,dense)
        type(CSR_csp_type), intent(in) :: CSR
        complex(sp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: i, j

        if(.not.allocated(dense)) allocate(dense(CSR%nrows,CSR%nrows),source=zero_csp)
        if( CSR%storage == sparse_full) then
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                end do
            end do
        else
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                    if( i == CSR%col(j) ) cycle
                    dense(CSR%col(j),i) = CSR%data(j)
                end do
            end do
        end if
    end subroutine

    subroutine csr2dense_cdp(CSR,dense)
        type(CSR_cdp_type), intent(in) :: CSR
        complex(dp), allocatable, intent(out) :: dense(:,:)
        integer(ilp) :: i, j

        if(.not.allocated(dense)) allocate(dense(CSR%nrows,CSR%nrows),source=zero_cdp)
        if( CSR%storage == sparse_full) then
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                end do
            end do
        else
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    dense(i,CSR%col(j)) = CSR%data(j)
                    if( i == CSR%col(j) ) cycle
                    dense(CSR%col(j),i) = CSR%data(j)
                end do
            end do
        end if
    end subroutine


    subroutine csr2coo_sp(CSR,COO)
        type(CSR_sp_type), intent(in)  :: CSR
        type(COO_sp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSR%nnz; COO%nrows = CSR%nrows; COO%ncols = CSR%ncols
        COO%storage = CSR%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSR%nnz) , source = CSR%data(1:CSR%nnz) )
        else 
            COO%data(1:CSR%nnz) = CSR%data(1:CSR%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSR%nnz) )
        
        do i = 1, CSR%nrows
            do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                COO%index(1:2,j) = [i,CSR%col(j)]
            end do
        end do
    end subroutine

    subroutine csr2coo_dp(CSR,COO)
        type(CSR_dp_type), intent(in)  :: CSR
        type(COO_dp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSR%nnz; COO%nrows = CSR%nrows; COO%ncols = CSR%ncols
        COO%storage = CSR%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSR%nnz) , source = CSR%data(1:CSR%nnz) )
        else 
            COO%data(1:CSR%nnz) = CSR%data(1:CSR%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSR%nnz) )
        
        do i = 1, CSR%nrows
            do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                COO%index(1:2,j) = [i,CSR%col(j)]
            end do
        end do
    end subroutine

    subroutine csr2coo_csp(CSR,COO)
        type(CSR_csp_type), intent(in)  :: CSR
        type(COO_csp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSR%nnz; COO%nrows = CSR%nrows; COO%ncols = CSR%ncols
        COO%storage = CSR%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSR%nnz) , source = CSR%data(1:CSR%nnz) )
        else 
            COO%data(1:CSR%nnz) = CSR%data(1:CSR%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSR%nnz) )
        
        do i = 1, CSR%nrows
            do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                COO%index(1:2,j) = [i,CSR%col(j)]
            end do
        end do
    end subroutine

    subroutine csr2coo_cdp(CSR,COO)
        type(CSR_cdp_type), intent(in)  :: CSR
        type(COO_cdp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSR%nnz; COO%nrows = CSR%nrows; COO%ncols = CSR%ncols
        COO%storage = CSR%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSR%nnz) , source = CSR%data(1:CSR%nnz) )
        else 
            COO%data(1:CSR%nnz) = CSR%data(1:CSR%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSR%nnz) )
        
        do i = 1, CSR%nrows
            do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                COO%index(1:2,j) = [i,CSR%col(j)]
            end do
        end do
    end subroutine


    subroutine csc2coo_sp(CSC,COO)
        type(CSC_sp_type), intent(in)  :: CSC
        type(COO_sp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSC%nnz; COO%nrows = CSC%nrows; COO%ncols = CSC%ncols
        COO%storage = CSC%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSC%nnz) , source = CSC%data(1:CSC%nnz) )
        else 
            COO%data(1:CSC%nnz) = CSC%data(1:CSC%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSC%nnz) )
        
        do j = 1, CSC%ncols
            do i = CSC%colptr(j), CSC%colptr(j+1)-1
                COO%index(1:2,i) = [CSC%row(i),j]
            end do
        end do
        call sort_coo_unique_sp( COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols )
    end subroutine

    subroutine csc2coo_dp(CSC,COO)
        type(CSC_dp_type), intent(in)  :: CSC
        type(COO_dp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSC%nnz; COO%nrows = CSC%nrows; COO%ncols = CSC%ncols
        COO%storage = CSC%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSC%nnz) , source = CSC%data(1:CSC%nnz) )
        else 
            COO%data(1:CSC%nnz) = CSC%data(1:CSC%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSC%nnz) )
        
        do j = 1, CSC%ncols
            do i = CSC%colptr(j), CSC%colptr(j+1)-1
                COO%index(1:2,i) = [CSC%row(i),j]
            end do
        end do
        call sort_coo_unique_dp( COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols )
    end subroutine

    subroutine csc2coo_csp(CSC,COO)
        type(CSC_csp_type), intent(in)  :: CSC
        type(COO_csp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSC%nnz; COO%nrows = CSC%nrows; COO%ncols = CSC%ncols
        COO%storage = CSC%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSC%nnz) , source = CSC%data(1:CSC%nnz) )
        else 
            COO%data(1:CSC%nnz) = CSC%data(1:CSC%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSC%nnz) )
        
        do j = 1, CSC%ncols
            do i = CSC%colptr(j), CSC%colptr(j+1)-1
                COO%index(1:2,i) = [CSC%row(i),j]
            end do
        end do
        call sort_coo_unique_csp( COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols )
    end subroutine

    subroutine csc2coo_cdp(CSC,COO)
        type(CSC_cdp_type), intent(in)  :: CSC
        type(COO_cdp_type), intent(out) :: COO
        integer(ilp) :: i, j

        COO%nnz = CSC%nnz; COO%nrows = CSC%nrows; COO%ncols = CSC%ncols
        COO%storage = CSC%storage

        if( .not.allocated(COO%data) ) then
            allocate( COO%data(CSC%nnz) , source = CSC%data(1:CSC%nnz) )
        else 
            COO%data(1:CSC%nnz) = CSC%data(1:CSC%nnz)
        end if

        if( .not.allocated(COO%index) ) allocate( COO%index(2,CSC%nnz) )
        
        do j = 1, CSC%ncols
            do i = CSC%colptr(j), CSC%colptr(j+1)-1
                COO%index(1:2,i) = [CSC%row(i),j]
            end do
        end do
        call sort_coo_unique_cdp( COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols )
    end subroutine


    subroutine csr2ell_sp(CSR,ELL,num_nz_rows)
        type(CSR_sp_type), intent(in)  :: CSR
        type(ELL_sp_type), intent(out) :: ELL
        integer, intent(in), optional :: num_nz_rows !! number of non zeros per row

        integer(ilp) :: i, j, num_nz_rows_, adr1, adr2
        !-------------------------------------------
        num_nz_rows_ = 0
        if(present(num_nz_rows)) then
            num_nz_rows_ = num_nz_rows
        else 
            do i = 1, CSR%nrows
                num_nz_rows_ = max(num_nz_rows_, CSR%rowptr( i+1 ) - CSR%rowptr( i ) )
            end do
        end if
        call ELL%malloc(CSR%nrows,CSR%ncols,num_nz_rows_)
        ELL%storage = CSR%storage
        !-------------------------------------------
        do i = 1, CSR%nrows
            adr1 = CSR%rowptr(i)
            adr2 = min( adr1+num_nz_rows_ , CSR%rowptr(i+1)-1)
            do j = adr1, adr2
                ELL%index(i,j-adr1+1) = CSR%col(j)
                ELL%data(i,j-adr1+1)  = CSR%data(j)
            end do
        end do
    end subroutine

    subroutine csr2ell_dp(CSR,ELL,num_nz_rows)
        type(CSR_dp_type), intent(in)  :: CSR
        type(ELL_dp_type), intent(out) :: ELL
        integer, intent(in), optional :: num_nz_rows !! number of non zeros per row

        integer(ilp) :: i, j, num_nz_rows_, adr1, adr2
        !-------------------------------------------
        num_nz_rows_ = 0
        if(present(num_nz_rows)) then
            num_nz_rows_ = num_nz_rows
        else 
            do i = 1, CSR%nrows
                num_nz_rows_ = max(num_nz_rows_, CSR%rowptr( i+1 ) - CSR%rowptr( i ) )
            end do
        end if
        call ELL%malloc(CSR%nrows,CSR%ncols,num_nz_rows_)
        ELL%storage = CSR%storage
        !-------------------------------------------
        do i = 1, CSR%nrows
            adr1 = CSR%rowptr(i)
            adr2 = min( adr1+num_nz_rows_ , CSR%rowptr(i+1)-1)
            do j = adr1, adr2
                ELL%index(i,j-adr1+1) = CSR%col(j)
                ELL%data(i,j-adr1+1)  = CSR%data(j)
            end do
        end do
    end subroutine

    subroutine csr2ell_csp(CSR,ELL,num_nz_rows)
        type(CSR_csp_type), intent(in)  :: CSR
        type(ELL_csp_type), intent(out) :: ELL
        integer, intent(in), optional :: num_nz_rows !! number of non zeros per row

        integer(ilp) :: i, j, num_nz_rows_, adr1, adr2
        !-------------------------------------------
        num_nz_rows_ = 0
        if(present(num_nz_rows)) then
            num_nz_rows_ = num_nz_rows
        else 
            do i = 1, CSR%nrows
                num_nz_rows_ = max(num_nz_rows_, CSR%rowptr( i+1 ) - CSR%rowptr( i ) )
            end do
        end if
        call ELL%malloc(CSR%nrows,CSR%ncols,num_nz_rows_)
        ELL%storage = CSR%storage
        !-------------------------------------------
        do i = 1, CSR%nrows
            adr1 = CSR%rowptr(i)
            adr2 = min( adr1+num_nz_rows_ , CSR%rowptr(i+1)-1)
            do j = adr1, adr2
                ELL%index(i,j-adr1+1) = CSR%col(j)
                ELL%data(i,j-adr1+1)  = CSR%data(j)
            end do
        end do
    end subroutine

    subroutine csr2ell_cdp(CSR,ELL,num_nz_rows)
        type(CSR_cdp_type), intent(in)  :: CSR
        type(ELL_cdp_type), intent(out) :: ELL
        integer, intent(in), optional :: num_nz_rows !! number of non zeros per row

        integer(ilp) :: i, j, num_nz_rows_, adr1, adr2
        !-------------------------------------------
        num_nz_rows_ = 0
        if(present(num_nz_rows)) then
            num_nz_rows_ = num_nz_rows
        else 
            do i = 1, CSR%nrows
                num_nz_rows_ = max(num_nz_rows_, CSR%rowptr( i+1 ) - CSR%rowptr( i ) )
            end do
        end if
        call ELL%malloc(CSR%nrows,CSR%ncols,num_nz_rows_)
        ELL%storage = CSR%storage
        !-------------------------------------------
        do i = 1, CSR%nrows
            adr1 = CSR%rowptr(i)
            adr2 = min( adr1+num_nz_rows_ , CSR%rowptr(i+1)-1)
            do j = adr1, adr2
                ELL%index(i,j-adr1+1) = CSR%col(j)
                ELL%data(i,j-adr1+1)  = CSR%data(j)
            end do
        end do
    end subroutine


    subroutine csr2sellc_sp(CSR,SELLC,chunk)
        !! csr2sellc: This function enables transfering data from a CSR matrix to a SELL-C matrix
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(CSR_sp_type), intent(in)    :: CSR
        type(SELLC_sp_type), intent(out) :: SELLC
        integer, intent(in), optional :: chunk
        real(sp), parameter :: zero = zero_sp
        integer(ilp) :: i, j, num_chunks

        if(present(chunk)) SELLC%chunk_size = chunk

        SELLC%nrows = CSR%nrows; SELLC%ncols = CSR%ncols
        SELLC%storage = CSR%storage
        associate( nrows=>SELLC%nrows, ncols=>SELLC%ncols, nnz=>SELLC%nnz, &
        &         chunk_size=>SELLC%chunk_size     )
        !-------------------------------------------
        ! csr rowptr to SELL-C chunked rowptr
        num_chunks = (nrows + chunk_size - 1)/chunk_size
        allocate( SELLC%rowptr(num_chunks+1) )
        block
            integer :: cidx, rownnz, chunknnz
            SELLC%rowptr(1) = 1
            cidx = 1
            do i = 1, nrows, chunk_size
                chunknnz = 0
                ! Iterate over rows in a given chunk
                do j = i, min(i+chunk_size-1,nrows)
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j)
                    chunknnz = max(chunknnz,rownnz)
                end do
                SELLC%rowptr(cidx+1) = SELLC%rowptr(cidx) + chunknnz
                cidx = cidx + 1
            end do
            nnz = SELLC%rowptr(num_chunks+1) - 1
        end block
        !-------------------------------------------
        ! copy values and colum index
        allocate(SELLC%col(chunk_size,nnz), source = 1)
        allocate(SELLC%data(chunk_size,nnz), source = zero )
        block
            integer :: lb, ri, iaa, iab, rownnz
            do i = 1, num_chunks

                lb = SELLC%rowptr(i)

                ! Loop over rows of a chunk
                do j = (i-1)*chunk_size + 1, min(i*chunk_size,nrows)
    
                    ri = j - (i - 1)*chunk_size
                    
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j) - 1
                    iaa    = CSR%rowptr(j)
                    iab    = CSR%rowptr(j+1) - 1
                    
                    SELLC%col(ri,lb:lb+rownnz)  = CSR%col(iaa:iab)
                    SELLC%data(ri,lb:lb+rownnz) = CSR%data(iaa:iab)
                
                end do
            end do
         end block
        end associate
    end subroutine

    subroutine csr2sellc_dp(CSR,SELLC,chunk)
        !! csr2sellc: This function enables transfering data from a CSR matrix to a SELL-C matrix
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(CSR_dp_type), intent(in)    :: CSR
        type(SELLC_dp_type), intent(out) :: SELLC
        integer, intent(in), optional :: chunk
        real(dp), parameter :: zero = zero_dp
        integer(ilp) :: i, j, num_chunks

        if(present(chunk)) SELLC%chunk_size = chunk

        SELLC%nrows = CSR%nrows; SELLC%ncols = CSR%ncols
        SELLC%storage = CSR%storage
        associate( nrows=>SELLC%nrows, ncols=>SELLC%ncols, nnz=>SELLC%nnz, &
        &         chunk_size=>SELLC%chunk_size     )
        !-------------------------------------------
        ! csr rowptr to SELL-C chunked rowptr
        num_chunks = (nrows + chunk_size - 1)/chunk_size
        allocate( SELLC%rowptr(num_chunks+1) )
        block
            integer :: cidx, rownnz, chunknnz
            SELLC%rowptr(1) = 1
            cidx = 1
            do i = 1, nrows, chunk_size
                chunknnz = 0
                ! Iterate over rows in a given chunk
                do j = i, min(i+chunk_size-1,nrows)
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j)
                    chunknnz = max(chunknnz,rownnz)
                end do
                SELLC%rowptr(cidx+1) = SELLC%rowptr(cidx) + chunknnz
                cidx = cidx + 1
            end do
            nnz = SELLC%rowptr(num_chunks+1) - 1
        end block
        !-------------------------------------------
        ! copy values and colum index
        allocate(SELLC%col(chunk_size,nnz), source = 1)
        allocate(SELLC%data(chunk_size,nnz), source = zero )
        block
            integer :: lb, ri, iaa, iab, rownnz
            do i = 1, num_chunks

                lb = SELLC%rowptr(i)

                ! Loop over rows of a chunk
                do j = (i-1)*chunk_size + 1, min(i*chunk_size,nrows)
    
                    ri = j - (i - 1)*chunk_size
                    
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j) - 1
                    iaa    = CSR%rowptr(j)
                    iab    = CSR%rowptr(j+1) - 1
                    
                    SELLC%col(ri,lb:lb+rownnz)  = CSR%col(iaa:iab)
                    SELLC%data(ri,lb:lb+rownnz) = CSR%data(iaa:iab)
                
                end do
            end do
         end block
        end associate
    end subroutine

    subroutine csr2sellc_csp(CSR,SELLC,chunk)
        !! csr2sellc: This function enables transfering data from a CSR matrix to a SELL-C matrix
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(CSR_csp_type), intent(in)    :: CSR
        type(SELLC_csp_type), intent(out) :: SELLC
        integer, intent(in), optional :: chunk
        complex(sp), parameter :: zero = zero_csp
        integer(ilp) :: i, j, num_chunks

        if(present(chunk)) SELLC%chunk_size = chunk

        SELLC%nrows = CSR%nrows; SELLC%ncols = CSR%ncols
        SELLC%storage = CSR%storage
        associate( nrows=>SELLC%nrows, ncols=>SELLC%ncols, nnz=>SELLC%nnz, &
        &         chunk_size=>SELLC%chunk_size     )
        !-------------------------------------------
        ! csr rowptr to SELL-C chunked rowptr
        num_chunks = (nrows + chunk_size - 1)/chunk_size
        allocate( SELLC%rowptr(num_chunks+1) )
        block
            integer :: cidx, rownnz, chunknnz
            SELLC%rowptr(1) = 1
            cidx = 1
            do i = 1, nrows, chunk_size
                chunknnz = 0
                ! Iterate over rows in a given chunk
                do j = i, min(i+chunk_size-1,nrows)
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j)
                    chunknnz = max(chunknnz,rownnz)
                end do
                SELLC%rowptr(cidx+1) = SELLC%rowptr(cidx) + chunknnz
                cidx = cidx + 1
            end do
            nnz = SELLC%rowptr(num_chunks+1) - 1
        end block
        !-------------------------------------------
        ! copy values and colum index
        allocate(SELLC%col(chunk_size,nnz), source = 1)
        allocate(SELLC%data(chunk_size,nnz), source = zero )
        block
            integer :: lb, ri, iaa, iab, rownnz
            do i = 1, num_chunks

                lb = SELLC%rowptr(i)

                ! Loop over rows of a chunk
                do j = (i-1)*chunk_size + 1, min(i*chunk_size,nrows)
    
                    ri = j - (i - 1)*chunk_size
                    
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j) - 1
                    iaa    = CSR%rowptr(j)
                    iab    = CSR%rowptr(j+1) - 1
                    
                    SELLC%col(ri,lb:lb+rownnz)  = CSR%col(iaa:iab)
                    SELLC%data(ri,lb:lb+rownnz) = CSR%data(iaa:iab)
                
                end do
            end do
         end block
        end associate
    end subroutine

    subroutine csr2sellc_cdp(CSR,SELLC,chunk)
        !! csr2sellc: This function enables transfering data from a CSR matrix to a SELL-C matrix
        !! This algorithm was gracefully provided by Ivan Privec and adapted by Jose Alves
        type(CSR_cdp_type), intent(in)    :: CSR
        type(SELLC_cdp_type), intent(out) :: SELLC
        integer, intent(in), optional :: chunk
        complex(dp), parameter :: zero = zero_cdp
        integer(ilp) :: i, j, num_chunks

        if(present(chunk)) SELLC%chunk_size = chunk

        SELLC%nrows = CSR%nrows; SELLC%ncols = CSR%ncols
        SELLC%storage = CSR%storage
        associate( nrows=>SELLC%nrows, ncols=>SELLC%ncols, nnz=>SELLC%nnz, &
        &         chunk_size=>SELLC%chunk_size     )
        !-------------------------------------------
        ! csr rowptr to SELL-C chunked rowptr
        num_chunks = (nrows + chunk_size - 1)/chunk_size
        allocate( SELLC%rowptr(num_chunks+1) )
        block
            integer :: cidx, rownnz, chunknnz
            SELLC%rowptr(1) = 1
            cidx = 1
            do i = 1, nrows, chunk_size
                chunknnz = 0
                ! Iterate over rows in a given chunk
                do j = i, min(i+chunk_size-1,nrows)
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j)
                    chunknnz = max(chunknnz,rownnz)
                end do
                SELLC%rowptr(cidx+1) = SELLC%rowptr(cidx) + chunknnz
                cidx = cidx + 1
            end do
            nnz = SELLC%rowptr(num_chunks+1) - 1
        end block
        !-------------------------------------------
        ! copy values and colum index
        allocate(SELLC%col(chunk_size,nnz), source = 1)
        allocate(SELLC%data(chunk_size,nnz), source = zero )
        block
            integer :: lb, ri, iaa, iab, rownnz
            do i = 1, num_chunks

                lb = SELLC%rowptr(i)

                ! Loop over rows of a chunk
                do j = (i-1)*chunk_size + 1, min(i*chunk_size,nrows)
    
                    ri = j - (i - 1)*chunk_size
                    
                    rownnz = CSR%rowptr(j+1) - CSR%rowptr(j) - 1
                    iaa    = CSR%rowptr(j)
                    iab    = CSR%rowptr(j+1) - 1
                    
                    SELLC%col(ri,lb:lb+rownnz)  = CSR%col(iaa:iab)
                    SELLC%data(ri,lb:lb+rownnz) = CSR%data(iaa:iab)
                
                end do
            end do
         end block
        end associate
    end subroutine


    recursive subroutine quicksort_i_sp(a, b, first, last)
        integer(ilp), intent(inout) :: a(*) !! reference table to sort
        real(sp), intent(inout)  :: b(*) !! secondary real data to sort w.r.t. a
        integer(ilp), intent(in)     :: first, last
        integer(ilp)  :: i, j, x, t
        real(sp) :: d

        x = a( (first+last) / 2 )
        i = first
        j = last
        do
            do while (a(i) < x)
                i=i+1
            end do
            do while (x < a(j))
                j=j-1
            end do
            if (i >= j) exit
            t = a(i);  a(i) = a(j);  a(j) = t
            d = b(i);  b(i) = b(j);  b(j) = d
            i=i+1
            j=j-1
        end do
        if (first < i-1) call quicksort_i_sp(a, b, first, i-1)
        if (j+1 < last)  call quicksort_i_sp(a, b, j+1, last)
    end subroutine 

    recursive subroutine quicksort_i_dp(a, b, first, last)
        integer(ilp), intent(inout) :: a(*) !! reference table to sort
        real(dp), intent(inout)  :: b(*) !! secondary real data to sort w.r.t. a
        integer(ilp), intent(in)     :: first, last
        integer(ilp)  :: i, j, x, t
        real(dp) :: d

        x = a( (first+last) / 2 )
        i = first
        j = last
        do
            do while (a(i) < x)
                i=i+1
            end do
            do while (x < a(j))
                j=j-1
            end do
            if (i >= j) exit
            t = a(i);  a(i) = a(j);  a(j) = t
            d = b(i);  b(i) = b(j);  b(j) = d
            i=i+1
            j=j-1
        end do
        if (first < i-1) call quicksort_i_dp(a, b, first, i-1)
        if (j+1 < last)  call quicksort_i_dp(a, b, j+1, last)
    end subroutine 

    recursive subroutine quicksort_i_csp(a, b, first, last)
        integer(ilp), intent(inout) :: a(*) !! reference table to sort
        complex(sp), intent(inout)  :: b(*) !! secondary real data to sort w.r.t. a
        integer(ilp), intent(in)     :: first, last
        integer(ilp)  :: i, j, x, t
        complex(sp) :: d

        x = a( (first+last) / 2 )
        i = first
        j = last
        do
            do while (a(i) < x)
                i=i+1
            end do
            do while (x < a(j))
                j=j-1
            end do
            if (i >= j) exit
            t = a(i);  a(i) = a(j);  a(j) = t
            d = b(i);  b(i) = b(j);  b(j) = d
            i=i+1
            j=j-1
        end do
        if (first < i-1) call quicksort_i_csp(a, b, first, i-1)
        if (j+1 < last)  call quicksort_i_csp(a, b, j+1, last)
    end subroutine 

    recursive subroutine quicksort_i_cdp(a, b, first, last)
        integer(ilp), intent(inout) :: a(*) !! reference table to sort
        complex(dp), intent(inout)  :: b(*) !! secondary real data to sort w.r.t. a
        integer(ilp), intent(in)     :: first, last
        integer(ilp)  :: i, j, x, t
        complex(dp) :: d

        x = a( (first+last) / 2 )
        i = first
        j = last
        do
            do while (a(i) < x)
                i=i+1
            end do
            do while (x < a(j))
                j=j-1
            end do
            if (i >= j) exit
            t = a(i);  a(i) = a(j);  a(j) = t
            d = b(i);  b(i) = b(j);  b(j) = d
            i=i+1
            j=j-1
        end do
        if (first < i-1) call quicksort_i_cdp(a, b, first, i-1)
        if (j+1 < last)  call quicksort_i_cdp(a, b, j+1, last)
    end subroutine 


    subroutine sort_coo_unique( a, n, num_rows, num_cols )
        !! Sort a 2d array in increasing order first by index 1 and then by index 2
        integer(ilp), intent(inout) :: a(2,*)
        integer(ilp), intent(inout) :: n
        integer(ilp), intent(in) :: num_rows
        integer(ilp), intent(in) :: num_cols

        integer(ilp) :: stride, adr0, adr1, dd
        integer(ilp) :: n_i, pos, ed
        integer(ilp), allocatable :: count_i(:), count_i_aux(:), rows_(:), cols_(:)
        !---------------------------------------------------------
        ! Sort a first time with respect to first index using count sort
        allocate( count_i( 0:num_rows ) , source = 0 )
        do ed = 1, n
            count_i( a(1,ed) ) = count_i( a(1,ed) ) + 1
        end do
        do n_i = 2, num_rows
            count_i(n_i) = count_i(n_i) + count_i(n_i-1)
        end do
        allocate( count_i_aux( 0:num_rows ) , source = count_i )

        allocate( rows_(n), cols_(n) )
        do ed = n, 1, -1
            n_i = a(1,ed)
            pos = count_i(n_i)
            rows_(pos) = a(1,ed)
            cols_(pos) = a(2,ed)
            count_i(n_i) = count_i(n_i) - 1
        end do
        !---------------------------------------------------------
        ! Sort with respect to second column
        do n_i = 1, num_rows
            adr0 = count_i_aux(n_i-1)+1
            adr1 = count_i_aux(n_i)
            dd = adr1-adr0+1
            if(dd>0) call sort(cols_(adr0:adr1))
        end do
        !---------------------------------------------------------
        ! Remove duplicates
        do ed = 1,n
            a(1:2,ed) = [rows_(ed),cols_(ed)]
        end do
        stride = 0
        do ed = 2, n
            if( a(1,ed) == a(1,ed-1) .and. a(2,ed) == a(2,ed-1) ) then
                stride = stride + 1
            else
                a(1:2,ed-stride) = a(1:2,ed)
            end if
        end do
        n = n - stride
    end subroutine

    subroutine sort_coo_unique_sp( a, data, n, num_rows, num_cols )
        !! Sort a 2d array in increasing order first by index 1 and then by index 2
        real(sp), intent(inout) :: data(*)
        integer(ilp), intent(inout) :: a(2,*)
        integer(ilp), intent(inout) :: n
        integer(ilp), intent(in) :: num_rows
        integer(ilp), intent(in) :: num_cols

        integer(ilp) :: stride, adr0, adr1, dd
        integer(ilp) :: n_i, pos, ed
        integer(ilp), allocatable :: count_i(:), count_i_aux(:), rows_(:), cols_(:)
        real(sp), allocatable :: temp(:)
        !---------------------------------------------------------
        ! Sort a first time with respect to first index using Count sort
        allocate( count_i( 0:num_rows ) , source = 0 )
        do ed = 1, n
            count_i( a(1,ed) ) = count_i( a(1,ed) ) + 1
        end do
        do n_i = 2, num_rows
            count_i(n_i) = count_i(n_i) + count_i(n_i-1)
        end do
        allocate( count_i_aux( 0:num_rows ) , source = count_i )

        allocate( rows_(n), cols_(n), temp(n) )
        do ed = n, 1, -1
            n_i = a(1,ed)
            pos = count_i(n_i)
            rows_(pos) = a(1,ed)
            cols_(pos) = a(2,ed)
            temp(pos)  = data(ed)
            count_i(n_i) = count_i(n_i) - 1
        end do
        !---------------------------------------------------------
        ! Sort with respect to second colum using a quicksort
        do n_i = 1, num_rows
            adr0 = count_i_aux(n_i-1)+1
            adr1 = count_i_aux(n_i)
            dd = adr1-adr0+1
            if(dd>0) call quicksort_i_sp(cols_(adr0),temp(adr0),1,dd)
        end do
        !---------------------------------------------------------
        ! Remove duplicates
        do ed = 1,n
            a(1:2,ed) = [rows_(ed),cols_(ed)]
        end do
        data(1:n) = temp(1:n)
        stride = 0
        do ed = 2, n
            if( a(1,ed) == a(1,ed-1) .and. a(2,ed) == a(2,ed-1) ) then
                data(ed-1-stride) = data(ed-1-stride) + data(ed)
                data(ed) = data(ed-1-stride)
                stride = stride + 1
            else
                a(1:2,ed-stride) = a(1:2,ed)
                data(ed-stride) = data(ed)
            end if
        end do
        n = n - stride
    end subroutine

    subroutine sort_coo_unique_dp( a, data, n, num_rows, num_cols )
        !! Sort a 2d array in increasing order first by index 1 and then by index 2
        real(dp), intent(inout) :: data(*)
        integer(ilp), intent(inout) :: a(2,*)
        integer(ilp), intent(inout) :: n
        integer(ilp), intent(in) :: num_rows
        integer(ilp), intent(in) :: num_cols

        integer(ilp) :: stride, adr0, adr1, dd
        integer(ilp) :: n_i, pos, ed
        integer(ilp), allocatable :: count_i(:), count_i_aux(:), rows_(:), cols_(:)
        real(dp), allocatable :: temp(:)
        !---------------------------------------------------------
        ! Sort a first time with respect to first index using Count sort
        allocate( count_i( 0:num_rows ) , source = 0 )
        do ed = 1, n
            count_i( a(1,ed) ) = count_i( a(1,ed) ) + 1
        end do
        do n_i = 2, num_rows
            count_i(n_i) = count_i(n_i) + count_i(n_i-1)
        end do
        allocate( count_i_aux( 0:num_rows ) , source = count_i )

        allocate( rows_(n), cols_(n), temp(n) )
        do ed = n, 1, -1
            n_i = a(1,ed)
            pos = count_i(n_i)
            rows_(pos) = a(1,ed)
            cols_(pos) = a(2,ed)
            temp(pos)  = data(ed)
            count_i(n_i) = count_i(n_i) - 1
        end do
        !---------------------------------------------------------
        ! Sort with respect to second colum using a quicksort
        do n_i = 1, num_rows
            adr0 = count_i_aux(n_i-1)+1
            adr1 = count_i_aux(n_i)
            dd = adr1-adr0+1
            if(dd>0) call quicksort_i_dp(cols_(adr0),temp(adr0),1,dd)
        end do
        !---------------------------------------------------------
        ! Remove duplicates
        do ed = 1,n
            a(1:2,ed) = [rows_(ed),cols_(ed)]
        end do
        data(1:n) = temp(1:n)
        stride = 0
        do ed = 2, n
            if( a(1,ed) == a(1,ed-1) .and. a(2,ed) == a(2,ed-1) ) then
                data(ed-1-stride) = data(ed-1-stride) + data(ed)
                data(ed) = data(ed-1-stride)
                stride = stride + 1
            else
                a(1:2,ed-stride) = a(1:2,ed)
                data(ed-stride) = data(ed)
            end if
        end do
        n = n - stride
    end subroutine

    subroutine sort_coo_unique_csp( a, data, n, num_rows, num_cols )
        !! Sort a 2d array in increasing order first by index 1 and then by index 2
        complex(sp), intent(inout) :: data(*)
        integer(ilp), intent(inout) :: a(2,*)
        integer(ilp), intent(inout) :: n
        integer(ilp), intent(in) :: num_rows
        integer(ilp), intent(in) :: num_cols

        integer(ilp) :: stride, adr0, adr1, dd
        integer(ilp) :: n_i, pos, ed
        integer(ilp), allocatable :: count_i(:), count_i_aux(:), rows_(:), cols_(:)
        complex(sp), allocatable :: temp(:)
        !---------------------------------------------------------
        ! Sort a first time with respect to first index using Count sort
        allocate( count_i( 0:num_rows ) , source = 0 )
        do ed = 1, n
            count_i( a(1,ed) ) = count_i( a(1,ed) ) + 1
        end do
        do n_i = 2, num_rows
            count_i(n_i) = count_i(n_i) + count_i(n_i-1)
        end do
        allocate( count_i_aux( 0:num_rows ) , source = count_i )

        allocate( rows_(n), cols_(n), temp(n) )
        do ed = n, 1, -1
            n_i = a(1,ed)
            pos = count_i(n_i)
            rows_(pos) = a(1,ed)
            cols_(pos) = a(2,ed)
            temp(pos)  = data(ed)
            count_i(n_i) = count_i(n_i) - 1
        end do
        !---------------------------------------------------------
        ! Sort with respect to second colum using a quicksort
        do n_i = 1, num_rows
            adr0 = count_i_aux(n_i-1)+1
            adr1 = count_i_aux(n_i)
            dd = adr1-adr0+1
            if(dd>0) call quicksort_i_csp(cols_(adr0),temp(adr0),1,dd)
        end do
        !---------------------------------------------------------
        ! Remove duplicates
        do ed = 1,n
            a(1:2,ed) = [rows_(ed),cols_(ed)]
        end do
        data(1:n) = temp(1:n)
        stride = 0
        do ed = 2, n
            if( a(1,ed) == a(1,ed-1) .and. a(2,ed) == a(2,ed-1) ) then
                data(ed-1-stride) = data(ed-1-stride) + data(ed)
                data(ed) = data(ed-1-stride)
                stride = stride + 1
            else
                a(1:2,ed-stride) = a(1:2,ed)
                data(ed-stride) = data(ed)
            end if
        end do
        n = n - stride
    end subroutine

    subroutine sort_coo_unique_cdp( a, data, n, num_rows, num_cols )
        !! Sort a 2d array in increasing order first by index 1 and then by index 2
        complex(dp), intent(inout) :: data(*)
        integer(ilp), intent(inout) :: a(2,*)
        integer(ilp), intent(inout) :: n
        integer(ilp), intent(in) :: num_rows
        integer(ilp), intent(in) :: num_cols

        integer(ilp) :: stride, adr0, adr1, dd
        integer(ilp) :: n_i, pos, ed
        integer(ilp), allocatable :: count_i(:), count_i_aux(:), rows_(:), cols_(:)
        complex(dp), allocatable :: temp(:)
        !---------------------------------------------------------
        ! Sort a first time with respect to first index using Count sort
        allocate( count_i( 0:num_rows ) , source = 0 )
        do ed = 1, n
            count_i( a(1,ed) ) = count_i( a(1,ed) ) + 1
        end do
        do n_i = 2, num_rows
            count_i(n_i) = count_i(n_i) + count_i(n_i-1)
        end do
        allocate( count_i_aux( 0:num_rows ) , source = count_i )

        allocate( rows_(n), cols_(n), temp(n) )
        do ed = n, 1, -1
            n_i = a(1,ed)
            pos = count_i(n_i)
            rows_(pos) = a(1,ed)
            cols_(pos) = a(2,ed)
            temp(pos)  = data(ed)
            count_i(n_i) = count_i(n_i) - 1
        end do
        !---------------------------------------------------------
        ! Sort with respect to second colum using a quicksort
        do n_i = 1, num_rows
            adr0 = count_i_aux(n_i-1)+1
            adr1 = count_i_aux(n_i)
            dd = adr1-adr0+1
            if(dd>0) call quicksort_i_cdp(cols_(adr0),temp(adr0),1,dd)
        end do
        !---------------------------------------------------------
        ! Remove duplicates
        do ed = 1,n
            a(1:2,ed) = [rows_(ed),cols_(ed)]
        end do
        data(1:n) = temp(1:n)
        stride = 0
        do ed = 2, n
            if( a(1,ed) == a(1,ed-1) .and. a(2,ed) == a(2,ed-1) ) then
                data(ed-1-stride) = data(ed-1-stride) + data(ed)
                data(ed) = data(ed-1-stride)
                stride = stride + 1
            else
                a(1:2,ed-stride) = a(1:2,ed)
                data(ed-stride) = data(ed)
            end if
        end do
        n = n - stride
    end subroutine


    !! version: experimental
    !!
    !! Transform COO matrix to canonical form with ordered and unique entries
    !! [Specifications](../page/specs/stdlib_sparse.html#sparse_conversion)
    subroutine coo2ordered(COO,sort_data)
        class(COO_type), intent(inout) :: COO
        logical, intent(in), optional :: sort_data
        integer(ilp), allocatable :: itemp(:,:)
        logical :: sort_data_
        
        if(COO%is_sorted) return

        sort_data_ = .false.
        if(present(sort_data)) sort_data_ = sort_data

        select type (COO)
            type is( COO_type )
                call sort_coo(COO%index, COO%nnz, COO%nrows, COO%ncols)
            type is( COO_sp_type )
                block
                real(sp), allocatable :: temp(:)
                if( sort_data_ ) then
                    call sort_coo(COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) , source=COO%data(1:COO%nnz) )
                else 
                    call sort_coo(COO%index, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) )
                end if
                call move_alloc( temp , COO%data )
                end block
            type is( COO_dp_type )
                block
                real(dp), allocatable :: temp(:)
                if( sort_data_ ) then
                    call sort_coo(COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) , source=COO%data(1:COO%nnz) )
                else 
                    call sort_coo(COO%index, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) )
                end if
                call move_alloc( temp , COO%data )
                end block
            type is( COO_csp_type )
                block
                complex(sp), allocatable :: temp(:)
                if( sort_data_ ) then
                    call sort_coo(COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) , source=COO%data(1:COO%nnz) )
                else 
                    call sort_coo(COO%index, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) )
                end if
                call move_alloc( temp , COO%data )
                end block
            type is( COO_cdp_type )
                block
                complex(dp), allocatable :: temp(:)
                if( sort_data_ ) then
                    call sort_coo(COO%index, COO%data, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) , source=COO%data(1:COO%nnz) )
                else 
                    call sort_coo(COO%index, COO%nnz, COO%nrows, COO%ncols)
                    allocate( temp(COO%nnz) )
                end if
                call move_alloc( temp , COO%data )
                end block
        end select
        
        allocate( itemp(2,COO%nnz) , source=COO%index(1:2,1:COO%nnz) )
        call move_alloc( itemp , COO%index )

        COO%is_sorted = .true.
    end subroutine

    subroutine coo_from_ijv_type(COO,row,col,nrows,ncols)
        type(COO_type), intent(inout) :: COO
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_, nnz, ed
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = size(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = size(col)
        end if
        nnz = size(row)
        !---------------------------------------------------------
        call COO%malloc(nrows_,ncols_,nnz)
        do ed = 1, nnz 
            COO%index(1:2,ed) = [row(ed),col(ed)]
        end do

        call coo2ordered(COO,.true.)
    end subroutine

    subroutine coo_from_ijv_sp(COO,row,col,data,nrows,ncols)
        type(COO_sp_type), intent(inout) :: COO
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_, nnz, ed
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        nnz = size(row)
        !---------------------------------------------------------
        call COO%malloc(nrows_,ncols_,nnz)
        do ed = 1, nnz 
            COO%index(1:2,ed) = [row(ed),col(ed)]
        end do
        if(present(data)) COO%data = data

        call coo2ordered(COO,.true.)
    end subroutine
    subroutine coo_from_ijv_dp(COO,row,col,data,nrows,ncols)
        type(COO_dp_type), intent(inout) :: COO
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_, nnz, ed
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        nnz = size(row)
        !---------------------------------------------------------
        call COO%malloc(nrows_,ncols_,nnz)
        do ed = 1, nnz 
            COO%index(1:2,ed) = [row(ed),col(ed)]
        end do
        if(present(data)) COO%data = data

        call coo2ordered(COO,.true.)
    end subroutine
    subroutine coo_from_ijv_csp(COO,row,col,data,nrows,ncols)
        type(COO_csp_type), intent(inout) :: COO
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_, nnz, ed
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        nnz = size(row)
        !---------------------------------------------------------
        call COO%malloc(nrows_,ncols_,nnz)
        do ed = 1, nnz 
            COO%index(1:2,ed) = [row(ed),col(ed)]
        end do
        if(present(data)) COO%data = data

        call coo2ordered(COO,.true.)
    end subroutine
    subroutine coo_from_ijv_cdp(COO,row,col,data,nrows,ncols)
        type(COO_cdp_type), intent(inout) :: COO
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_, nnz, ed
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        nnz = size(row)
        !---------------------------------------------------------
        call COO%malloc(nrows_,ncols_,nnz)
        do ed = 1, nnz 
            COO%index(1:2,ed) = [row(ed),col(ed)]
        end do
        if(present(data)) COO%data = data

        call coo2ordered(COO,.true.)
    end subroutine

    subroutine csr_from_ijv_sp(CSR,row,col,data,nrows,ncols)
        type(CSR_sp_type), intent(inout) :: CSR
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_sp_type) :: COO
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
        end block
    end subroutine
    subroutine csr_from_ijv_dp(CSR,row,col,data,nrows,ncols)
        type(CSR_dp_type), intent(inout) :: CSR
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_dp_type) :: COO
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
        end block
    end subroutine
    subroutine csr_from_ijv_csp(CSR,row,col,data,nrows,ncols)
        type(CSR_csp_type), intent(inout) :: CSR
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_csp_type) :: COO
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
        end block
    end subroutine
    subroutine csr_from_ijv_cdp(CSR,row,col,data,nrows,ncols)
        type(CSR_cdp_type), intent(inout) :: CSR
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_cdp_type) :: COO
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
        end block
    end subroutine

    subroutine ell_from_ijv_sp(ELL,row,col,data,nrows,ncols,num_nz_rows)
        type(ELL_sp_type), intent(inout) :: ELL
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: num_nz_rows

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_sp_type) :: COO
            type(CSR_sp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            if(present(num_nz_rows)) then
                call csr2ell(CSR,ELL,num_nz_rows)
            else 
                call csr2ell(CSR,ELL)
            end if
        end block
    end subroutine
    subroutine ell_from_ijv_dp(ELL,row,col,data,nrows,ncols,num_nz_rows)
        type(ELL_dp_type), intent(inout) :: ELL
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: num_nz_rows

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_dp_type) :: COO
            type(CSR_dp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            if(present(num_nz_rows)) then
                call csr2ell(CSR,ELL,num_nz_rows)
            else 
                call csr2ell(CSR,ELL)
            end if
        end block
    end subroutine
    subroutine ell_from_ijv_csp(ELL,row,col,data,nrows,ncols,num_nz_rows)
        type(ELL_csp_type), intent(inout) :: ELL
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: num_nz_rows

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_csp_type) :: COO
            type(CSR_csp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            if(present(num_nz_rows)) then
                call csr2ell(CSR,ELL,num_nz_rows)
            else 
                call csr2ell(CSR,ELL)
            end if
        end block
    end subroutine
    subroutine ell_from_ijv_cdp(ELL,row,col,data,nrows,ncols,num_nz_rows)
        type(ELL_cdp_type), intent(inout) :: ELL
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: num_nz_rows

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        !---------------------------------------------------------
        block
            type(COO_cdp_type) :: COO
            type(CSR_cdp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            if(present(num_nz_rows)) then
                call csr2ell(CSR,ELL,num_nz_rows)
            else 
                call csr2ell(CSR,ELL)
            end if
        end block
    end subroutine

    subroutine sellc_from_ijv_sp(SELLC,row,col,data,nrows,ncols,chunk)
        type(SELLC_sp_type), intent(inout) :: SELLC
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: chunk

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        if(present(chunk)) SELLC%chunk_size = chunk
        !---------------------------------------------------------
        block
            type(COO_sp_type) :: COO
            type(CSR_sp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            call csr2sellc(CSR,SELLC)
        end block
    end subroutine
    subroutine sellc_from_ijv_dp(SELLC,row,col,data,nrows,ncols,chunk)
        type(SELLC_dp_type), intent(inout) :: SELLC
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        real(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: chunk

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        if(present(chunk)) SELLC%chunk_size = chunk
        !---------------------------------------------------------
        block
            type(COO_dp_type) :: COO
            type(CSR_dp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            call csr2sellc(CSR,SELLC)
        end block
    end subroutine
    subroutine sellc_from_ijv_csp(SELLC,row,col,data,nrows,ncols,chunk)
        type(SELLC_csp_type), intent(inout) :: SELLC
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(sp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: chunk

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        if(present(chunk)) SELLC%chunk_size = chunk
        !---------------------------------------------------------
        block
            type(COO_csp_type) :: COO
            type(CSR_csp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            call csr2sellc(CSR,SELLC)
        end block
    end subroutine
    subroutine sellc_from_ijv_cdp(SELLC,row,col,data,nrows,ncols,chunk)
        type(SELLC_cdp_type), intent(inout) :: SELLC
        integer(ilp), intent(in) :: row(:)
        integer(ilp), intent(in) :: col(:)
        complex(dp), intent(in), optional :: data(:)
        integer(ilp), intent(in), optional :: nrows
        integer(ilp), intent(in), optional :: ncols
        integer, intent(in), optional :: chunk

        integer(ilp) :: nrows_, ncols_
        !---------------------------------------------------------
        if(present(nrows)) then
            nrows_ = nrows
        else 
            nrows_ = maxval(row)
        end if
        if(present(ncols)) then
            ncols_ = ncols
        else 
            ncols_ = maxval(col)
        end if
        if(present(chunk)) SELLC%chunk_size = chunk
        !---------------------------------------------------------
        block
            type(COO_cdp_type) :: COO
            type(CSR_cdp_type) :: CSR
            if(present(data)) then
                call from_ijv(COO,row,col,data=data,nrows=nrows_,ncols=ncols_)
            else 
                call from_ijv(COO,row,col,nrows=nrows_,ncols=ncols_)
            end if
            call coo2csr(COO,CSR)
            call csr2sellc(CSR,SELLC)
        end block
    end subroutine

    !! Diagonal extraction

    subroutine dense2diagonal_sp(dense,diagonal)
        real(sp), intent(in) :: dense(:,:)
        real(sp), intent(inout), allocatable :: diagonal(:)
        integer :: num_rows
        integer :: i

        num_rows = size(dense,dim=1)
        if(.not.allocated(diagonal)) allocate(diagonal(num_rows))

        do i = 1, num_rows
            diagonal(i) = dense(i,i)
        end do
    end subroutine

    subroutine dense2diagonal_dp(dense,diagonal)
        real(dp), intent(in) :: dense(:,:)
        real(dp), intent(inout), allocatable :: diagonal(:)
        integer :: num_rows
        integer :: i

        num_rows = size(dense,dim=1)
        if(.not.allocated(diagonal)) allocate(diagonal(num_rows))

        do i = 1, num_rows
            diagonal(i) = dense(i,i)
        end do
    end subroutine

    subroutine dense2diagonal_csp(dense,diagonal)
        complex(sp), intent(in) :: dense(:,:)
        complex(sp), intent(inout), allocatable :: diagonal(:)
        integer :: num_rows
        integer :: i

        num_rows = size(dense,dim=1)
        if(.not.allocated(diagonal)) allocate(diagonal(num_rows))

        do i = 1, num_rows
            diagonal(i) = dense(i,i)
        end do
    end subroutine

    subroutine dense2diagonal_cdp(dense,diagonal)
        complex(dp), intent(in) :: dense(:,:)
        complex(dp), intent(inout), allocatable :: diagonal(:)
        integer :: num_rows
        integer :: i

        num_rows = size(dense,dim=1)
        if(.not.allocated(diagonal)) allocate(diagonal(num_rows))

        do i = 1, num_rows
            diagonal(i) = dense(i,i)
        end do
    end subroutine


    subroutine coo2diagonal_sp(COO,diagonal)
        type(COO_sp_type), intent(in) :: COO
        real(sp), intent(inout), allocatable :: diagonal(:)
        integer :: idx

        if(.not.allocated(diagonal)) allocate(diagonal(COO%nrows))

        do concurrent(idx = 1:COO%nnz)
            if(COO%index(1,idx)==COO%index(2,idx)) &
            & diagonal( COO%index(1,idx) ) = COO%data(idx)
        end do
    end subroutine

    subroutine coo2diagonal_dp(COO,diagonal)
        type(COO_dp_type), intent(in) :: COO
        real(dp), intent(inout), allocatable :: diagonal(:)
        integer :: idx

        if(.not.allocated(diagonal)) allocate(diagonal(COO%nrows))

        do concurrent(idx = 1:COO%nnz)
            if(COO%index(1,idx)==COO%index(2,idx)) &
            & diagonal( COO%index(1,idx) ) = COO%data(idx)
        end do
    end subroutine

    subroutine coo2diagonal_csp(COO,diagonal)
        type(COO_csp_type), intent(in) :: COO
        complex(sp), intent(inout), allocatable :: diagonal(:)
        integer :: idx

        if(.not.allocated(diagonal)) allocate(diagonal(COO%nrows))

        do concurrent(idx = 1:COO%nnz)
            if(COO%index(1,idx)==COO%index(2,idx)) &
            & diagonal( COO%index(1,idx) ) = COO%data(idx)
        end do
    end subroutine

    subroutine coo2diagonal_cdp(COO,diagonal)
        type(COO_cdp_type), intent(in) :: COO
        complex(dp), intent(inout), allocatable :: diagonal(:)
        integer :: idx

        if(.not.allocated(diagonal)) allocate(diagonal(COO%nrows))

        do concurrent(idx = 1:COO%nnz)
            if(COO%index(1,idx)==COO%index(2,idx)) &
            & diagonal( COO%index(1,idx) ) = COO%data(idx)
        end do
    end subroutine


    subroutine csr2diagonal_sp(CSR,diagonal)
        type(CSR_sp_type), intent(in) :: CSR
        real(sp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSR%nrows))

        select case(CSR%storage)
        case(sparse_lower)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    if( CSR%col(j) == i ) then
                        diagonal(i) = CSR%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine

    subroutine csr2diagonal_dp(CSR,diagonal)
        type(CSR_dp_type), intent(in) :: CSR
        real(dp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSR%nrows))

        select case(CSR%storage)
        case(sparse_lower)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    if( CSR%col(j) == i ) then
                        diagonal(i) = CSR%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine

    subroutine csr2diagonal_csp(CSR,diagonal)
        type(CSR_csp_type), intent(in) :: CSR
        complex(sp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSR%nrows))

        select case(CSR%storage)
        case(sparse_lower)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    if( CSR%col(j) == i ) then
                        diagonal(i) = CSR%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine

    subroutine csr2diagonal_cdp(CSR,diagonal)
        type(CSR_cdp_type), intent(in) :: CSR
        complex(dp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSR%nrows))

        select case(CSR%storage)
        case(sparse_lower)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSR%nrows
                diagonal(i) = CSR%data( CSR%rowptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSR%nrows
                do j = CSR%rowptr(i), CSR%rowptr(i+1)-1
                    if( CSR%col(j) == i ) then
                        diagonal(i) = CSR%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine


    subroutine csc2diagonal_sp(CSC,diagonal)
        type(CSC_sp_type), intent(in) :: CSC
        real(sp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSC%nrows))

        select case(CSC%storage)
        case(sparse_lower)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSC%ncols
                do j = CSC%colptr(i), CSC%colptr(i+1)-1
                    if( CSC%row(j) == i ) then
                        diagonal(i) = CSC%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine

    subroutine csc2diagonal_dp(CSC,diagonal)
        type(CSC_dp_type), intent(in) :: CSC
        real(dp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSC%nrows))

        select case(CSC%storage)
        case(sparse_lower)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSC%ncols
                do j = CSC%colptr(i), CSC%colptr(i+1)-1
                    if( CSC%row(j) == i ) then
                        diagonal(i) = CSC%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine

    subroutine csc2diagonal_csp(CSC,diagonal)
        type(CSC_csp_type), intent(in) :: CSC
        complex(sp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSC%nrows))

        select case(CSC%storage)
        case(sparse_lower)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSC%ncols
                do j = CSC%colptr(i), CSC%colptr(i+1)-1
                    if( CSC%row(j) == i ) then
                        diagonal(i) = CSC%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine

    subroutine csc2diagonal_cdp(CSC,diagonal)
        type(CSC_cdp_type), intent(in) :: CSC
        complex(dp), intent(inout), allocatable :: diagonal(:)
        integer :: i, j

        if(.not.allocated(diagonal)) allocate(diagonal(CSC%nrows))

        select case(CSC%storage)
        case(sparse_lower)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i+1)-1 )
            end do
        case(sparse_upper)
            do i = 1, CSC%ncols
                diagonal(i) = CSC%data( CSC%colptr(i) )
            end do
        case(sparse_full)
            do i = 1, CSC%ncols
                do j = CSC%colptr(i), CSC%colptr(i+1)-1
                    if( CSC%row(j) == i ) then
                        diagonal(i) = CSC%data(j)
                        exit
                    end if
                end do
            end do
        end select
    end subroutine


    subroutine ell2diagonal_sp(ELL,diagonal)
        type(ELL_sp_type), intent(in) :: ELL
        real(sp), intent(inout), allocatable :: diagonal(:)
        integer :: i, k

        if(.not.allocated(diagonal)) allocate(diagonal(ELL%nrows))
        if( ELL%storage == sparse_full) then
            do i = 1, ELL%nrows
                do k = 1, ELL%K
                    if(ELL%index(i,k)==i) diagonal(i) = ELL%data(i,k)
                end do
            end do
        end if
    end subroutine

    subroutine ell2diagonal_dp(ELL,diagonal)
        type(ELL_dp_type), intent(in) :: ELL
        real(dp), intent(inout), allocatable :: diagonal(:)
        integer :: i, k

        if(.not.allocated(diagonal)) allocate(diagonal(ELL%nrows))
        if( ELL%storage == sparse_full) then
            do i = 1, ELL%nrows
                do k = 1, ELL%K
                    if(ELL%index(i,k)==i) diagonal(i) = ELL%data(i,k)
                end do
            end do
        end if
    end subroutine

    subroutine ell2diagonal_csp(ELL,diagonal)
        type(ELL_csp_type), intent(in) :: ELL
        complex(sp), intent(inout), allocatable :: diagonal(:)
        integer :: i, k

        if(.not.allocated(diagonal)) allocate(diagonal(ELL%nrows))
        if( ELL%storage == sparse_full) then
            do i = 1, ELL%nrows
                do k = 1, ELL%K
                    if(ELL%index(i,k)==i) diagonal(i) = ELL%data(i,k)
                end do
            end do
        end if
    end subroutine

    subroutine ell2diagonal_cdp(ELL,diagonal)
        type(ELL_cdp_type), intent(in) :: ELL
        complex(dp), intent(inout), allocatable :: diagonal(:)
        integer :: i, k

        if(.not.allocated(diagonal)) allocate(diagonal(ELL%nrows))
        if( ELL%storage == sparse_full) then
            do i = 1, ELL%nrows
                do k = 1, ELL%K
                    if(ELL%index(i,k)==i) diagonal(i) = ELL%data(i,k)
                end do
            end do
        end if
    end subroutine


end module