!! The `stdlib_sparse_kinds` module provides derived type definitions for different sparse matrices
!!
! This code was modified from https://github.com/jalvesz/FSPARSE by its author: Alves Jose
module stdlib_sparse_kinds
    use ieee_arithmetic
    use stdlib_sparse_constants
    implicit none
    private
    public :: sparse_full, sparse_lower, sparse_upper
    public :: sparse_op_none, sparse_op_transpose, sparse_op_hermitian
    !! version: experimental
    !!
    !! Base sparse type holding the meta data related to the storage capacity of a matrix.
    type, public, abstract :: sparse_type
      integer(ilp) :: nrows = 0 !! number of rows
      integer(ilp) :: ncols = 0 !! number of columns
      integer(ilp) :: nnz   = 0 !! number of non-zero values
      integer :: storage = sparse_full !! assumed storage symmetry
    end type

    !! version: experimental
    !!
    !! COO: COOrdinates compresed format 
    type, public, extends(sparse_type) :: COO_type
      logical               :: is_sorted = .false. !! whether the matrix is sorted or not
      integer(ilp), allocatable  :: index(:,:) !! Matrix coordinates index(2,nnz)
    contains
      procedure :: malloc => malloc_coo
    end type

    type, public, extends(COO_type) :: COO_sp_type
        real(sp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_coo_sp
        procedure, non_overridable :: add_value => add_value_coo_sp
        procedure, non_overridable :: add_block => add_block_coo_sp
        generic :: add => add_value, add_block
    end type
    type, public, extends(COO_type) :: COO_dp_type
        real(dp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_coo_dp
        procedure, non_overridable :: add_value => add_value_coo_dp
        procedure, non_overridable :: add_block => add_block_coo_dp
        generic :: add => add_value, add_block
    end type
    type, public, extends(COO_type) :: COO_csp_type
        complex(sp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_coo_csp
        procedure, non_overridable :: add_value => add_value_coo_csp
        procedure, non_overridable :: add_block => add_block_coo_csp
        generic :: add => add_value, add_block
    end type
    type, public, extends(COO_type) :: COO_cdp_type
        complex(dp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_coo_cdp
        procedure, non_overridable :: add_value => add_value_coo_cdp
        procedure, non_overridable :: add_block => add_block_coo_cdp
        generic :: add => add_value, add_block
    end type

    !! version: experimental
    !!
    !! CSR: Compressed sparse row or Yale format
    type, public, extends(sparse_type) :: CSR_type  
      integer(ilp), allocatable  :: col(:)    !! matrix column pointer
      integer(ilp), allocatable  :: rowptr(:) !! matrix row pointer
    contains
      procedure :: malloc => malloc_csr
    end type
  
    type, public, extends(CSR_type) :: CSR_sp_type
        real(sp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csr_sp
        procedure, non_overridable :: add_value => add_value_csr_sp
        procedure, non_overridable :: add_block => add_block_csr_sp
        generic :: add => add_value, add_block
    end type
    type, public, extends(CSR_type) :: CSR_dp_type
        real(dp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csr_dp
        procedure, non_overridable :: add_value => add_value_csr_dp
        procedure, non_overridable :: add_block => add_block_csr_dp
        generic :: add => add_value, add_block
    end type
    type, public, extends(CSR_type) :: CSR_csp_type
        complex(sp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csr_csp
        procedure, non_overridable :: add_value => add_value_csr_csp
        procedure, non_overridable :: add_block => add_block_csr_csp
        generic :: add => add_value, add_block
    end type
    type, public, extends(CSR_type) :: CSR_cdp_type
        complex(dp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csr_cdp
        procedure, non_overridable :: add_value => add_value_csr_cdp
        procedure, non_overridable :: add_block => add_block_csr_cdp
        generic :: add => add_value, add_block
    end type

    !! version: experimental
    !!
    !! CSC: Compressed sparse column
    type, public, extends(sparse_type) :: CSC_type  
      integer(ilp), allocatable  :: colptr(:) !! matrix column pointer
      integer(ilp), allocatable  :: row(:)    !! matrix row pointer
    contains
      procedure :: malloc => malloc_csc
    end type
  
    type, public, extends(CSC_type) :: CSC_sp_type
        real(sp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csc_sp
        procedure, non_overridable :: add_value => add_value_csc_sp
        procedure, non_overridable :: add_block => add_block_csc_sp
        generic :: add => add_value, add_block
    end type
    type, public, extends(CSC_type) :: CSC_dp_type
        real(dp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csc_dp
        procedure, non_overridable :: add_value => add_value_csc_dp
        procedure, non_overridable :: add_block => add_block_csc_dp
        generic :: add => add_value, add_block
    end type
    type, public, extends(CSC_type) :: CSC_csp_type
        complex(sp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csc_csp
        procedure, non_overridable :: add_value => add_value_csc_csp
        procedure, non_overridable :: add_block => add_block_csc_csp
        generic :: add => add_value, add_block
    end type
    type, public, extends(CSC_type) :: CSC_cdp_type
        complex(dp), allocatable :: data(:) 
    contains
        procedure, non_overridable :: at => at_value_csc_cdp
        procedure, non_overridable :: add_value => add_value_csc_cdp
        procedure, non_overridable :: add_block => add_block_csc_cdp
        generic :: add => add_value, add_block
    end type

    !! version: experimental
    !!
    !! Compressed ELLPACK
    type, public, extends(sparse_type) :: ELL_type 
      integer               :: K = 0 !! maximum number of nonzeros per row
      integer(ilp), allocatable :: index(:,:) !! column indices
    contains
      procedure :: malloc => malloc_ell
    end type
  
    type, public, extends(ELL_type) :: ELL_sp_type
        real(sp), allocatable :: data(:,:) 
    contains
        procedure, non_overridable :: at => at_value_ell_sp
        procedure, non_overridable :: add_value => add_value_ell_sp
        procedure, non_overridable :: add_block => add_block_ell_sp
        generic :: add => add_value, add_block
    end type
    type, public, extends(ELL_type) :: ELL_dp_type
        real(dp), allocatable :: data(:,:) 
    contains
        procedure, non_overridable :: at => at_value_ell_dp
        procedure, non_overridable :: add_value => add_value_ell_dp
        procedure, non_overridable :: add_block => add_block_ell_dp
        generic :: add => add_value, add_block
    end type
    type, public, extends(ELL_type) :: ELL_csp_type
        complex(sp), allocatable :: data(:,:) 
    contains
        procedure, non_overridable :: at => at_value_ell_csp
        procedure, non_overridable :: add_value => add_value_ell_csp
        procedure, non_overridable :: add_block => add_block_ell_csp
        generic :: add => add_value, add_block
    end type
    type, public, extends(ELL_type) :: ELL_cdp_type
        complex(dp), allocatable :: data(:,:) 
    contains
        procedure, non_overridable :: at => at_value_ell_cdp
        procedure, non_overridable :: add_value => add_value_ell_cdp
        procedure, non_overridable :: add_block => add_block_ell_cdp
        generic :: add => add_value, add_block
    end type

    !! version: experimental
    !!
    !! Compressed SELL-C 
    !! Reference : https://library.eecs.utk.edu/storage/files/ut-eecs-14-727.pdf
    type, public, extends(sparse_type) :: SELLC_type 
      integer               :: chunk_size = 8 !!  default chunk size
      integer(ilp), allocatable  :: rowptr(:) !! row pointer
      integer(ilp), allocatable  :: col(:,:)  !! column indices
    end type
  
    type, public, extends(SELLC_type) :: SELLC_sp_type
        real(sp), allocatable :: data(:,:)
    contains
        procedure, non_overridable :: at => at_value_sellc_sp
        procedure, non_overridable :: add_value => add_value_sellc_sp
        procedure, non_overridable :: add_block => add_block_sellc_sp
        generic :: add => add_value, add_block
    end type
    type, public, extends(SELLC_type) :: SELLC_dp_type
        real(dp), allocatable :: data(:,:)
    contains
        procedure, non_overridable :: at => at_value_sellc_dp
        procedure, non_overridable :: add_value => add_value_sellc_dp
        procedure, non_overridable :: add_block => add_block_sellc_dp
        generic :: add => add_value, add_block
    end type
    type, public, extends(SELLC_type) :: SELLC_csp_type
        complex(sp), allocatable :: data(:,:)
    contains
        procedure, non_overridable :: at => at_value_sellc_csp
        procedure, non_overridable :: add_value => add_value_sellc_csp
        procedure, non_overridable :: add_block => add_block_sellc_csp
        generic :: add => add_value, add_block
    end type
    type, public, extends(SELLC_type) :: SELLC_cdp_type
        complex(dp), allocatable :: data(:,:)
    contains
        procedure, non_overridable :: at => at_value_sellc_cdp
        procedure, non_overridable :: add_value => add_value_sellc_cdp
        procedure, non_overridable :: add_block => add_block_sellc_cdp
        generic :: add => add_value, add_block
    end type

contains

    !! (re)Allocate matrix memory for the COO type
    subroutine malloc_coo(self,num_rows,num_cols,nnz)
        class(COO_type) :: self
        integer(ilp), intent(in) :: num_rows !! number of rows
        integer(ilp), intent(in) :: num_cols !! number of columns
        integer(ilp), intent(in) :: nnz      !! number of non zeros

        integer(ilp),  allocatable :: temp_idx(:,:)
        !-----------------------------------------------------

        self%nrows = num_rows
        self%ncols = num_cols
        self%nnz   = nnz

        if(.not.allocated(self%index)) then
            allocate(temp_idx(2,nnz) , source = 0 )
        else
            allocate(temp_idx(2,nnz) , source = self%index )
        end if
        call move_alloc(from=temp_idx,to=self%index)

        select type(self)
            type is(COO_sp_type)
                block
                real(sp), allocatable :: temp_data_sp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_sp(nnz) , source = zero_sp )
                else
                    allocate(temp_data_sp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_sp,to=self%data)
                end block
            type is(COO_dp_type)
                block
                real(dp), allocatable :: temp_data_dp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_dp(nnz) , source = zero_dp )
                else
                    allocate(temp_data_dp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_dp,to=self%data)
                end block
            type is(COO_csp_type)
                block
                complex(sp), allocatable :: temp_data_csp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_csp(nnz) , source = zero_csp )
                else
                    allocate(temp_data_csp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_csp,to=self%data)
                end block
            type is(COO_cdp_type)
                block
                complex(dp), allocatable :: temp_data_cdp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_cdp(nnz) , source = zero_cdp )
                else
                    allocate(temp_data_cdp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_cdp,to=self%data)
                end block
        end select
    end subroutine

    !! (re)Allocate matrix memory for the CSR type
    subroutine malloc_csr(self,num_rows,num_cols,nnz)
        class(CSR_type) :: self
        integer(ilp), intent(in) :: num_rows !! number of rows
        integer(ilp), intent(in) :: num_cols !! number of columns
        integer(ilp), intent(in) :: nnz      !! number of non zeros

        integer(ilp),  allocatable :: temp_idx(:)
        !-----------------------------------------------------

        self%nrows = num_rows
        self%ncols = num_cols
        self%nnz   = nnz

        if(.not.allocated(self%col)) then
            allocate(temp_idx(nnz) , source = 0 )
        else
            allocate(temp_idx(nnz) , source = self%col )
        end if
        call move_alloc(from=temp_idx,to=self%col)

        if(.not.allocated(self%rowptr)) then
            allocate(temp_idx(num_rows+1) , source = 0 )
        else
            allocate(temp_idx(num_rows+1) , source = self%rowptr )
        end if
        call move_alloc(from=temp_idx,to=self%rowptr)

        select type(self)
            type is(CSR_sp_type)
                block
                real(sp), allocatable :: temp_data_sp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_sp(nnz) , source = zero_sp )
                else
                    allocate(temp_data_sp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_sp,to=self%data)
                end block
            type is(CSR_dp_type)
                block
                real(dp), allocatable :: temp_data_dp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_dp(nnz) , source = zero_dp )
                else
                    allocate(temp_data_dp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_dp,to=self%data)
                end block
            type is(CSR_csp_type)
                block
                complex(sp), allocatable :: temp_data_csp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_csp(nnz) , source = zero_csp )
                else
                    allocate(temp_data_csp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_csp,to=self%data)
                end block
            type is(CSR_cdp_type)
                block
                complex(dp), allocatable :: temp_data_cdp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_cdp(nnz) , source = zero_cdp )
                else
                    allocate(temp_data_cdp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_cdp,to=self%data)
                end block
        end select
    end subroutine

    !! (re)Allocate matrix memory for the CSC type
    subroutine malloc_csc(self,num_rows,num_cols,nnz)
        class(CSC_type) :: self
        integer(ilp), intent(in) :: num_rows !! number of rows
        integer(ilp), intent(in) :: num_cols !! number of columns
        integer(ilp), intent(in) :: nnz      !! number of non zeros

        integer(ilp),  allocatable :: temp_idx(:)
        !-----------------------------------------------------

        self%nrows = num_rows
        self%ncols = num_cols
        self%nnz   = nnz

        if(.not.allocated(self%row)) then
            allocate(temp_idx(nnz) , source = 0 )
        else
            allocate(temp_idx(nnz) , source = self%row )
        end if
        call move_alloc(from=temp_idx,to=self%row)

        if(.not.allocated(self%colptr)) then
            allocate(temp_idx(num_cols+1) , source = 0 )
        else
            allocate(temp_idx(num_cols+1) , source = self%colptr )
        end if
        call move_alloc(from=temp_idx,to=self%colptr)

        select type(self)
            type is(CSC_sp_type)
                block
                real(sp), allocatable :: temp_data_sp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_sp(nnz) , source = zero_sp )
                else
                    allocate(temp_data_sp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_sp,to=self%data)
                end block
            type is(CSC_dp_type)
                block
                real(dp), allocatable :: temp_data_dp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_dp(nnz) , source = zero_dp )
                else
                    allocate(temp_data_dp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_dp,to=self%data)
                end block
            type is(CSC_csp_type)
                block
                complex(sp), allocatable :: temp_data_csp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_csp(nnz) , source = zero_csp )
                else
                    allocate(temp_data_csp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_csp,to=self%data)
                end block
            type is(CSC_cdp_type)
                block
                complex(dp), allocatable :: temp_data_cdp(:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_cdp(nnz) , source = zero_cdp )
                else
                    allocate(temp_data_cdp(nnz) , source = self%data )
                end if
                call move_alloc(from=temp_data_cdp,to=self%data)
                end block
        end select
    end subroutine

    !! (re)Allocate matrix memory for the ELLPACK type
    subroutine malloc_ell(self,num_rows,num_cols,num_nz_rows)
        class(ELL_type) :: self
        integer(ilp), intent(in) :: num_rows    !! number of rows
        integer(ilp), intent(in) :: num_cols    !! number of columns
        integer(ilp), intent(in) :: num_nz_rows !! number of non zeros per row

        integer(ilp),  allocatable :: temp_idx(:,:)
        !-----------------------------------------------------

        self%nrows = num_rows
        self%ncols = num_cols
        self%K     = num_nz_rows

        if(.not.allocated(self%index)) then
            allocate(temp_idx(num_rows,num_nz_rows) , source = 0 )
        else
            allocate(temp_idx(num_rows,num_nz_rows) , source = self%index )
        end if
        call move_alloc(from=temp_idx,to=self%index)

        select type(self)
            type is(ELL_sp_type)
                block
                real(sp), allocatable :: temp_data_sp(:,:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_sp(num_rows,num_nz_rows) , source = zero_sp )
                else
                    allocate(temp_data_sp(num_rows,num_nz_rows) , source = self%data )
                end if
                call move_alloc(from=temp_data_sp,to=self%data)
                end block
            type is(ELL_dp_type)
                block
                real(dp), allocatable :: temp_data_dp(:,:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_dp(num_rows,num_nz_rows) , source = zero_dp )
                else
                    allocate(temp_data_dp(num_rows,num_nz_rows) , source = self%data )
                end if
                call move_alloc(from=temp_data_dp,to=self%data)
                end block
            type is(ELL_csp_type)
                block
                complex(sp), allocatable :: temp_data_csp(:,:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_csp(num_rows,num_nz_rows) , source = zero_csp )
                else
                    allocate(temp_data_csp(num_rows,num_nz_rows) , source = self%data )
                end if
                call move_alloc(from=temp_data_csp,to=self%data)
                end block
            type is(ELL_cdp_type)
                block
                complex(dp), allocatable :: temp_data_cdp(:,:)
                if(.not.allocated(self%data)) then
                    allocate(temp_data_cdp(num_rows,num_nz_rows) , source = zero_cdp )
                else
                    allocate(temp_data_cdp(num_rows,num_nz_rows) , source = self%data )
                end if
                call move_alloc(from=temp_data_cdp,to=self%data)
                end block
        end select
    end subroutine

    !==================================================================
    ! data accessors
    !==================================================================

    pure real(sp) function at_value_coo_sp(self,ik,jk) result(val)
        class(COO_sp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1, self%nnz
            if( ik_ == self%index(1,k) .and. jk_ == self%index(2,k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_sp
    end function

    subroutine add_value_coo_sp(self,ik,jk,val)
        class(COO_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1,self%nnz
            if( ik == self%index(1,k) .and. jk == self%index(2,k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_coo_sp(self,ik,jk,val)
        class(COO_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1, self%nnz
            do i = 1, size(ik)
                if( ik(i) /= self%index(1,k) ) cycle
                do j = 1, size(jk)
                    if( jk(j) /= self%index(2,k) ) cycle
                    self%data(k) = self%data(k) + val(i,j)
                end do
            end do
        end do
    end subroutine

    pure real(dp) function at_value_coo_dp(self,ik,jk) result(val)
        class(COO_dp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1, self%nnz
            if( ik_ == self%index(1,k) .and. jk_ == self%index(2,k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_dp
    end function

    subroutine add_value_coo_dp(self,ik,jk,val)
        class(COO_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1,self%nnz
            if( ik == self%index(1,k) .and. jk == self%index(2,k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_coo_dp(self,ik,jk,val)
        class(COO_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1, self%nnz
            do i = 1, size(ik)
                if( ik(i) /= self%index(1,k) ) cycle
                do j = 1, size(jk)
                    if( jk(j) /= self%index(2,k) ) cycle
                    self%data(k) = self%data(k) + val(i,j)
                end do
            end do
        end do
    end subroutine

    pure complex(sp) function at_value_coo_csp(self,ik,jk) result(val)
        class(COO_csp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1, self%nnz
            if( ik_ == self%index(1,k) .and. jk_ == self%index(2,k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_csp
    end function

    subroutine add_value_coo_csp(self,ik,jk,val)
        class(COO_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1,self%nnz
            if( ik == self%index(1,k) .and. jk == self%index(2,k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_coo_csp(self,ik,jk,val)
        class(COO_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1, self%nnz
            do i = 1, size(ik)
                if( ik(i) /= self%index(1,k) ) cycle
                do j = 1, size(jk)
                    if( jk(j) /= self%index(2,k) ) cycle
                    self%data(k) = self%data(k) + val(i,j)
                end do
            end do
        end do
    end subroutine

    pure complex(dp) function at_value_coo_cdp(self,ik,jk) result(val)
        class(COO_cdp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1, self%nnz
            if( ik_ == self%index(1,k) .and. jk_ == self%index(2,k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_cdp
    end function

    subroutine add_value_coo_cdp(self,ik,jk,val)
        class(COO_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1,self%nnz
            if( ik == self%index(1,k) .and. jk == self%index(2,k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_coo_cdp(self,ik,jk,val)
        class(COO_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1, self%nnz
            do i = 1, size(ik)
                if( ik(i) /= self%index(1,k) ) cycle
                do j = 1, size(jk)
                    if( jk(j) /= self%index(2,k) ) cycle
                    self%data(k) = self%data(k) + val(i,j)
                end do
            end do
        end do
    end subroutine


    pure real(sp) function at_value_csr_sp(self,ik,jk) result(val)
        class(CSR_sp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%rowptr(ik_), self%rowptr(ik_+1)-1
            if( jk_ == self%col(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_sp
    end function

    subroutine add_value_csr_sp(self,ik,jk,val)
        class(CSR_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%rowptr(ik), self%rowptr(ik+1)-1
            if( jk == self%col(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csr_sp(self,ik,jk,val)
        class(CSR_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do i = 1, size(ik)
            do k = self%rowptr(ik(i)), self%rowptr(ik(i)+1)-1
                do j = 1, size(jk)
                    if( jk(j) == self%col(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure real(dp) function at_value_csr_dp(self,ik,jk) result(val)
        class(CSR_dp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%rowptr(ik_), self%rowptr(ik_+1)-1
            if( jk_ == self%col(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_dp
    end function

    subroutine add_value_csr_dp(self,ik,jk,val)
        class(CSR_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%rowptr(ik), self%rowptr(ik+1)-1
            if( jk == self%col(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csr_dp(self,ik,jk,val)
        class(CSR_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do i = 1, size(ik)
            do k = self%rowptr(ik(i)), self%rowptr(ik(i)+1)-1
                do j = 1, size(jk)
                    if( jk(j) == self%col(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(sp) function at_value_csr_csp(self,ik,jk) result(val)
        class(CSR_csp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%rowptr(ik_), self%rowptr(ik_+1)-1
            if( jk_ == self%col(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_csp
    end function

    subroutine add_value_csr_csp(self,ik,jk,val)
        class(CSR_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%rowptr(ik), self%rowptr(ik+1)-1
            if( jk == self%col(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csr_csp(self,ik,jk,val)
        class(CSR_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do i = 1, size(ik)
            do k = self%rowptr(ik(i)), self%rowptr(ik(i)+1)-1
                do j = 1, size(jk)
                    if( jk(j) == self%col(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(dp) function at_value_csr_cdp(self,ik,jk) result(val)
        class(CSR_cdp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%rowptr(ik_), self%rowptr(ik_+1)-1
            if( jk_ == self%col(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_cdp
    end function

    subroutine add_value_csr_cdp(self,ik,jk,val)
        class(CSR_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%rowptr(ik), self%rowptr(ik+1)-1
            if( jk == self%col(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csr_cdp(self,ik,jk,val)
        class(CSR_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in) :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do i = 1, size(ik)
            do k = self%rowptr(ik(i)), self%rowptr(ik(i)+1)-1
                do j = 1, size(jk)
                    if( jk(j) == self%col(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine


    pure real(sp) function at_value_csc_sp(self,ik,jk) result(val)
        class(CSC_sp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%colptr(jk_), self%colptr(jk_+1)-1
            if( ik_ == self%row(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_sp
    end function

    subroutine add_value_csc_sp(self,ik,jk,val)
        class(CSC_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%colptr(jk), self%colptr(jk+1)-1
            if( ik == self%row(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csc_sp(self,ik,jk,val)
        class(CSC_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do j = 1, size(jk)
            do k = self%colptr(jk(j)), self%colptr(jk(j)+1)-1
                do i = 1, size(ik)
                    if( ik(i) == self%row(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure real(dp) function at_value_csc_dp(self,ik,jk) result(val)
        class(CSC_dp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%colptr(jk_), self%colptr(jk_+1)-1
            if( ik_ == self%row(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_dp
    end function

    subroutine add_value_csc_dp(self,ik,jk,val)
        class(CSC_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%colptr(jk), self%colptr(jk+1)-1
            if( ik == self%row(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csc_dp(self,ik,jk,val)
        class(CSC_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do j = 1, size(jk)
            do k = self%colptr(jk(j)), self%colptr(jk(j)+1)-1
                do i = 1, size(ik)
                    if( ik(i) == self%row(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(sp) function at_value_csc_csp(self,ik,jk) result(val)
        class(CSC_csp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%colptr(jk_), self%colptr(jk_+1)-1
            if( ik_ == self%row(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_csp
    end function

    subroutine add_value_csc_csp(self,ik,jk,val)
        class(CSC_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%colptr(jk), self%colptr(jk+1)-1
            if( ik == self%row(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csc_csp(self,ik,jk,val)
        class(CSC_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do j = 1, size(jk)
            do k = self%colptr(jk(j)), self%colptr(jk(j)+1)-1
                do i = 1, size(ik)
                    if( ik(i) == self%row(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(dp) function at_value_csc_cdp(self,ik,jk) result(val)
        class(CSC_cdp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = self%colptr(jk_), self%colptr(jk_+1)-1
            if( ik_ == self%row(k) ) then
                val = self%data(k)
                return
            end if
        end do
        val = zero_cdp
    end function

    subroutine add_value_csc_cdp(self,ik,jk,val)
        class(CSC_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = self%colptr(jk), self%colptr(jk+1)-1
            if( ik == self%row(k) ) then
                self%data(k) = self%data(k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_csc_cdp(self,ik,jk,val)
        class(CSC_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do j = 1, size(jk)
            do k = self%colptr(jk(j)), self%colptr(jk(j)+1)-1
                do i = 1, size(ik)
                    if( ik(i) == self%row(k) ) then
                        self%data(k) = self%data(k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine


    pure real(sp) function at_value_ell_sp(self,ik,jk) result(val)
        class(ELL_sp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1 , self%K
            if( jk_ == self%index(ik_,k) ) then
                val = self%data(ik_,k)
                return
            end if
        end do
        val = zero_sp
    end function

    subroutine add_value_ell_sp(self,ik,jk,val)
        class(ELL_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1 , self%K
            if( jk == self%index(ik,k) ) then
                self%data(ik,k) = self%data(ik,k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_ell_sp(self,ik,jk,val)
        class(ELL_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1 , self%K
            do j = 1, size(jk)
                do i = 1, size(ik)
                    if( jk(j) == self%index(ik(i),k) ) then
                        self%data(ik(i),k) = self%data(ik(i),k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure real(dp) function at_value_ell_dp(self,ik,jk) result(val)
        class(ELL_dp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1 , self%K
            if( jk_ == self%index(ik_,k) ) then
                val = self%data(ik_,k)
                return
            end if
        end do
        val = zero_dp
    end function

    subroutine add_value_ell_dp(self,ik,jk,val)
        class(ELL_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1 , self%K
            if( jk == self%index(ik,k) ) then
                self%data(ik,k) = self%data(ik,k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_ell_dp(self,ik,jk,val)
        class(ELL_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1 , self%K
            do j = 1, size(jk)
                do i = 1, size(ik)
                    if( jk(j) == self%index(ik(i),k) ) then
                        self%data(ik(i),k) = self%data(ik(i),k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(sp) function at_value_ell_csp(self,ik,jk) result(val)
        class(ELL_csp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1 , self%K
            if( jk_ == self%index(ik_,k) ) then
                val = self%data(ik_,k)
                return
            end if
        end do
        val = zero_csp
    end function

    subroutine add_value_ell_csp(self,ik,jk,val)
        class(ELL_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1 , self%K
            if( jk == self%index(ik,k) ) then
                self%data(ik,k) = self%data(ik,k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_ell_csp(self,ik,jk,val)
        class(ELL_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1 , self%K
            do j = 1, size(jk)
                do i = 1, size(ik)
                    if( jk(j) == self%index(ik(i),k) ) then
                        self%data(ik(i),k) = self%data(ik(i),k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(dp) function at_value_ell_cdp(self,ik,jk) result(val)
        class(ELL_cdp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if
        do k = 1 , self%K
            if( jk_ == self%index(ik_,k) ) then
                val = self%data(ik_,k)
                return
            end if
        end do
        val = zero_cdp
    end function

    subroutine add_value_ell_cdp(self,ik,jk,val)
        class(ELL_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k
        ! naive implementation
        do k = 1 , self%K
            if( jk == self%index(ik,k) ) then
                self%data(ik,k) = self%data(ik,k) + val
                return
            end if
        end do
    end subroutine

    subroutine add_block_ell_cdp(self,ik,jk,val)
        class(ELL_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j
        ! naive implementation
        do k = 1 , self%K
            do j = 1, size(jk)
                do i = 1, size(ik)
                    if( jk(j) == self%index(ik(i),k) ) then
                        self%data(ik(i),k) = self%data(ik(i),k) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine


    pure real(sp) function at_value_sellc_sp(self,ik,jk) result(val)
        class(SELLC_sp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_, idx
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if

        idx = self%rowptr((ik_ - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk_ == self%col(k,idx) )then
                val = self%data(k,idx)
                return
            endif
        end do
        val = zero_sp
    end function

    subroutine add_value_sellc_sp(self,ik,jk,val)
        class(SELLC_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k, idx
        ! naive implementation
        idx = self%rowptr((ik - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk == self%col(k,idx) )then
                self%data(k,idx) = self%data(k,idx) + val
                return
            endif
        end do
    end subroutine

    subroutine add_block_sellc_sp(self,ik,jk,val)
        class(SELLC_sp_type), intent(inout) :: self
        real(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j, idx
        ! naive implementation
        do k = 1 , self%chunk_size
            do j = 1, size(jk)
                do i = 1, size(ik)
                    idx = self%rowptr((ik(i) - 1)/self%chunk_size + 1)
                    if( jk(j) == self%col(k,idx) ) then
                        self%data(k,idx) = self%data(k,idx) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure real(dp) function at_value_sellc_dp(self,ik,jk) result(val)
        class(SELLC_dp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_, idx
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if

        idx = self%rowptr((ik_ - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk_ == self%col(k,idx) )then
                val = self%data(k,idx)
                return
            endif
        end do
        val = zero_dp
    end function

    subroutine add_value_sellc_dp(self,ik,jk,val)
        class(SELLC_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k, idx
        ! naive implementation
        idx = self%rowptr((ik - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk == self%col(k,idx) )then
                self%data(k,idx) = self%data(k,idx) + val
                return
            endif
        end do
    end subroutine

    subroutine add_block_sellc_dp(self,ik,jk,val)
        class(SELLC_dp_type), intent(inout) :: self
        real(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j, idx
        ! naive implementation
        do k = 1 , self%chunk_size
            do j = 1, size(jk)
                do i = 1, size(ik)
                    idx = self%rowptr((ik(i) - 1)/self%chunk_size + 1)
                    if( jk(j) == self%col(k,idx) ) then
                        self%data(k,idx) = self%data(k,idx) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(sp) function at_value_sellc_csp(self,ik,jk) result(val)
        class(SELLC_csp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_, idx
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._sp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if

        idx = self%rowptr((ik_ - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk_ == self%col(k,idx) )then
                val = self%data(k,idx)
                return
            endif
        end do
        val = zero_csp
    end function

    subroutine add_value_sellc_csp(self,ik,jk,val)
        class(SELLC_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k, idx
        ! naive implementation
        idx = self%rowptr((ik - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk == self%col(k,idx) )then
                self%data(k,idx) = self%data(k,idx) + val
                return
            endif
        end do
    end subroutine

    subroutine add_block_sellc_csp(self,ik,jk,val)
        class(SELLC_csp_type), intent(inout) :: self
        complex(sp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j, idx
        ! naive implementation
        do k = 1 , self%chunk_size
            do j = 1, size(jk)
                do i = 1, size(ik)
                    idx = self%rowptr((ik(i) - 1)/self%chunk_size + 1)
                    if( jk(j) == self%col(k,idx) ) then
                        self%data(k,idx) = self%data(k,idx) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine

    pure complex(dp) function at_value_sellc_cdp(self,ik,jk) result(val)
        class(SELLC_cdp_type), intent(in) :: self
        integer(ilp), intent(in) :: ik, jk
        integer(ilp) :: k, ik_, jk_, idx
        logical :: transpose
        ! naive implementation
        if( (ik<1 .or. ik>self%nrows) .or. (jk<1 .or. jk>self%ncols) ) then
            val = ieee_value( 0._dp , ieee_quiet_nan)
            return
        end if
        ik_ = ik; jk_ = jk
        transpose = (self%storage == sparse_lower .and. ik > jk) .or. (self%storage == sparse_upper .and. ik < jk)
        if(transpose) then ! allow extraction of symmetric elements
            ik_ = jk; jk_ = ik
        end if

        idx = self%rowptr((ik_ - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk_ == self%col(k,idx) )then
                val = self%data(k,idx)
                return
            endif
        end do
        val = zero_cdp
    end function

    subroutine add_value_sellc_cdp(self,ik,jk,val)
        class(SELLC_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val
        integer(ilp), intent(in)  :: ik, jk
        integer(ilp) :: k, idx
        ! naive implementation
        idx = self%rowptr((ik - 1)/self%chunk_size + 1)
        do k = 1, self%chunk_size
            if ( jk == self%col(k,idx) )then
                self%data(k,idx) = self%data(k,idx) + val
                return
            endif
        end do
    end subroutine

    subroutine add_block_sellc_cdp(self,ik,jk,val)
        class(SELLC_cdp_type), intent(inout) :: self
        complex(dp), intent(in) :: val(:,:)
        integer(ilp), intent(in)  :: ik(:), jk(:)
        integer(ilp) :: k, i, j, idx
        ! naive implementation
        do k = 1 , self%chunk_size
            do j = 1, size(jk)
                do i = 1, size(ik)
                    idx = self%rowptr((ik(i) - 1)/self%chunk_size + 1)
                    if( jk(j) == self%col(k,idx) ) then
                        self%data(k,idx) = self%data(k,idx) + val(i,j)
                    end if
                end do
            end do
        end do
    end subroutine


end module stdlib_sparse_kinds
