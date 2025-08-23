program example_sparse_from_ijv
    use stdlib_linalg_constants, only: dp
    use stdlib_sparse
    implicit none

    integer :: row(10), col(10)
    real(dp) :: data(10)
    type(COO_dp_type) :: COO
    type(CSR_dp_type) :: CSR
    type(ELL_dp_type) :: ELL
    integer :: i, j

    ! Initial data
    row = [1,1,2,2,3,3,3,4,4,4]
    col = [1,5,1,2,2,3,4,1,3,4]
    data = real([9,-3,4,7,8,-1,8,4,5,6] , kind = dp )

    ! Create a COO matrix from triplet
    call from_ijv(COO,row,col,data)
    print *, 'COO'
    print *, '  i,  j,    v'
    do i = 1, COO%nnz
        print '(2I4,f8.1)', COO%index(:,i), COO%data(i)
    end do

    ! Create a CSR matrix from triplet
    call from_ijv(CSR,row,col,data)
    print *, 'CSR'
    print '(A,5I8)',    'rowptr :', CSR%rowptr
    print '(A,10I8)',   'col    :', CSR%col 
    print '(A,10f8.1)', 'data   :', CSR%data
    
    ! Create an ELL matrix from triplet
    call from_ijv(ELL,row,col,data)
    print *, 'ELL'
    print *, ' index        |         data'
    do i = 1, ELL%nrows
        print '(3I4,1x,3f8.1)', ELL%index(i,:) , ELL%data(i,:)
    end do
  
end program example_sparse_from_ijv
