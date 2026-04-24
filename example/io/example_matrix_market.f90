program example_matrix_market
    use stdlib_io_mm, only : load_mm, save_mm
    use stdlib_kinds, only : dp
    implicit none

    real(dp), allocatable :: matrix(:,:), matrix2(:,:)
    integer, allocatable  :: index(:,:)
    complex(dp), allocatable :: data(:)
    character(len=*), parameter :: dense_filename = "example_dense.mtx"
    character(len=*), parameter :: sparse_filename = "example_sparse.mtx"
    integer :: iostat, i
    character(len=:), allocatable :: iomsg

    ! Create a test dense matrix
    allocate(matrix(3,3))
    matrix = reshape([1.0_dp, 2.0_dp, 3.0_dp, &
                      4.0_dp, 5.0_dp, 6.0_dp, &
                      7.0_dp, 8.0_dp, 9.0_dp], [3,3])

    print *, "=== Dense Matrix Example ==="
    print *, "Original dense matrix:"
    call print_matrix(matrix)

    ! Save dense matrix to Matrix Market file
    call save_mm(dense_filename, matrix, format="ES24.15E2", symmetry="general", iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) then
        print *, "Error saving dense matrix: ", iomsg
        stop 1
    end if

    print *, "Dense matrix saved to ", dense_filename

    ! Load dense matrix from Matrix Market file  
    call load_mm(dense_filename, matrix2, iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) then
        print *, "Error loading dense matrix: ", iomsg
        stop 1
    end if

    print *, "Loaded dense matrix:"
    call print_matrix(matrix2)

    print *, "=== Sparse Matrix Example ==="
    ! Create a test sparse matrix
    allocate(index(2,6))
    allocate(data(6))
    index(:,1) = [1,1]; data(1) = (10.0_dp, -1.5_dp)
    index(:,2) = [2,2]; data(2) = (20.0_dp, 2.0_dp)
    index(:,3) = [3,3]; data(3) = (30.0_dp, 3.0_dp)
    index(:,4) = [4,4]; data(4) = (40.0_dp, -4.0_dp)
    index(:,5) = [1,4]; data(5) = ( 5.0_dp, -7.5_dp)
    index(:,6) = [3,1]; data(6) = (15.0_dp, 25.0_dp)

    ! Save sparse matrix to Matrix Market file
    call save_mm(sparse_filename, index, data, format="ES24.15E2", symmetry="general", iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) then
        print *, "Error saving sparse matrix: ", iomsg
        stop 1
    end if

    print *, "Sparse matrix saved to ", sparse_filename

    ! Load sparse matrix from Matrix Market file
    call load_mm(sparse_filename, index, data, iostat=iostat, iomsg=iomsg)
    if (iostat /= 0) then
        print *, "Error loading sparse matrix: ", iomsg
        stop 1
    end if

    print *, "Loaded sparse matrix (COO format):"
    print *, "Data (row, col, value):"
    do i = 1, size(data)
        print *, index(1,i), index(2,i), data(i)
    end do

contains

    subroutine print_matrix(mat)
        real(dp), intent(in) :: mat(:,:)
        integer :: i
        
        do i = 1, size(mat, 1)
            print *, mat(i, :)
        end do
        print *
    end subroutine print_matrix

end program example_matrix_market