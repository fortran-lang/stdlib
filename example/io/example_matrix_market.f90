program example_matrix_market
    use stdlib_io_mm, only : load_mm, save_mm, mm_header_type
    use stdlib_kinds, only : dp
    implicit none

    real(dp), allocatable :: matrix(:,:), matrix2(:,:)
    integer, allocatable  :: index(:,:)
    real(dp), allocatable :: data(:)
    character(len=*), parameter :: dense_filename = "test_dense.mtx"
    character(len=*), parameter :: sparse_filename = "test_sparse.mtx"
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
    call save_mm(dense_filename, matrix, iostat=iostat, iomsg=iomsg)
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

    ! Create a sparse test file manually for demonstration
    call create_sparse_test_file(sparse_filename)

    print *, "=== Sparse Matrix Example ==="
    print *, "Loading sparse matrix from ", sparse_filename

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

    subroutine create_sparse_test_file(filename)
        character(len=*), intent(in) :: filename
        integer :: u
        
        open(newunit=u, file=filename, status='replace')
        write(u, '(A)') '%%MatrixMarket matrix coordinate real general'
        write(u, '(A)') '% This is a test sparse matrix'
        write(u, '(A)') '4 4 6'
        write(u, '(A)') '1 1 10.0'
        write(u, '(A)') '2 2 20.0'
        write(u, '(A)') '3 3 30.0'
        write(u, '(A)') '4 4 40.0'
        write(u, '(A)') '1 4 5.0'
        write(u, '(A)') '3 1 15.0'
        close(u)
    end subroutine create_sparse_test_file

end program example_matrix_market