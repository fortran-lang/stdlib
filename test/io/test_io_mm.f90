! Simple test for Matrix Market loading only
program test_matrix_market
    use stdlib_io_mm
    use stdlib_kinds, only: dp
    implicit none
    
    call test_load_simple()
    
    write(*,*) 'Matrix Market load test passed!'
    
contains
    
    subroutine test_load_simple()
        real(dp), allocatable :: matrix(:,:)
        integer :: iostat, io, i
        character(len=:), allocatable :: iomsg
        
        write(*,*) 'Testing simple Matrix Market loading...'
        
        ! Create a simple test file
        open(newunit=io, file='simple_test.mtx', action='write')
        write(io, '(A)') '%%MatrixMarket matrix array real general'
        write(io, '(A)') '% Simple test matrix'
        write(io, '(A)') '2 2'
        write(io, '(A)') '1.0'
        write(io, '(A)') '0.0'
        write(io, '(A)') '0.0'
        write(io, '(A)') '2.0'
        close(io)
        
        ! Try to load it
        call load_mm('simple_test.mtx', matrix, iostat=iostat, iomsg=iomsg)
        if (iostat /= 0) then
            write(*,*) 'Error loading simple matrix: iostat=', iostat
            if (allocated(iomsg)) write(*,*) 'Message: ', iomsg
            stop 1
        end if
        
        ! Check results
        if (.not. allocated(matrix)) then
            write(*,*) 'Error: matrix not allocated'
            stop 1
        end if
        
        if (size(matrix,1) /= 2 .or. size(matrix,2) /= 2) then
            write(*,*) 'Error: wrong dimensions', size(matrix,1), size(matrix,2)
            stop 1
        end if
        
        if (abs(matrix(1,1) - 1.0_dp) > 1e-12_dp .or. &
            abs(matrix(2,1) - 0.0_dp) > 1e-12_dp .or. &
            abs(matrix(1,2) - 0.0_dp) > 1e-12_dp .or. &
            abs(matrix(2,2) - 2.0_dp) > 1e-12_dp) then
            write(*,*) 'Error: wrong matrix values'
            write(*,*) 'Expected: 1 0; 0 2'
            write(*,*) 'Got:'
            do i = 1, 2
                write(*,'(*(F8.3))') matrix(i,:)
            end do
            stop 1
        end if
        
        write(*,*) 'Simple load test passed'
    end subroutine test_load_simple
    
end program test_matrix_market