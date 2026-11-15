module test_sparse_spmv
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_sparse
    use stdlib_constants
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan

    implicit none

contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('coo', test_coo), &
            new_unittest('coo2ordered', test_coo2ordered), &
            new_unittest('bsr', test_bsr), &
            new_unittest('bsr_symmetric', test_bsr_symmetric), &
            new_unittest('bsr_block_sizes', test_bsr_block_sizes), &
            new_unittest('csr', test_csr), &
            new_unittest('csc', test_csc), &
            new_unittest('ell', test_ell),  &
            new_unittest('sellc', test_sellc),  &
            new_unittest('symmetries', test_symmetries), &
            new_unittest('diagonal', test_diagonal), &
            new_unittest('add_get_values', test_add_get_values),  &
            new_unittest('at_symmetric', test_at_symmetric),  &
            new_unittest('sellc_add_get_values', test_sellc_add_get_values),  &
            new_unittest('sparse_operators', test_sparse_operators), &
            new_unittest('add_block_symmetric_skip', test_add_block_symmetric_skip), &
            new_unittest('csc2dense', test_csc2dense), &
            new_unittest('coo2dense', test_coo2dense), &
            new_unittest('csr2dense', test_csr2dense) &
        ]
    end subroutine

    subroutine test_coo(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(COO_sp_type) :: COO
            real(sp), parameter :: alpha = 1._wp
            real(sp), parameter :: beta = 0._wp
            real(sp), allocatable :: dense(:,:)
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y1(:), vec_y2(:)

            allocate( dense(4,5) , source = &
                    reshape(real([9,4, 0,4, &
                                  0,7, 8,0, &
                                  0,0,-1,5, &
                                  0,0, 8,6, &
                                 -3,0, 0,0],kind=wp),[4,5]) )
            
            call dense2coo( dense , COO )
            
            allocate( vec_x(5)  , source = 1._wp )
            allocate( vec_y1(4) , source = 0._wp )
            allocate( vec_y2(4) , source = 0._wp )

            vec_y1 = matmul( dense, vec_x )
            
            call check(error, all(vec_y1 == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! High-level API
            call spmv( COO, vec_x, vec_y2 )
            call check(error, all(vec_y1 == vec_y2) )
            if (allocated(error)) return

            ! Low-level API
            call spmv_kernel_coo(sparse_op_none, alpha, &
                COO%data ,COO%index, COO%storage, &
                vec_x, beta, vec_y2 )
            call check(error, all(vec_y1 == vec_y2), 'COO - low level API: wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y1 = 1._wp
            ! High-level API
            call spmv( COO, vec_y1, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return

            ! Low-level API
            call spmv_kernel_coo(sparse_op_transpose, alpha, &
                COO%data ,COO%index, COO%storage, &
                vec_y1, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'COO - transpose - low level API: wrong output' )
            if (allocated(error)) return
 
        end block
        block
            integer, parameter :: wp = dp
            type(COO_dp_type) :: COO
            real(dp), parameter :: alpha = 1._wp
            real(dp), parameter :: beta = 0._wp
            real(dp), allocatable :: dense(:,:)
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y1(:), vec_y2(:)

            allocate( dense(4,5) , source = &
                    reshape(real([9,4, 0,4, &
                                  0,7, 8,0, &
                                  0,0,-1,5, &
                                  0,0, 8,6, &
                                 -3,0, 0,0],kind=wp),[4,5]) )
            
            call dense2coo( dense , COO )
            
            allocate( vec_x(5)  , source = 1._wp )
            allocate( vec_y1(4) , source = 0._wp )
            allocate( vec_y2(4) , source = 0._wp )

            vec_y1 = matmul( dense, vec_x )
            
            call check(error, all(vec_y1 == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! High-level API
            call spmv( COO, vec_x, vec_y2 )
            call check(error, all(vec_y1 == vec_y2) )
            if (allocated(error)) return

            ! Low-level API
            call spmv_kernel_coo(sparse_op_none, alpha, &
                COO%data ,COO%index, COO%storage, &
                vec_x, beta, vec_y2 )
            call check(error, all(vec_y1 == vec_y2), 'COO - low level API: wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y1 = 1._wp
            ! High-level API
            call spmv( COO, vec_y1, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return

            ! Low-level API
            call spmv_kernel_coo(sparse_op_transpose, alpha, &
                COO%data ,COO%index, COO%storage, &
                vec_y1, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'COO - transpose - low level API: wrong output' )
            if (allocated(error)) return
 
        end block
    end subroutine

    subroutine test_coo2ordered(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        type(COO_sp_type) :: COO
        integer :: row(12), col(12)
        real    :: data(12)

        row = [1,1,1,2,2,3,2,2,2,3,3,4]
        col = [2,3,4,3,4,4,3,4,5,4,5,5]
        data = 1.0

        call from_ijv(COO,row,col,data)

        call coo2ordered(COO,sort_data=.true.)
        call check(error, COO%nnz < 12 .and. COO%nnz == 9 )
        if (allocated(error)) return

        call check(error, all(COO%data==[1,1,1,2,2,1,2,1,1])  )
        if (allocated(error)) return

        call check(error, all(COO%index(1,:)==[1,1,1,2,2,2,3,3,4])  )
        if (allocated(error)) return

        call check(error, all(COO%index(2,:)==[2,3,4,3,4,5,4,5,5])  )
        if (allocated(error)) return

    end subroutine 

    subroutine test_bsr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(BSR_sp_type) :: BSR
            real(wp) :: dense(6,6)
            real(sp) :: vec_x(6), vec_y(6), vec_x2(2,6), vec_y2(2,6)
            integer :: i, j

            ! |1 0| |6 7|  0 0
            ! |2 1| |8 2|  0 0
            ! -----------------
            !  0 0  |1 4|  0 0
            !  0 0  |5 1|  0 0
            ! -----------------
            !  0 0  |4 3| |7 2|
            !  0 0  |0 0| |0 0|
            dense = transpose(reshape(real([1,0,6,7,0,0, &
                                            2,1,8,2,0,0, &
                                            0,0,1,4,0,0, &
                                            0,0,5,1,0,0, &
                                            0,0,4,3,7,2, &
                                            0,0,0,0,0,0],kind=wp),[6,6]))

            call BSR%malloc([2,2],3,3,5)
            BSR%rowptr = [1,3,4,6]
            BSR%col    = [1,2,2,2,3]
            BSR%data(:,:,1) = reshape(real([1,2,0,1],kind=wp),[2,2])
            BSR%data(:,:,2) = reshape(real([6,8,7,2],kind=wp),[2,2])
            BSR%data(:,:,3) = reshape(real([1,5,4,1],kind=wp),[2,2])
            BSR%data(:,:,4) = reshape(real([4,0,3,0],kind=wp),[2,2])
            BSR%data(:,:,5) = reshape(real([7,0,2,0],kind=wp),[2,2])

            do j = 1, 6
                do i = 1, 6
                    call check(error, BSR%at(i,j) == dense(i,j), "BSR sp: at() wrong value" )
                    if (allocated(error)) return
                end do
            end do

            vec_x = real([1,2,3,4,5,6],kind=wp)
            call spmv( BSR, vec_x, vec_y )
            call check(error, all(vec_y == real([47,36,19,19,71,0],kind=wp)), "BSR sp: spmv" )
            if (allocated(error)) return

            call spmv( BSR, vec_x, vec_y, op=sparse_op_transpose )
            call check(error, all(vec_y == real([5,2,65,42,35,10],kind=wp)), "BSR sp: spmv transpose" )
            if (allocated(error)) return

            ! y = 2*A*x - y, low-level API
            vec_y = 1._wp
            call spmv_kernel_bsr(sparse_op_none, 2*one_sp, BSR%data, BSR%col, BSR%rowptr, BSR%storage, &
                vec_x, -one_sp, vec_y)
            call check(error, all(vec_y == real([93,71,37,37,141,-1],kind=wp)), "BSR sp: spmv_kernel_bsr" )
            if (allocated(error)) return

            ! two right-hand sides in one call, vec_x2(k,n): x and a vector of ones (giving the row sums)
            vec_x2(1,:) = vec_x
            vec_x2(2,:) = 1._wp
            call spmv( BSR, vec_x2, vec_y2 )
            call check(error, all(vec_y2(1,:) == real([47,36,19,19,71,0],kind=wp)) .and. &
                              all(vec_y2(2,:) == real([14,13,5,6,16,0],kind=wp)), "BSR sp: rank-2 spmv" )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(BSR_dp_type) :: BSR
            real(wp) :: dense(6,6)
            real(dp) :: vec_x(6), vec_y(6), vec_x2(2,6), vec_y2(2,6)
            integer :: i, j

            ! |1 0| |6 7|  0 0
            ! |2 1| |8 2|  0 0
            ! -----------------
            !  0 0  |1 4|  0 0
            !  0 0  |5 1|  0 0
            ! -----------------
            !  0 0  |4 3| |7 2|
            !  0 0  |0 0| |0 0|
            dense = transpose(reshape(real([1,0,6,7,0,0, &
                                            2,1,8,2,0,0, &
                                            0,0,1,4,0,0, &
                                            0,0,5,1,0,0, &
                                            0,0,4,3,7,2, &
                                            0,0,0,0,0,0],kind=wp),[6,6]))

            call BSR%malloc([2,2],3,3,5)
            BSR%rowptr = [1,3,4,6]
            BSR%col    = [1,2,2,2,3]
            BSR%data(:,:,1) = reshape(real([1,2,0,1],kind=wp),[2,2])
            BSR%data(:,:,2) = reshape(real([6,8,7,2],kind=wp),[2,2])
            BSR%data(:,:,3) = reshape(real([1,5,4,1],kind=wp),[2,2])
            BSR%data(:,:,4) = reshape(real([4,0,3,0],kind=wp),[2,2])
            BSR%data(:,:,5) = reshape(real([7,0,2,0],kind=wp),[2,2])

            do j = 1, 6
                do i = 1, 6
                    call check(error, BSR%at(i,j) == dense(i,j), "BSR dp: at() wrong value" )
                    if (allocated(error)) return
                end do
            end do

            vec_x = real([1,2,3,4,5,6],kind=wp)
            call spmv( BSR, vec_x, vec_y )
            call check(error, all(vec_y == real([47,36,19,19,71,0],kind=wp)), "BSR dp: spmv" )
            if (allocated(error)) return

            call spmv( BSR, vec_x, vec_y, op=sparse_op_transpose )
            call check(error, all(vec_y == real([5,2,65,42,35,10],kind=wp)), "BSR dp: spmv transpose" )
            if (allocated(error)) return

            ! y = 2*A*x - y, low-level API
            vec_y = 1._wp
            call spmv_kernel_bsr(sparse_op_none, 2*one_dp, BSR%data, BSR%col, BSR%rowptr, BSR%storage, &
                vec_x, -one_dp, vec_y)
            call check(error, all(vec_y == real([93,71,37,37,141,-1],kind=wp)), "BSR dp: spmv_kernel_bsr" )
            if (allocated(error)) return

            ! two right-hand sides in one call, vec_x2(k,n): x and a vector of ones (giving the row sums)
            vec_x2(1,:) = vec_x
            vec_x2(2,:) = 1._wp
            call spmv( BSR, vec_x2, vec_y2 )
            call check(error, all(vec_y2(1,:) == real([47,36,19,19,71,0],kind=wp)) .and. &
                              all(vec_y2(2,:) == real([14,13,5,6,16,0],kind=wp)), "BSR dp: rank-2 spmv" )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = sp
            type(BSR_csp_type) :: BSR
            real(wp) :: dense(6,6)
            complex(sp) :: vec_x(6), vec_y(6), vec_x2(2,6), vec_y2(2,6)
            integer :: i, j

            ! |1 0| |6 7|  0 0
            ! |2 1| |8 2|  0 0
            ! -----------------
            !  0 0  |1 4|  0 0
            !  0 0  |5 1|  0 0
            ! -----------------
            !  0 0  |4 3| |7 2|
            !  0 0  |0 0| |0 0|
            dense = transpose(reshape(real([1,0,6,7,0,0, &
                                            2,1,8,2,0,0, &
                                            0,0,1,4,0,0, &
                                            0,0,5,1,0,0, &
                                            0,0,4,3,7,2, &
                                            0,0,0,0,0,0],kind=wp),[6,6]))

            call BSR%malloc([2,2],3,3,5)
            BSR%rowptr = [1,3,4,6]
            BSR%col    = [1,2,2,2,3]
            BSR%data(:,:,1) = reshape(real([1,2,0,1],kind=wp),[2,2])
            BSR%data(:,:,2) = reshape(real([6,8,7,2],kind=wp),[2,2])
            BSR%data(:,:,3) = reshape(real([1,5,4,1],kind=wp),[2,2])
            BSR%data(:,:,4) = reshape(real([4,0,3,0],kind=wp),[2,2])
            BSR%data(:,:,5) = reshape(real([7,0,2,0],kind=wp),[2,2])

            do j = 1, 6
                do i = 1, 6
                    call check(error, BSR%at(i,j) == dense(i,j), "BSR csp: at() wrong value" )
                    if (allocated(error)) return
                end do
            end do

            vec_x = real([1,2,3,4,5,6],kind=wp)
            call spmv( BSR, vec_x, vec_y )
            call check(error, all(vec_y == real([47,36,19,19,71,0],kind=wp)), "BSR csp: spmv" )
            if (allocated(error)) return

            call spmv( BSR, vec_x, vec_y, op=sparse_op_transpose )
            call check(error, all(vec_y == real([5,2,65,42,35,10],kind=wp)), "BSR csp: spmv transpose" )
            if (allocated(error)) return

            ! y = 2*A*x - y, low-level API
            vec_y = 1._wp
            call spmv_kernel_bsr(sparse_op_none, 2*one_csp, BSR%data, BSR%col, BSR%rowptr, BSR%storage, &
                vec_x, -one_csp, vec_y)
            call check(error, all(vec_y == real([93,71,37,37,141,-1],kind=wp)), "BSR csp: spmv_kernel_bsr" )
            if (allocated(error)) return

            ! two right-hand sides in one call, vec_x2(k,n): x and a vector of ones (giving the row sums)
            vec_x2(1,:) = vec_x
            vec_x2(2,:) = 1._wp
            call spmv( BSR, vec_x2, vec_y2 )
            call check(error, all(vec_y2(1,:) == real([47,36,19,19,71,0],kind=wp)) .and. &
                              all(vec_y2(2,:) == real([14,13,5,6,16,0],kind=wp)), "BSR csp: rank-2 spmv" )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(BSR_cdp_type) :: BSR
            real(wp) :: dense(6,6)
            complex(dp) :: vec_x(6), vec_y(6), vec_x2(2,6), vec_y2(2,6)
            integer :: i, j

            ! |1 0| |6 7|  0 0
            ! |2 1| |8 2|  0 0
            ! -----------------
            !  0 0  |1 4|  0 0
            !  0 0  |5 1|  0 0
            ! -----------------
            !  0 0  |4 3| |7 2|
            !  0 0  |0 0| |0 0|
            dense = transpose(reshape(real([1,0,6,7,0,0, &
                                            2,1,8,2,0,0, &
                                            0,0,1,4,0,0, &
                                            0,0,5,1,0,0, &
                                            0,0,4,3,7,2, &
                                            0,0,0,0,0,0],kind=wp),[6,6]))

            call BSR%malloc([2,2],3,3,5)
            BSR%rowptr = [1,3,4,6]
            BSR%col    = [1,2,2,2,3]
            BSR%data(:,:,1) = reshape(real([1,2,0,1],kind=wp),[2,2])
            BSR%data(:,:,2) = reshape(real([6,8,7,2],kind=wp),[2,2])
            BSR%data(:,:,3) = reshape(real([1,5,4,1],kind=wp),[2,2])
            BSR%data(:,:,4) = reshape(real([4,0,3,0],kind=wp),[2,2])
            BSR%data(:,:,5) = reshape(real([7,0,2,0],kind=wp),[2,2])

            do j = 1, 6
                do i = 1, 6
                    call check(error, BSR%at(i,j) == dense(i,j), "BSR cdp: at() wrong value" )
                    if (allocated(error)) return
                end do
            end do

            vec_x = real([1,2,3,4,5,6],kind=wp)
            call spmv( BSR, vec_x, vec_y )
            call check(error, all(vec_y == real([47,36,19,19,71,0],kind=wp)), "BSR cdp: spmv" )
            if (allocated(error)) return

            call spmv( BSR, vec_x, vec_y, op=sparse_op_transpose )
            call check(error, all(vec_y == real([5,2,65,42,35,10],kind=wp)), "BSR cdp: spmv transpose" )
            if (allocated(error)) return

            ! y = 2*A*x - y, low-level API
            vec_y = 1._wp
            call spmv_kernel_bsr(sparse_op_none, 2*one_cdp, BSR%data, BSR%col, BSR%rowptr, BSR%storage, &
                vec_x, -one_cdp, vec_y)
            call check(error, all(vec_y == real([93,71,37,37,141,-1],kind=wp)), "BSR cdp: spmv_kernel_bsr" )
            if (allocated(error)) return

            ! two right-hand sides in one call, vec_x2(k,n): x and a vector of ones (giving the row sums)
            vec_x2(1,:) = vec_x
            vec_x2(2,:) = 1._wp
            call spmv( BSR, vec_x2, vec_y2 )
            call check(error, all(vec_y2(1,:) == real([47,36,19,19,71,0],kind=wp)) .and. &
                              all(vec_y2(2,:) == real([14,13,5,6,16,0],kind=wp)), "BSR cdp: rank-2 spmv" )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_bsr_symmetric(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(BSR_sp_type) :: BSR(2)
            real(wp) :: dense(6,6)
            real(sp) :: vec_x(6), vec_y(6)
            integer :: s, i, j

            ! |2 1| |0 3|  0 0
            ! |1 2| |4 0|  0 0
            ! -----------------
            ! |0 4| |5 1| |1 0|
            ! |3 0| |1 5| |0 1|
            ! -----------------
            !  0 0  |1 0| |6 2|
            !  0 0  |0 1| |2 6|
            dense = transpose(reshape(real([2,1,0,3,0,0, &
                                            1,2,4,0,0,0, &
                                            0,4,5,1,1,0, &
                                            3,0,1,5,0,1, &
                                            0,0,1,0,6,2, &
                                            0,0,0,1,2,6],kind=wp),[6,6]))

            ! lower triangle of blocks
            call BSR(1)%malloc([2,2],3,3,5)
            BSR(1)%storage = sparse_lower
            BSR(1)%rowptr = [1,2,4,6]
            BSR(1)%col    = [1,1,2,2,3]
            BSR(1)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(1)%data(:,:,2) = reshape(real([0,3,4,0],kind=wp),[2,2])
            BSR(1)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(1)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(1)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            ! upper triangle of blocks
            call BSR(2)%malloc([2,2],3,3,5)
            BSR(2)%storage = sparse_upper
            BSR(2)%rowptr = [1,3,5,6]
            BSR(2)%col    = [1,2,2,3,3]
            BSR(2)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(2)%data(:,:,2) = reshape(real([0,4,3,0],kind=wp),[2,2])
            BSR(2)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(2)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(2)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            vec_x = real([1,2,3,4,5,6],kind=wp)
            do s = 1, 2
                do j = 1, 6
                    do i = 1, 6
                        call check(error, BSR(s)%at(i,j) == dense(i,j), "BSR sp symmetric: at() wrong value" )
                        if (allocated(error)) return
                    end do
                end do

                call spmv( BSR(s), vec_x, vec_y )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), "BSR sp symmetric: spmv" )
                if (allocated(error)) return

                call spmv( BSR(s), vec_x, vec_y, op=sparse_op_transpose )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), &
                    "BSR sp symmetric: spmv transpose" )
                if (allocated(error)) return
            end do
        end block
        block
            integer, parameter :: wp = dp
            type(BSR_dp_type) :: BSR(2)
            real(wp) :: dense(6,6)
            real(dp) :: vec_x(6), vec_y(6)
            integer :: s, i, j

            ! |2 1| |0 3|  0 0
            ! |1 2| |4 0|  0 0
            ! -----------------
            ! |0 4| |5 1| |1 0|
            ! |3 0| |1 5| |0 1|
            ! -----------------
            !  0 0  |1 0| |6 2|
            !  0 0  |0 1| |2 6|
            dense = transpose(reshape(real([2,1,0,3,0,0, &
                                            1,2,4,0,0,0, &
                                            0,4,5,1,1,0, &
                                            3,0,1,5,0,1, &
                                            0,0,1,0,6,2, &
                                            0,0,0,1,2,6],kind=wp),[6,6]))

            ! lower triangle of blocks
            call BSR(1)%malloc([2,2],3,3,5)
            BSR(1)%storage = sparse_lower
            BSR(1)%rowptr = [1,2,4,6]
            BSR(1)%col    = [1,1,2,2,3]
            BSR(1)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(1)%data(:,:,2) = reshape(real([0,3,4,0],kind=wp),[2,2])
            BSR(1)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(1)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(1)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            ! upper triangle of blocks
            call BSR(2)%malloc([2,2],3,3,5)
            BSR(2)%storage = sparse_upper
            BSR(2)%rowptr = [1,3,5,6]
            BSR(2)%col    = [1,2,2,3,3]
            BSR(2)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(2)%data(:,:,2) = reshape(real([0,4,3,0],kind=wp),[2,2])
            BSR(2)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(2)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(2)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            vec_x = real([1,2,3,4,5,6],kind=wp)
            do s = 1, 2
                do j = 1, 6
                    do i = 1, 6
                        call check(error, BSR(s)%at(i,j) == dense(i,j), "BSR dp symmetric: at() wrong value" )
                        if (allocated(error)) return
                    end do
                end do

                call spmv( BSR(s), vec_x, vec_y )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), "BSR dp symmetric: spmv" )
                if (allocated(error)) return

                call spmv( BSR(s), vec_x, vec_y, op=sparse_op_transpose )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), &
                    "BSR dp symmetric: spmv transpose" )
                if (allocated(error)) return
            end do
        end block
        block
            integer, parameter :: wp = sp
            type(BSR_csp_type) :: BSR(2)
            real(wp) :: dense(6,6)
            complex(sp) :: vec_x(6), vec_y(6)
            integer :: s, i, j

            ! |2 1| |0 3|  0 0
            ! |1 2| |4 0|  0 0
            ! -----------------
            ! |0 4| |5 1| |1 0|
            ! |3 0| |1 5| |0 1|
            ! -----------------
            !  0 0  |1 0| |6 2|
            !  0 0  |0 1| |2 6|
            dense = transpose(reshape(real([2,1,0,3,0,0, &
                                            1,2,4,0,0,0, &
                                            0,4,5,1,1,0, &
                                            3,0,1,5,0,1, &
                                            0,0,1,0,6,2, &
                                            0,0,0,1,2,6],kind=wp),[6,6]))

            ! lower triangle of blocks
            call BSR(1)%malloc([2,2],3,3,5)
            BSR(1)%storage = sparse_lower
            BSR(1)%rowptr = [1,2,4,6]
            BSR(1)%col    = [1,1,2,2,3]
            BSR(1)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(1)%data(:,:,2) = reshape(real([0,3,4,0],kind=wp),[2,2])
            BSR(1)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(1)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(1)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            ! upper triangle of blocks
            call BSR(2)%malloc([2,2],3,3,5)
            BSR(2)%storage = sparse_upper
            BSR(2)%rowptr = [1,3,5,6]
            BSR(2)%col    = [1,2,2,3,3]
            BSR(2)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(2)%data(:,:,2) = reshape(real([0,4,3,0],kind=wp),[2,2])
            BSR(2)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(2)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(2)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            vec_x = real([1,2,3,4,5,6],kind=wp)
            do s = 1, 2
                do j = 1, 6
                    do i = 1, 6
                        call check(error, BSR(s)%at(i,j) == dense(i,j), "BSR csp symmetric: at() wrong value" )
                        if (allocated(error)) return
                    end do
                end do

                call spmv( BSR(s), vec_x, vec_y )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), "BSR csp symmetric: spmv" )
                if (allocated(error)) return

                call spmv( BSR(s), vec_x, vec_y, op=sparse_op_transpose )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), &
                    "BSR csp symmetric: spmv transpose" )
                if (allocated(error)) return
            end do
        end block
        block
            integer, parameter :: wp = dp
            type(BSR_cdp_type) :: BSR(2)
            real(wp) :: dense(6,6)
            complex(dp) :: vec_x(6), vec_y(6)
            integer :: s, i, j

            ! |2 1| |0 3|  0 0
            ! |1 2| |4 0|  0 0
            ! -----------------
            ! |0 4| |5 1| |1 0|
            ! |3 0| |1 5| |0 1|
            ! -----------------
            !  0 0  |1 0| |6 2|
            !  0 0  |0 1| |2 6|
            dense = transpose(reshape(real([2,1,0,3,0,0, &
                                            1,2,4,0,0,0, &
                                            0,4,5,1,1,0, &
                                            3,0,1,5,0,1, &
                                            0,0,1,0,6,2, &
                                            0,0,0,1,2,6],kind=wp),[6,6]))

            ! lower triangle of blocks
            call BSR(1)%malloc([2,2],3,3,5)
            BSR(1)%storage = sparse_lower
            BSR(1)%rowptr = [1,2,4,6]
            BSR(1)%col    = [1,1,2,2,3]
            BSR(1)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(1)%data(:,:,2) = reshape(real([0,3,4,0],kind=wp),[2,2])
            BSR(1)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(1)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(1)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            ! upper triangle of blocks
            call BSR(2)%malloc([2,2],3,3,5)
            BSR(2)%storage = sparse_upper
            BSR(2)%rowptr = [1,3,5,6]
            BSR(2)%col    = [1,2,2,3,3]
            BSR(2)%data(:,:,1) = reshape(real([2,1,1,2],kind=wp),[2,2])
            BSR(2)%data(:,:,2) = reshape(real([0,4,3,0],kind=wp),[2,2])
            BSR(2)%data(:,:,3) = reshape(real([5,1,1,5],kind=wp),[2,2])
            BSR(2)%data(:,:,4) = reshape(real([1,0,0,1],kind=wp),[2,2])
            BSR(2)%data(:,:,5) = reshape(real([6,2,2,6],kind=wp),[2,2])

            vec_x = real([1,2,3,4,5,6],kind=wp)
            do s = 1, 2
                do j = 1, 6
                    do i = 1, 6
                        call check(error, BSR(s)%at(i,j) == dense(i,j), "BSR cdp symmetric: at() wrong value" )
                        if (allocated(error)) return
                    end do
                end do

                call spmv( BSR(s), vec_x, vec_y )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), "BSR cdp symmetric: spmv" )
                if (allocated(error)) return

                call spmv( BSR(s), vec_x, vec_y, op=sparse_op_transpose )
                call check(error, all(vec_y == real([16,17,32,32,45,50],kind=wp)), &
                    "BSR cdp symmetric: spmv transpose" )
                if (allocated(error)) return
            end do
        end block
    end subroutine

    subroutine test_bsr_block_sizes(error)
        !> Each block size has its own spmv kernel: run all of them (and the generic one) for every
        !> storage and op against the dense product. Integer entries keep the results exact.
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: shapes(2,10) = reshape([2,2, 3,3, 4,4, 5,5, 6,6, 7,7, 8,8, 9,9, 2,3, 3,1],[2,10])
        integer :: is, storage

        do is = 1, size(shapes,2)
            do storage = sparse_full, sparse_upper
                if( storage /= sparse_full .and. shapes(1,is) /= shapes(2,is) ) cycle
                call check_bsr_sp(error, shapes(1,is), shapes(2,is), storage)
                if (allocated(error)) return
                call check_bsr_dp(error, shapes(1,is), shapes(2,is), storage)
                if (allocated(error)) return
                call check_bsr_csp(error, shapes(1,is), shapes(2,is), storage)
                if (allocated(error)) return
                call check_bsr_cdp(error, shapes(1,is), shapes(2,is), storage)
                if (allocated(error)) return
            end do
        end do
    end subroutine

    subroutine check_bsr_sp(error, br, bc, storage)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: br, bc, storage
        integer, parameter :: wp = sp
        integer, parameter :: nb = 3 !! number of block rows and block columns
        character(1), parameter :: ops(3) = [sparse_op_none, sparse_op_transpose, sparse_op_hermitian]
        type(BSR_sp_type) :: BSR
        real(sp) :: dense(nb*br, nb*bc)
        integer :: bi, bj, i, j, p, iop, m, n
        character(64) :: ctx

        ! blocks on the diagonal, the first super-diagonal and in the corner (3,1)
        dense = zero_sp
        do j = 1, nb*bc
            do i = 1, nb*br
                bi = (i-1)/br + 1; bj = (j-1)/bc + 1
                if( bj /= bi .and. bj /= bi+1 .and. .not.(bi == 3 .and. bj == 1) ) cycle
                dense(i,j) = mod(i+2*j,4) + 1
            end do
        end do
        if( storage /= sparse_full ) dense = dense + transpose(dense)

        ! copy the stored blocks into the BSR matrix
        call BSR%malloc([br,bc], nb, nb, count([((stored(bi,bj), bj=1,nb), bi=1,nb)]))
        BSR%storage = storage
        p = 0
        BSR%rowptr(1) = 1
        do bi = 1, nb
            do bj = 1, nb
                if( .not.stored(bi,bj) ) cycle
                p = p + 1
                BSR%col(p) = bj
                BSR%data(:,:,p) = dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc)
            end do
            BSR%rowptr(bi+1) = p + 1
        end do

        do iop = 1, size(ops)
            write(ctx,'(a,i0,a,i0,a,i0,a)') "BSR sp block ",br,"x",bc," storage ",storage," op "//ops(iop)
            ! op(A) is m x n
            m = merge(nb*br, nb*bc, ops(iop) == sparse_op_none)
            n = merge(nb*bc, nb*br, ops(iop) == sparse_op_none)
            block
                real(sp) :: opA(m,n), x(n), y(m), yref(m), x2(2,n), y2(2,m), y2ref(2,m)

                select case(ops(iop))
                case(sparse_op_none);      opA = dense
                case(sparse_op_transpose); opA = transpose(dense)
                case default;              opA = transpose(dense)
                end select

                ! rank-1: y = 2*op(A)*x - y
                x = [(real(mod(j,3) + 1, wp), j = 1, n)]
                y = [(real(mod(i,2) + 1, wp), i = 1, m)]
                yref = 2*matmul(opA, x) - y
                call spmv(BSR, x, y, alpha=2*one_sp, beta=-one_sp, op=ops(iop))
                call check(error, all(y == yref), trim(ctx)//": rank-1")
                if (allocated(error)) return

                ! rank-2, 2 right-hand sides: y2 = op(A)*x2, with beta = 0 y2 must not be read
                x2 = reshape([(real(mod(j,4) + 1, wp), j = 1, 2*n)], [2,n])
                y2ref = transpose(matmul(opA, transpose(x2)))
                y2 = ieee_value(0._wp, ieee_quiet_nan)
                call spmv(BSR, x2, y2, op=ops(iop))
                call check(error, all(y2 == y2ref), trim(ctx)//": rank-2")
                if (allocated(error)) return
            end block
        end do

    contains

        logical function stored(bi, bj)
            integer, intent(in) :: bi, bj
            stored = any(dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc) /= zero_sp)
            if( storage == sparse_lower ) stored = stored .and. bj <= bi
            if( storage == sparse_upper ) stored = stored .and. bj >= bi
        end function

    end subroutine
    subroutine check_bsr_dp(error, br, bc, storage)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: br, bc, storage
        integer, parameter :: wp = dp
        integer, parameter :: nb = 3 !! number of block rows and block columns
        character(1), parameter :: ops(3) = [sparse_op_none, sparse_op_transpose, sparse_op_hermitian]
        type(BSR_dp_type) :: BSR
        real(dp) :: dense(nb*br, nb*bc)
        integer :: bi, bj, i, j, p, iop, m, n
        character(64) :: ctx

        ! blocks on the diagonal, the first super-diagonal and in the corner (3,1)
        dense = zero_dp
        do j = 1, nb*bc
            do i = 1, nb*br
                bi = (i-1)/br + 1; bj = (j-1)/bc + 1
                if( bj /= bi .and. bj /= bi+1 .and. .not.(bi == 3 .and. bj == 1) ) cycle
                dense(i,j) = mod(i+2*j,4) + 1
            end do
        end do
        if( storage /= sparse_full ) dense = dense + transpose(dense)

        ! copy the stored blocks into the BSR matrix
        call BSR%malloc([br,bc], nb, nb, count([((stored(bi,bj), bj=1,nb), bi=1,nb)]))
        BSR%storage = storage
        p = 0
        BSR%rowptr(1) = 1
        do bi = 1, nb
            do bj = 1, nb
                if( .not.stored(bi,bj) ) cycle
                p = p + 1
                BSR%col(p) = bj
                BSR%data(:,:,p) = dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc)
            end do
            BSR%rowptr(bi+1) = p + 1
        end do

        do iop = 1, size(ops)
            write(ctx,'(a,i0,a,i0,a,i0,a)') "BSR dp block ",br,"x",bc," storage ",storage," op "//ops(iop)
            ! op(A) is m x n
            m = merge(nb*br, nb*bc, ops(iop) == sparse_op_none)
            n = merge(nb*bc, nb*br, ops(iop) == sparse_op_none)
            block
                real(dp) :: opA(m,n), x(n), y(m), yref(m), x2(2,n), y2(2,m), y2ref(2,m)

                select case(ops(iop))
                case(sparse_op_none);      opA = dense
                case(sparse_op_transpose); opA = transpose(dense)
                case default;              opA = transpose(dense)
                end select

                ! rank-1: y = 2*op(A)*x - y
                x = [(real(mod(j,3) + 1, wp), j = 1, n)]
                y = [(real(mod(i,2) + 1, wp), i = 1, m)]
                yref = 2*matmul(opA, x) - y
                call spmv(BSR, x, y, alpha=2*one_dp, beta=-one_dp, op=ops(iop))
                call check(error, all(y == yref), trim(ctx)//": rank-1")
                if (allocated(error)) return

                ! rank-2, 2 right-hand sides: y2 = op(A)*x2, with beta = 0 y2 must not be read
                x2 = reshape([(real(mod(j,4) + 1, wp), j = 1, 2*n)], [2,n])
                y2ref = transpose(matmul(opA, transpose(x2)))
                y2 = ieee_value(0._wp, ieee_quiet_nan)
                call spmv(BSR, x2, y2, op=ops(iop))
                call check(error, all(y2 == y2ref), trim(ctx)//": rank-2")
                if (allocated(error)) return
            end block
        end do

    contains

        logical function stored(bi, bj)
            integer, intent(in) :: bi, bj
            stored = any(dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc) /= zero_dp)
            if( storage == sparse_lower ) stored = stored .and. bj <= bi
            if( storage == sparse_upper ) stored = stored .and. bj >= bi
        end function

    end subroutine
    subroutine check_bsr_csp(error, br, bc, storage)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: br, bc, storage
        integer, parameter :: wp = sp
        integer, parameter :: nb = 3 !! number of block rows and block columns
        character(1), parameter :: ops(3) = [sparse_op_none, sparse_op_transpose, sparse_op_hermitian]
        type(BSR_csp_type) :: BSR
        complex(sp) :: dense(nb*br, nb*bc)
        integer :: bi, bj, i, j, p, iop, m, n
        character(64) :: ctx

        ! blocks on the diagonal, the first super-diagonal and in the corner (3,1)
        dense = zero_csp
        do j = 1, nb*bc
            do i = 1, nb*br
                bi = (i-1)/br + 1; bj = (j-1)/bc + 1
                if( bj /= bi .and. bj /= bi+1 .and. .not.(bi == 3 .and. bj == 1) ) cycle
                dense(i,j) = cmplx(mod(i+2*j,4) + 1, mod(i+j,3) - 1, kind=wp)
            end do
        end do
        if( storage /= sparse_full ) dense = dense + transpose(dense)

        ! copy the stored blocks into the BSR matrix
        call BSR%malloc([br,bc], nb, nb, count([((stored(bi,bj), bj=1,nb), bi=1,nb)]))
        BSR%storage = storage
        p = 0
        BSR%rowptr(1) = 1
        do bi = 1, nb
            do bj = 1, nb
                if( .not.stored(bi,bj) ) cycle
                p = p + 1
                BSR%col(p) = bj
                BSR%data(:,:,p) = dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc)
            end do
            BSR%rowptr(bi+1) = p + 1
        end do

        do iop = 1, size(ops)
            write(ctx,'(a,i0,a,i0,a,i0,a)') "BSR csp block ",br,"x",bc," storage ",storage," op "//ops(iop)
            ! op(A) is m x n
            m = merge(nb*br, nb*bc, ops(iop) == sparse_op_none)
            n = merge(nb*bc, nb*br, ops(iop) == sparse_op_none)
            block
                complex(sp) :: opA(m,n), x(n), y(m), yref(m), x2(2,n), y2(2,m), y2ref(2,m)

                select case(ops(iop))
                case(sparse_op_none);      opA = dense
                case(sparse_op_transpose); opA = transpose(dense)
                case default;              opA = conjg(transpose(dense))
                end select

                ! rank-1: y = 2*op(A)*x - y
                x = [(real(mod(j,3) + 1, wp), j = 1, n)]
                y = [(real(mod(i,2) + 1, wp), i = 1, m)]
                yref = 2*matmul(opA, x) - y
                call spmv(BSR, x, y, alpha=2*one_csp, beta=-one_csp, op=ops(iop))
                call check(error, all(y == yref), trim(ctx)//": rank-1")
                if (allocated(error)) return

                ! rank-2, 2 right-hand sides: y2 = op(A)*x2, with beta = 0 y2 must not be read
                x2 = reshape([(real(mod(j,4) + 1, wp), j = 1, 2*n)], [2,n])
                y2ref = transpose(matmul(opA, transpose(x2)))
                y2 = ieee_value(0._wp, ieee_quiet_nan)
                call spmv(BSR, x2, y2, op=ops(iop))
                call check(error, all(y2 == y2ref), trim(ctx)//": rank-2")
                if (allocated(error)) return
            end block
        end do

    contains

        logical function stored(bi, bj)
            integer, intent(in) :: bi, bj
            stored = any(dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc) /= zero_csp)
            if( storage == sparse_lower ) stored = stored .and. bj <= bi
            if( storage == sparse_upper ) stored = stored .and. bj >= bi
        end function

    end subroutine
    subroutine check_bsr_cdp(error, br, bc, storage)
        type(error_type), allocatable, intent(out) :: error
        integer, intent(in) :: br, bc, storage
        integer, parameter :: wp = dp
        integer, parameter :: nb = 3 !! number of block rows and block columns
        character(1), parameter :: ops(3) = [sparse_op_none, sparse_op_transpose, sparse_op_hermitian]
        type(BSR_cdp_type) :: BSR
        complex(dp) :: dense(nb*br, nb*bc)
        integer :: bi, bj, i, j, p, iop, m, n
        character(64) :: ctx

        ! blocks on the diagonal, the first super-diagonal and in the corner (3,1)
        dense = zero_cdp
        do j = 1, nb*bc
            do i = 1, nb*br
                bi = (i-1)/br + 1; bj = (j-1)/bc + 1
                if( bj /= bi .and. bj /= bi+1 .and. .not.(bi == 3 .and. bj == 1) ) cycle
                dense(i,j) = cmplx(mod(i+2*j,4) + 1, mod(i+j,3) - 1, kind=wp)
            end do
        end do
        if( storage /= sparse_full ) dense = dense + transpose(dense)

        ! copy the stored blocks into the BSR matrix
        call BSR%malloc([br,bc], nb, nb, count([((stored(bi,bj), bj=1,nb), bi=1,nb)]))
        BSR%storage = storage
        p = 0
        BSR%rowptr(1) = 1
        do bi = 1, nb
            do bj = 1, nb
                if( .not.stored(bi,bj) ) cycle
                p = p + 1
                BSR%col(p) = bj
                BSR%data(:,:,p) = dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc)
            end do
            BSR%rowptr(bi+1) = p + 1
        end do

        do iop = 1, size(ops)
            write(ctx,'(a,i0,a,i0,a,i0,a)') "BSR cdp block ",br,"x",bc," storage ",storage," op "//ops(iop)
            ! op(A) is m x n
            m = merge(nb*br, nb*bc, ops(iop) == sparse_op_none)
            n = merge(nb*bc, nb*br, ops(iop) == sparse_op_none)
            block
                complex(dp) :: opA(m,n), x(n), y(m), yref(m), x2(2,n), y2(2,m), y2ref(2,m)

                select case(ops(iop))
                case(sparse_op_none);      opA = dense
                case(sparse_op_transpose); opA = transpose(dense)
                case default;              opA = conjg(transpose(dense))
                end select

                ! rank-1: y = 2*op(A)*x - y
                x = [(real(mod(j,3) + 1, wp), j = 1, n)]
                y = [(real(mod(i,2) + 1, wp), i = 1, m)]
                yref = 2*matmul(opA, x) - y
                call spmv(BSR, x, y, alpha=2*one_cdp, beta=-one_cdp, op=ops(iop))
                call check(error, all(y == yref), trim(ctx)//": rank-1")
                if (allocated(error)) return

                ! rank-2, 2 right-hand sides: y2 = op(A)*x2, with beta = 0 y2 must not be read
                x2 = reshape([(real(mod(j,4) + 1, wp), j = 1, 2*n)], [2,n])
                y2ref = transpose(matmul(opA, transpose(x2)))
                y2 = ieee_value(0._wp, ieee_quiet_nan)
                call spmv(BSR, x2, y2, op=ops(iop))
                call check(error, all(y2 == y2ref), trim(ctx)//": rank-2")
                if (allocated(error)) return
            end block
        end do

    contains

        logical function stored(bi, bj)
            integer, intent(in) :: bi, bj
            stored = any(dense((bi-1)*br+1:bi*br, (bj-1)*bc+1:bj*bc) /= zero_cdp)
            if( storage == sparse_lower ) stored = stored .and. bj <= bi
            if( storage == sparse_upper ) stored = stored .and. bj >= bi
        end function

    end subroutine

    subroutine test_csr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(CSR_sp_type) :: CSR
            real(sp), parameter :: alpha = 1._wp
            real(sp), parameter :: beta = 0._wp
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y(:)

            call CSR%malloc(4,5,10)
            CSR%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            CSR%col(:)    = [1,5,1,2,2,3,4,1,3,4]
            CSR%rowptr(:) = [1,3,5,8,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            ! High-level API
            call spmv( CSR, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'CSR - high-level API - wrong output' )
            if (allocated(error)) return

            ! Low-level API
            call spmv_kernel_csr(sparse_op_none, alpha, &
                CSR%data, CSR%col, CSR%rowptr, CSR%storage, &
                vec_x, beta, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'CSR - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            ! High-level API
            call spmv( CSR, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'CSR transpose - high-level API - wrong output' )
            if (allocated(error)) return

            call spmv_kernel_csr(sparse_op_transpose, alpha, &
                CSR%data, CSR%col, CSR%rowptr, CSR%storage, &
                vec_y, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'CSR transpose - low-level API - wrong output' )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(CSR_dp_type) :: CSR
            real(dp), parameter :: alpha = 1._wp
            real(dp), parameter :: beta = 0._wp
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y(:)

            call CSR%malloc(4,5,10)
            CSR%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            CSR%col(:)    = [1,5,1,2,2,3,4,1,3,4]
            CSR%rowptr(:) = [1,3,5,8,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            ! High-level API
            call spmv( CSR, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'CSR - high-level API - wrong output' )
            if (allocated(error)) return

            ! Low-level API
            call spmv_kernel_csr(sparse_op_none, alpha, &
                CSR%data, CSR%col, CSR%rowptr, CSR%storage, &
                vec_x, beta, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'CSR - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            ! High-level API
            call spmv( CSR, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'CSR transpose - high-level API - wrong output' )
            if (allocated(error)) return

            call spmv_kernel_csr(sparse_op_transpose, alpha, &
                CSR%data, CSR%col, CSR%rowptr, CSR%storage, &
                vec_y, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'CSR transpose - low-level API - wrong output' )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_csc(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(CSC_sp_type) :: CSC
            real(sp), parameter :: alpha = 1._wp
            real(sp), parameter :: beta = 0._wp
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y(:)

            call CSC%malloc(4,5,10)
            CSC%data(:)   = real([9,4,4,7,8,-1,5,8,6,-3],kind=wp)
            CSC%row(:)    = [1,2,4,2,3,3,4,3,4,1]
            CSC%colptr(:) = [1,4,6,8,10,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            !High-level API
            call spmv( CSC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            !High-level API
            call spmv_kernel_csc(sparse_op_none, alpha, &
                CSC%data, CSC%colptr, CSC%row, CSC%storage, &
                vec_x, beta, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'CSC - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            !High-level API
            call spmv( CSC, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_csc(sparse_op_transpose, alpha, &
                CSC%data, CSC%colptr, CSC%row, CSC%storage, &
                vec_y, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'CSC tranpose - low-level API - wrong output' )
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = dp
            type(CSC_dp_type) :: CSC
            real(dp), parameter :: alpha = 1._wp
            real(dp), parameter :: beta = 0._wp
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y(:)

            call CSC%malloc(4,5,10)
            CSC%data(:)   = real([9,4,4,7,8,-1,5,8,6,-3],kind=wp)
            CSC%row(:)    = [1,2,4,2,3,3,4,3,4,1]
            CSC%colptr(:) = [1,4,6,8,10,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            !High-level API
            call spmv( CSC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            !High-level API
            call spmv_kernel_csc(sparse_op_none, alpha, &
                CSC%data, CSC%colptr, CSC%row, CSC%storage, &
                vec_x, beta, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'CSC - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            !High-level API
            call spmv( CSC, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_csc(sparse_op_transpose, alpha, &
                CSC%data, CSC%colptr, CSC%row, CSC%storage, &
                vec_y, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'CSC tranpose - low-level API - wrong output' )
            if (allocated(error)) return

        end block
    end subroutine

    subroutine test_ell(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(ELL_sp_type) :: ELL
            real(sp), parameter :: alpha = 1._wp
            real(sp), parameter :: beta = 0._wp
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y(:)

            call ELL%malloc(4,5,3)
            ELL%data(1,1:3)   = real([9,-3,0],kind=wp)
            ELL%data(2,1:3)   = real([4,7,0],kind=wp)
            ELL%data(3,1:3)   = real([8,-1,8],kind=wp)
            ELL%data(4,1:3)   = real([4,5,6],kind=wp)

            ELL%index(1,1:3) = [1,5,0]
            ELL%index(2,1:3) = [1,2,0]
            ELL%index(3,1:3) = [2,3,4]
            ELL%index(4,1:3) = [1,3,4]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            !High-level API
            call spmv( ELL, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_ell(sparse_op_none, alpha, &
                ELL%data, ELL%index, ELL%storage, &
                vec_x, beta, vec_y )

            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'ELL - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            !High-level API
            call spmv( ELL, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_ell(sparse_op_transpose, alpha, &
                ELL%data, ELL%index, ELL%storage, &
                vec_y, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'ELL tranpose - low-level API - wrong output' )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(ELL_dp_type) :: ELL
            real(dp), parameter :: alpha = 1._wp
            real(dp), parameter :: beta = 0._wp
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y(:)

            call ELL%malloc(4,5,3)
            ELL%data(1,1:3)   = real([9,-3,0],kind=wp)
            ELL%data(2,1:3)   = real([4,7,0],kind=wp)
            ELL%data(3,1:3)   = real([8,-1,8],kind=wp)
            ELL%data(4,1:3)   = real([4,5,6],kind=wp)

            ELL%index(1,1:3) = [1,5,0]
            ELL%index(2,1:3) = [1,2,0]
            ELL%index(3,1:3) = [2,3,4]
            ELL%index(4,1:3) = [1,3,4]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            !High-level API
            call spmv( ELL, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_ell(sparse_op_none, alpha, &
                ELL%data, ELL%index, ELL%storage, &
                vec_x, beta, vec_y )

            call check(error, all(vec_y == real([6,11,15,15],kind=wp)), 'ELL - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            !High-level API
            call spmv( ELL, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_ell(sparse_op_transpose, alpha, &
                ELL%data, ELL%index, ELL%storage, &
                vec_y, beta, vec_x)
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)), 'ELL tranpose - low-level API - wrong output' )
            if (allocated(error)) return
        end block
        
    end subroutine

    subroutine test_sellc(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(SELLC_sp_type) :: SELLC
            type(CSR_sp_type)   :: CSR
            real(sp), parameter :: alpha = 1._wp
            real(sp), parameter :: beta = 0._wp
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y(:), vec_y2(:)
            integer :: i

            call CSR%malloc(6,6,17)
            !           1   2   3   4   5   6 
            CSR%col = [ 1,      3,  4,         &
                            2,  3,      5,  6, &
                        1,  2,  3,             &
                                        5,  6, &
                                    4,  5,     &
                            2,          5,  6]
            CSR%rowptr = [1,4,8,11,13,15,18]
            CSR%data = [(real(i,kind=wp),i=1,CSR%nnz)] 
            
            call csr2sellc(CSR,SELLC,4)

            allocate( vec_x(6) , source = 1._wp )
            allocate( vec_y(6) , source = 0._wp )
            
            !High-level API
            call spmv( SELLC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,22,27,23,27,48],kind=wp)), 'SELLC - high-level API - wrong output' )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_sellc(sparse_op_none, alpha, &
                SELLC%data, SELLC%rowptr, SELLC%col, SELLC%storage, &
                vec_x, beta, vec_y )

            call check(error, all(vec_y == real([6,22,27,23,27,48],kind=wp)), 'SELLC - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_x = real( [1,2,3,4,5,6] , kind=wp )
            call spmv( CSR, vec_x, vec_y , op = sparse_op_transpose )
            allocate( vec_y2(6) , source = 0._wp )
            !High-level API
            call spmv( SELLC, vec_x, vec_y2 , op = sparse_op_transpose )

            call check(error, all(vec_y == vec_y2), 'SELLC tranpose - high-level API - wrong output' )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_sellc(sparse_op_transpose, alpha, &
                SELLC%data, SELLC%rowptr, SELLC%col, SELLC%storage, &
                vec_x, beta, vec_y2)

            call check(error, all(vec_y == vec_y2), 'SELLC tranpose - low-level API - wrong output' )
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = dp
            type(SELLC_dp_type) :: SELLC
            type(CSR_dp_type)   :: CSR
            real(dp), parameter :: alpha = 1._wp
            real(dp), parameter :: beta = 0._wp
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y(:), vec_y2(:)
            integer :: i

            call CSR%malloc(6,6,17)
            !           1   2   3   4   5   6 
            CSR%col = [ 1,      3,  4,         &
                            2,  3,      5,  6, &
                        1,  2,  3,             &
                                        5,  6, &
                                    4,  5,     &
                            2,          5,  6]
            CSR%rowptr = [1,4,8,11,13,15,18]
            CSR%data = [(real(i,kind=wp),i=1,CSR%nnz)] 
            
            call csr2sellc(CSR,SELLC,4)

            allocate( vec_x(6) , source = 1._wp )
            allocate( vec_y(6) , source = 0._wp )
            
            !High-level API
            call spmv( SELLC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,22,27,23,27,48],kind=wp)), 'SELLC - high-level API - wrong output' )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_sellc(sparse_op_none, alpha, &
                SELLC%data, SELLC%rowptr, SELLC%col, SELLC%storage, &
                vec_x, beta, vec_y )

            call check(error, all(vec_y == real([6,22,27,23,27,48],kind=wp)), 'SELLC - low-level API - wrong output' )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_x = real( [1,2,3,4,5,6] , kind=wp )
            call spmv( CSR, vec_x, vec_y , op = sparse_op_transpose )
            allocate( vec_y2(6) , source = 0._wp )
            !High-level API
            call spmv( SELLC, vec_x, vec_y2 , op = sparse_op_transpose )

            call check(error, all(vec_y == vec_y2), 'SELLC tranpose - high-level API - wrong output' )
            if (allocated(error)) return

            !Low-level API
            call spmv_kernel_sellc(sparse_op_transpose, alpha, &
                SELLC%data, SELLC%rowptr, SELLC%col, SELLC%storage, &
                vec_x, beta, vec_y2)

            call check(error, all(vec_y == vec_y2), 'SELLC tranpose - low-level API - wrong output' )
            if (allocated(error)) return

        end block
    end subroutine

    subroutine test_symmetries(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(COO_sp_type) :: COO_low, COO_upper
            type(CSR_sp_type) :: CSR_low, CSR_upper
            type(CSC_sp_type) :: CSC_low, CSC_upper
            type(ELL_sp_type) :: ELL_low, ELL_upper
            integer, parameter :: ndim = 4
            real(sp), parameter :: dense(ndim,ndim) = reshape(real([1,2,0,0, &
                                                            2,3,4,0, &
                                                            0,4,5,6, &
                                                            0,0,6,7], kind=wp), [ndim,ndim])
            real(sp), parameter :: vec_x(ndim) = real([1,2,3,4], kind=wp)
            real(sp), parameter :: vec_y_ref(ndim) = matmul(dense, vec_x)
            real(sp) :: dense_low(ndim,ndim), dense_upper(ndim,ndim)
            real(sp) :: vec_y(ndim)
            integer :: i, j

            vec_y = 0._wp

            dense_low = dense
            dense_upper = dense

            do i = 1, ndim
                do j = i + 1, ndim
                    dense_low(i,j) = 0._wp
                    dense_upper(j,i) = 0._wp
                end do
            end do

            call dense2coo(dense_low, COO_low)
            COO_low%storage = sparse_lower
            call coo2csr(COO_low, CSR_low)
            call coo2csc(COO_low, CSC_low)
            call csr2ell(CSR_low, ELL_low)

            call dense2coo(dense_upper, COO_upper)
            COO_upper%storage = sparse_upper
            call coo2csr(COO_upper, CSR_upper)
            call coo2csc(COO_upper, CSC_upper)
            call csr2ell(CSR_upper, ELL_upper)

            vec_y = 0._wp
            call spmv(COO_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in COO lower sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(COO_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in COO lower sp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(COO_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in COO upper sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(COO_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in COO upper sp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSR lower sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSR lower sp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSR upper sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSR upper sp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSC lower sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSC lower sp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSC upper sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSC upper sp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in ELL lower sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in ELL lower sp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in ELL upper sp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in ELL upper sp transpose spmv")
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(COO_dp_type) :: COO_low, COO_upper
            type(CSR_dp_type) :: CSR_low, CSR_upper
            type(CSC_dp_type) :: CSC_low, CSC_upper
            type(ELL_dp_type) :: ELL_low, ELL_upper
            integer, parameter :: ndim = 4
            real(dp), parameter :: dense(ndim,ndim) = reshape(real([1,2,0,0, &
                                                            2,3,4,0, &
                                                            0,4,5,6, &
                                                            0,0,6,7], kind=wp), [ndim,ndim])
            real(dp), parameter :: vec_x(ndim) = real([1,2,3,4], kind=wp)
            real(dp), parameter :: vec_y_ref(ndim) = matmul(dense, vec_x)
            real(dp) :: dense_low(ndim,ndim), dense_upper(ndim,ndim)
            real(dp) :: vec_y(ndim)
            integer :: i, j

            vec_y = 0._wp

            dense_low = dense
            dense_upper = dense

            do i = 1, ndim
                do j = i + 1, ndim
                    dense_low(i,j) = 0._wp
                    dense_upper(j,i) = 0._wp
                end do
            end do

            call dense2coo(dense_low, COO_low)
            COO_low%storage = sparse_lower
            call coo2csr(COO_low, CSR_low)
            call coo2csc(COO_low, CSC_low)
            call csr2ell(CSR_low, ELL_low)

            call dense2coo(dense_upper, COO_upper)
            COO_upper%storage = sparse_upper
            call coo2csr(COO_upper, CSR_upper)
            call coo2csc(COO_upper, CSC_upper)
            call csr2ell(CSR_upper, ELL_upper)

            vec_y = 0._wp
            call spmv(COO_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in COO lower dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(COO_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in COO lower dp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(COO_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in COO upper dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(COO_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in COO upper dp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSR lower dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSR lower dp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSR upper dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSR_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSR upper dp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSC lower dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSC lower dp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in CSC upper dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(CSC_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in CSC upper dp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_low, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in ELL lower dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_low, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in ELL lower dp transpose spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_upper, vec_x, vec_y)
            call check(error, all(vec_y == vec_y_ref), "error in ELL upper dp spmv")
            if (allocated(error)) return

            vec_y = 0._wp
            call spmv(ELL_upper, vec_x, vec_y, op=sparse_op_transpose)
            call check(error, all(vec_y == vec_y_ref), "error in ELL upper dp transpose spmv")
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_diagonal(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            real(sp), allocatable :: dense(:,:)
            type(COO_sp_type) :: COO
            type(CSR_sp_type) :: CSR
            type(CSC_sp_type) :: CSC
            real(sp), allocatable :: diagonal(:)

            allocate( dense(4,4) , source = &
                    reshape(real([1,0,0,5, &
                                  0,2,0,0, &
                                  0,6,3,0,&
                                  0,0,7,4],kind=wp),[4,4]) )

            call diag(dense,diagonal)
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return

            diagonal = 0.0
            call dense2coo( dense , COO )
            call diag( COO , diagonal )
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return

            diagonal = 0.0
            call coo2csr( COO, CSR )
            call diag( CSR , diagonal )
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return

            diagonal = 0.0
            call coo2csc( COO, CSC )
            call diag( CSC , diagonal )
            print *, 'diagonal csc:', diagonal
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            real(dp), allocatable :: dense(:,:)
            type(COO_dp_type) :: COO
            type(CSR_dp_type) :: CSR
            type(CSC_dp_type) :: CSC
            real(dp), allocatable :: diagonal(:)

            allocate( dense(4,4) , source = &
                    reshape(real([1,0,0,5, &
                                  0,2,0,0, &
                                  0,6,3,0,&
                                  0,0,7,4],kind=wp),[4,4]) )

            call diag(dense,diagonal)
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return

            diagonal = 0.0
            call dense2coo( dense , COO )
            call diag( COO , diagonal )
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return

            diagonal = 0.0
            call coo2csr( COO, CSR )
            call diag( CSR , diagonal )
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return

            diagonal = 0.0
            call coo2csc( COO, CSC )
            call diag( CSC , diagonal )
            print *, 'diagonal csc:', diagonal
            call check(error, all(diagonal == [1,2,3,4]) )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_add_get_values(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            real(wp) :: dense(5,5), mat(2,2)
            type(COO_sp_type) :: COO
            type(CSR_sp_type) :: CSR
            real(sp):: err
            integer :: i, j, locdof(2)

            mat(:,1) = [1,2]; mat(:,2) = [2,1]
            dense = 0._wp
            do i = 0, 3
                dense(1+i:2+i,1+i:2+i) = dense(1+i:2+i,1+i:2+i) + mat
            end do

            call dense2coo(dense,COO)
            call coo2csr(COO,CSR)

            CSR%data = 0._wp

            do i = 0, 3
                locdof(1:2) = [1+i,2+i] 
                call CSR%add(locdof,locdof,mat)
            end do

            call check(error, all(CSR%data == COO%data) )
            if (allocated(error)) return
            err = 0._wp
            do i = 1, 5
                do j = 1, 5
                    err = err + abs(dense(i,j) - CSR%at(i,j))
                end do
            end do
            err = err / 5*5

            call check(error, err <= epsilon(0._wp) )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            real(wp) :: dense(5,5), mat(2,2)
            type(COO_dp_type) :: COO
            type(CSR_dp_type) :: CSR
            real(dp):: err
            integer :: i, j, locdof(2)

            mat(:,1) = [1,2]; mat(:,2) = [2,1]
            dense = 0._wp
            do i = 0, 3
                dense(1+i:2+i,1+i:2+i) = dense(1+i:2+i,1+i:2+i) + mat
            end do

            call dense2coo(dense,COO)
            call coo2csr(COO,CSR)

            CSR%data = 0._wp

            do i = 0, 3
                locdof(1:2) = [1+i,2+i] 
                call CSR%add(locdof,locdof,mat)
            end do

            call check(error, all(CSR%data == COO%data) )
            if (allocated(error)) return
            err = 0._wp
            do i = 1, 5
                do j = 1, 5
                    err = err + abs(dense(i,j) - CSR%at(i,j))
                end do
            end do
            err = err / 5*5

            call check(error, err <= epsilon(0._wp) )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_sparse_operators(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            real(wp), parameter :: tol = 1000*epsilon(0._wp)
            real(wp) :: scalar
            integer :: row(10), col(10)
            real(wp) :: data(10)
            type(COO_sp_type) :: a, b, c
            real(sp):: err

            data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            col(:)    = [1,5,1,2,2,3,4,1,3,4]
            row(:)    = [1,1,2,2,3,3,3,4,4,4]
            
            call from_ijv(a, row, col, data)
            call from_ijv(b, row, col, data)
            scalar = 2.0_wp
            
            ! Test sparse + sparse
            c = a + b
            err = sum( abs( c%data - 2.0_wp*a%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse + sparse" )
            if (allocated(error)) return

            ! Test scalar + sparse
            c = scalar + a
            err = sum( abs( c%data - (scalar + a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar + sparse" )
            if (allocated(error)) return

            ! Test sparse + scalar
            c = a + scalar
            err = sum( abs( c%data - (a%data + scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse + scalar" )
            if (allocated(error)) return

            ! Test sparse * sparse
            c = a * b
            err = sum( abs( c%data - a%data*b%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse * sparse" )
            if (allocated(error)) return

            ! Test scalar * sparse
            c = scalar * a
            err = sum( abs( c%data - (scalar*a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar * sparse" )
            if (allocated(error)) return

            ! Test sparse * scalar
            c = a * scalar
            err = sum( abs( c%data - (a%data*scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse * scalar" )
            if (allocated(error)) return

            ! Test sparse - sparse
            c = a - b
            err = sum( abs( c%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse - sparse" )
            if (allocated(error)) return

            ! Test scalar - sparse
            c = scalar - a
            err = sum( abs( c%data - (scalar - a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar - sparse" )
            if (allocated(error)) return

            ! Test sparse - scalar
            c = a - scalar
            err = sum( abs( c%data - (a%data - scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse - scalar" )
            if (allocated(error)) return

            ! Test sparse / sparse
            c = a / b
            err = sum( abs( c%data - 1.0_wp ) ) / size(data)
            call check(error, err <= tol, "error in sparse / sparse" )
            if (allocated(error)) return

            ! Test scalar / sparse
            c = scalar / a
            err = sum( abs( c%data - (scalar / a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar / sparse" )
            if (allocated(error)) return

            ! Test sparse / scalar
            c = a / scalar
            err = sum( abs( c%data - (a%data / scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse / scalar" )
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = dp
            real(wp), parameter :: tol = 1000*epsilon(0._wp)
            real(wp) :: scalar
            integer :: row(10), col(10)
            real(wp) :: data(10)
            type(COO_dp_type) :: a, b, c
            real(dp):: err

            data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            col(:)    = [1,5,1,2,2,3,4,1,3,4]
            row(:)    = [1,1,2,2,3,3,3,4,4,4]
            
            call from_ijv(a, row, col, data)
            call from_ijv(b, row, col, data)
            scalar = 2.0_wp
            
            ! Test sparse + sparse
            c = a + b
            err = sum( abs( c%data - 2.0_wp*a%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse + sparse" )
            if (allocated(error)) return

            ! Test scalar + sparse
            c = scalar + a
            err = sum( abs( c%data - (scalar + a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar + sparse" )
            if (allocated(error)) return

            ! Test sparse + scalar
            c = a + scalar
            err = sum( abs( c%data - (a%data + scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse + scalar" )
            if (allocated(error)) return

            ! Test sparse * sparse
            c = a * b
            err = sum( abs( c%data - a%data*b%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse * sparse" )
            if (allocated(error)) return

            ! Test scalar * sparse
            c = scalar * a
            err = sum( abs( c%data - (scalar*a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar * sparse" )
            if (allocated(error)) return

            ! Test sparse * scalar
            c = a * scalar
            err = sum( abs( c%data - (a%data*scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse * scalar" )
            if (allocated(error)) return

            ! Test sparse - sparse
            c = a - b
            err = sum( abs( c%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse - sparse" )
            if (allocated(error)) return

            ! Test scalar - sparse
            c = scalar - a
            err = sum( abs( c%data - (scalar - a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar - sparse" )
            if (allocated(error)) return

            ! Test sparse - scalar
            c = a - scalar
            err = sum( abs( c%data - (a%data - scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse - scalar" )
            if (allocated(error)) return

            ! Test sparse / sparse
            c = a / b
            err = sum( abs( c%data - 1.0_wp ) ) / size(data)
            call check(error, err <= tol, "error in sparse / sparse" )
            if (allocated(error)) return

            ! Test scalar / sparse
            c = scalar / a
            err = sum( abs( c%data - (scalar / a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar / sparse" )
            if (allocated(error)) return

            ! Test sparse / scalar
            c = a / scalar
            err = sum( abs( c%data - (a%data / scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse / scalar" )
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = sp
            real(wp), parameter :: tol = 1000*epsilon(0._wp)
            real(wp) :: scalar
            integer :: row(10), col(10)
            real(wp) :: data(10)
            type(CSR_sp_type) :: a, b, c
            real(sp):: err

            data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            col(:)    = [1,5,1,2,2,3,4,1,3,4]
            row(:)    = [1,1,2,2,3,3,3,4,4,4]
            
            call from_ijv(a, row, col, data)
            call from_ijv(b, row, col, data)
            scalar = 2.0_wp
            
            ! Test sparse + sparse
            c = a + b
            err = sum( abs( c%data - 2.0_wp*a%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse + sparse" )
            if (allocated(error)) return

            ! Test scalar + sparse
            c = scalar + a
            err = sum( abs( c%data - (scalar + a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar + sparse" )
            if (allocated(error)) return

            ! Test sparse + scalar
            c = a + scalar
            err = sum( abs( c%data - (a%data + scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse + scalar" )
            if (allocated(error)) return

            ! Test sparse * sparse
            c = a * b
            err = sum( abs( c%data - a%data*b%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse * sparse" )
            if (allocated(error)) return

            ! Test scalar * sparse
            c = scalar * a
            err = sum( abs( c%data - (scalar*a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar * sparse" )
            if (allocated(error)) return

            ! Test sparse * scalar
            c = a * scalar
            err = sum( abs( c%data - (a%data*scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse * scalar" )
            if (allocated(error)) return

            ! Test sparse - sparse
            c = a - b
            err = sum( abs( c%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse - sparse" )
            if (allocated(error)) return

            ! Test scalar - sparse
            c = scalar - a
            err = sum( abs( c%data - (scalar - a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar - sparse" )
            if (allocated(error)) return

            ! Test sparse - scalar
            c = a - scalar
            err = sum( abs( c%data - (a%data - scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse - scalar" )
            if (allocated(error)) return

            ! Test sparse / sparse
            c = a / b
            err = sum( abs( c%data - 1.0_wp ) ) / size(data)
            call check(error, err <= tol, "error in sparse / sparse" )
            if (allocated(error)) return

            ! Test scalar / sparse
            c = scalar / a
            err = sum( abs( c%data - (scalar / a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar / sparse" )
            if (allocated(error)) return

            ! Test sparse / scalar
            c = a / scalar
            err = sum( abs( c%data - (a%data / scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse / scalar" )
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = dp
            real(wp), parameter :: tol = 1000*epsilon(0._wp)
            real(wp) :: scalar
            integer :: row(10), col(10)
            real(wp) :: data(10)
            type(CSR_dp_type) :: a, b, c
            real(dp):: err

            data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            col(:)    = [1,5,1,2,2,3,4,1,3,4]
            row(:)    = [1,1,2,2,3,3,3,4,4,4]
            
            call from_ijv(a, row, col, data)
            call from_ijv(b, row, col, data)
            scalar = 2.0_wp
            
            ! Test sparse + sparse
            c = a + b
            err = sum( abs( c%data - 2.0_wp*a%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse + sparse" )
            if (allocated(error)) return

            ! Test scalar + sparse
            c = scalar + a
            err = sum( abs( c%data - (scalar + a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar + sparse" )
            if (allocated(error)) return

            ! Test sparse + scalar
            c = a + scalar
            err = sum( abs( c%data - (a%data + scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse + scalar" )
            if (allocated(error)) return

            ! Test sparse * sparse
            c = a * b
            err = sum( abs( c%data - a%data*b%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse * sparse" )
            if (allocated(error)) return

            ! Test scalar * sparse
            c = scalar * a
            err = sum( abs( c%data - (scalar*a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar * sparse" )
            if (allocated(error)) return

            ! Test sparse * scalar
            c = a * scalar
            err = sum( abs( c%data - (a%data*scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse * scalar" )
            if (allocated(error)) return

            ! Test sparse - sparse
            c = a - b
            err = sum( abs( c%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse - sparse" )
            if (allocated(error)) return

            ! Test scalar - sparse
            c = scalar - a
            err = sum( abs( c%data - (scalar - a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar - sparse" )
            if (allocated(error)) return

            ! Test sparse - scalar
            c = a - scalar
            err = sum( abs( c%data - (a%data - scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse - scalar" )
            if (allocated(error)) return

            ! Test sparse / sparse
            c = a / b
            err = sum( abs( c%data - 1.0_wp ) ) / size(data)
            call check(error, err <= tol, "error in sparse / sparse" )
            if (allocated(error)) return

            ! Test scalar / sparse
            c = scalar / a
            err = sum( abs( c%data - (scalar / a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar / sparse" )
            if (allocated(error)) return

            ! Test sparse / scalar
            c = a / scalar
            err = sum( abs( c%data - (a%data / scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse / scalar" )
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = sp
            real(wp), parameter :: tol = 1000*epsilon(0._wp)
            real(wp) :: scalar
            integer :: row(10), col(10)
            real(wp) :: data(10)
            type(CSC_sp_type) :: a, b, c
            real(sp):: err

            data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            col(:)    = [1,5,1,2,2,3,4,1,3,4]
            row(:)    = [1,1,2,2,3,3,3,4,4,4]
            
            call from_ijv(a, row, col, data)
            call from_ijv(b, row, col, data)
            scalar = 2.0_wp
            
            ! Test sparse + sparse
            c = a + b
            err = sum( abs( c%data - 2.0_wp*a%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse + sparse" )
            if (allocated(error)) return

            ! Test scalar + sparse
            c = scalar + a
            err = sum( abs( c%data - (scalar + a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar + sparse" )
            if (allocated(error)) return

            ! Test sparse + scalar
            c = a + scalar
            err = sum( abs( c%data - (a%data + scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse + scalar" )
            if (allocated(error)) return

            ! Test sparse * sparse
            c = a * b
            err = sum( abs( c%data - a%data*b%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse * sparse" )
            if (allocated(error)) return

            ! Test scalar * sparse
            c = scalar * a
            err = sum( abs( c%data - (scalar*a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar * sparse" )
            if (allocated(error)) return

            ! Test sparse * scalar
            c = a * scalar
            err = sum( abs( c%data - (a%data*scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse * scalar" )
            if (allocated(error)) return

            ! Test sparse - sparse
            c = a - b
            err = sum( abs( c%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse - sparse" )
            if (allocated(error)) return

            ! Test scalar - sparse
            c = scalar - a
            err = sum( abs( c%data - (scalar - a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar - sparse" )
            if (allocated(error)) return

            ! Test sparse - scalar
            c = a - scalar
            err = sum( abs( c%data - (a%data - scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse - scalar" )
            if (allocated(error)) return

            ! Test sparse / sparse
            c = a / b
            err = sum( abs( c%data - 1.0_wp ) ) / size(data)
            call check(error, err <= tol, "error in sparse / sparse" )
            if (allocated(error)) return

            ! Test scalar / sparse
            c = scalar / a
            err = sum( abs( c%data - (scalar / a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar / sparse" )
            if (allocated(error)) return

            ! Test sparse / scalar
            c = a / scalar
            err = sum( abs( c%data - (a%data / scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse / scalar" )
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = dp
            real(wp), parameter :: tol = 1000*epsilon(0._wp)
            real(wp) :: scalar
            integer :: row(10), col(10)
            real(wp) :: data(10)
            type(CSC_dp_type) :: a, b, c
            real(dp):: err

            data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            col(:)    = [1,5,1,2,2,3,4,1,3,4]
            row(:)    = [1,1,2,2,3,3,3,4,4,4]
            
            call from_ijv(a, row, col, data)
            call from_ijv(b, row, col, data)
            scalar = 2.0_wp
            
            ! Test sparse + sparse
            c = a + b
            err = sum( abs( c%data - 2.0_wp*a%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse + sparse" )
            if (allocated(error)) return

            ! Test scalar + sparse
            c = scalar + a
            err = sum( abs( c%data - (scalar + a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar + sparse" )
            if (allocated(error)) return

            ! Test sparse + scalar
            c = a + scalar
            err = sum( abs( c%data - (a%data + scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse + scalar" )
            if (allocated(error)) return

            ! Test sparse * sparse
            c = a * b
            err = sum( abs( c%data - a%data*b%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse * sparse" )
            if (allocated(error)) return

            ! Test scalar * sparse
            c = scalar * a
            err = sum( abs( c%data - (scalar*a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar * sparse" )
            if (allocated(error)) return

            ! Test sparse * scalar
            c = a * scalar
            err = sum( abs( c%data - (a%data*scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse * scalar" )
            if (allocated(error)) return

            ! Test sparse - sparse
            c = a - b
            err = sum( abs( c%data ) ) / size(data)
            call check(error, err <= tol, "error in sparse - sparse" )
            if (allocated(error)) return

            ! Test scalar - sparse
            c = scalar - a
            err = sum( abs( c%data - (scalar - a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar - sparse" )
            if (allocated(error)) return

            ! Test sparse - scalar
            c = a - scalar
            err = sum( abs( c%data - (a%data - scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse - scalar" )
            if (allocated(error)) return

            ! Test sparse / sparse
            c = a / b
            err = sum( abs( c%data - 1.0_wp ) ) / size(data)
            call check(error, err <= tol, "error in sparse / sparse" )
            if (allocated(error)) return

            ! Test scalar / sparse
            c = scalar / a
            err = sum( abs( c%data - (scalar / a%data) ) ) / size(data)
            call check(error, err <= tol, "error in scalar / sparse" )
            if (allocated(error)) return

            ! Test sparse / scalar
            c = a / scalar
            err = sum( abs( c%data - (a%data / scalar) ) ) / size(data)
            call check(error, err <= tol, "error in sparse / scalar" )
            if (allocated(error)) return

        end block

    end subroutine

    subroutine test_at_symmetric(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: ndim = 5
            real(sp), parameter :: dense(ndim,ndim) = reshape(real([1,2,0,8,0, &
                                                            2,3,4,0,0, &
                                                            0,4,5,6,0, &
                                                            8,0,6,7,9, &
                                                            0,0,0,9,1], kind=wp), [ndim,ndim])
            real(sp) :: dense_tri(ndim,ndim)
            type(COO_sp_type) :: COO
            type(CSR_sp_type) :: CSR
            type(CSC_sp_type) :: CSC
            type(ELL_sp_type) :: ELL
            type(SELLC_sp_type) :: SELLC
            integer :: storage, i, j
            logical :: ok(5)

            do storage = sparse_full, sparse_upper
                dense_tri = dense
                do j = 1, ndim
                    do i = 1, ndim
                        if( storage == sparse_lower .and. i < j ) dense_tri(i,j) = 0._wp
                        if( storage == sparse_upper .and. i > j ) dense_tri(i,j) = 0._wp
                    end do
                end do

                call dense2coo(dense_tri, COO)
                COO%storage = storage
                call coo2csr(COO, CSR)
                call coo2csc(COO, CSC)
                call csr2ell(CSR, ELL)
                call csr2sellc(CSR, SELLC, 2)

                do j = 1, ndim
                    do i = 1, ndim
                        ok(1) = COO%at(i,j) == dense(i,j)
                        ok(2) = CSR%at(i,j) == dense(i,j)
                        ok(3) = CSC%at(i,j) == dense(i,j)
                        ok(4) = ELL%at(i,j) == dense(i,j)
                        ok(5) = SELLC%at(i,j) == dense(i,j)
                        call check(error, all(ok), "error in sp at() with storage "//char(48+storage)// &
                            ", formats COO/CSR/CSC/ELL/SELLC: "//merge('T','F',ok(1))//merge('T','F',ok(2))// &
                            merge('T','F',ok(3))//merge('T','F',ok(4))//merge('T','F',ok(5)))
                        if (allocated(error)) return
                    end do
                end do
            end do
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: ndim = 5
            real(dp), parameter :: dense(ndim,ndim) = reshape(real([1,2,0,8,0, &
                                                            2,3,4,0,0, &
                                                            0,4,5,6,0, &
                                                            8,0,6,7,9, &
                                                            0,0,0,9,1], kind=wp), [ndim,ndim])
            real(dp) :: dense_tri(ndim,ndim)
            type(COO_dp_type) :: COO
            type(CSR_dp_type) :: CSR
            type(CSC_dp_type) :: CSC
            type(ELL_dp_type) :: ELL
            type(SELLC_dp_type) :: SELLC
            integer :: storage, i, j
            logical :: ok(5)

            do storage = sparse_full, sparse_upper
                dense_tri = dense
                do j = 1, ndim
                    do i = 1, ndim
                        if( storage == sparse_lower .and. i < j ) dense_tri(i,j) = 0._wp
                        if( storage == sparse_upper .and. i > j ) dense_tri(i,j) = 0._wp
                    end do
                end do

                call dense2coo(dense_tri, COO)
                COO%storage = storage
                call coo2csr(COO, CSR)
                call coo2csc(COO, CSC)
                call csr2ell(CSR, ELL)
                call csr2sellc(CSR, SELLC, 2)

                do j = 1, ndim
                    do i = 1, ndim
                        ok(1) = COO%at(i,j) == dense(i,j)
                        ok(2) = CSR%at(i,j) == dense(i,j)
                        ok(3) = CSC%at(i,j) == dense(i,j)
                        ok(4) = ELL%at(i,j) == dense(i,j)
                        ok(5) = SELLC%at(i,j) == dense(i,j)
                        call check(error, all(ok), "error in dp at() with storage "//char(48+storage)// &
                            ", formats COO/CSR/CSC/ELL/SELLC: "//merge('T','F',ok(1))//merge('T','F',ok(2))// &
                            merge('T','F',ok(3))//merge('T','F',ok(4))//merge('T','F',ok(5)))
                        if (allocated(error)) return
                    end do
                end do
            end do
        end block
    end subroutine

    subroutine test_sellc_add_get_values(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            real(wp) :: dense(5,5), mat(2,2)
            type(COO_sp_type) :: COO
            type(CSR_sp_type) :: CSR
            type(SELLC_sp_type) :: SELLC, SELLC_ref
            integer :: i, j, locdof(2)

            mat(:,1) = [1,2]; mat(:,2) = [2,1]
            dense = 0._wp
            do i = 0, 3
                dense(1+i:2+i,1+i:2+i) = dense(1+i:2+i,1+i:2+i) + mat
            end do

            call dense2coo(dense,COO)
            call coo2csr(COO,CSR)
            call csr2sellc(CSR,SELLC_ref,2)
            call csr2sellc(CSR,SELLC,2)

            ! block assembly
            SELLC%data = 0._wp
            do i = 0, 3
                locdof(1:2) = [1+i,2+i]
                call SELLC%add(locdof,locdof,mat)
            end do
            call check(error, all(SELLC%data == SELLC_ref%data), "error in SELLC sp add block" )
            if (allocated(error)) return

            ! element-wise assembly
            SELLC%data = 0._wp
            do j = 1, 5
                do i = 1, 5
                    if( dense(i,j) /= 0._wp ) call SELLC%add(i,j,dense(i,j))
                end do
            end do
            call check(error, all(SELLC%data == SELLC_ref%data), "error in SELLC sp add value" )
            if (allocated(error)) return

            do j = 1, 5
                do i = 1, 5
                    call check(error, SELLC%at(i,j) == dense(i,j), "error in SELLC sp at()" )
                    if (allocated(error)) return
                end do
            end do
        end block
        block
            integer, parameter :: wp = dp
            real(wp) :: dense(5,5), mat(2,2)
            type(COO_dp_type) :: COO
            type(CSR_dp_type) :: CSR
            type(SELLC_dp_type) :: SELLC, SELLC_ref
            integer :: i, j, locdof(2)

            mat(:,1) = [1,2]; mat(:,2) = [2,1]
            dense = 0._wp
            do i = 0, 3
                dense(1+i:2+i,1+i:2+i) = dense(1+i:2+i,1+i:2+i) + mat
            end do

            call dense2coo(dense,COO)
            call coo2csr(COO,CSR)
            call csr2sellc(CSR,SELLC_ref,2)
            call csr2sellc(CSR,SELLC,2)

            ! block assembly
            SELLC%data = 0._wp
            do i = 0, 3
                locdof(1:2) = [1+i,2+i]
                call SELLC%add(locdof,locdof,mat)
            end do
            call check(error, all(SELLC%data == SELLC_ref%data), "error in SELLC dp add block" )
            if (allocated(error)) return

            ! element-wise assembly
            SELLC%data = 0._wp
            do j = 1, 5
                do i = 1, 5
                    if( dense(i,j) /= 0._wp ) call SELLC%add(i,j,dense(i,j))
                end do
            end do
            call check(error, all(SELLC%data == SELLC_ref%data), "error in SELLC dp add value" )
            if (allocated(error)) return

            do j = 1, 5
                do i = 1, 5
                    call check(error, SELLC%at(i,j) == dense(i,j), "error in SELLC dp at()" )
                    if (allocated(error)) return
                end do
            end do
        end block
    end subroutine

    subroutine test_add_block_symmetric_skip(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer :: connectivity(3,3)

            real(wp) :: dense(5,5), dense_low(5,5), mat(3,3)
            type(COO_sp_type) :: COO_full, COO_low
            type(CSR_sp_type) :: CSR_full, CSR_low
            type(CSC_sp_type) :: CSC_full, CSC_low
            real(wp)  :: x(5), y(5), y_ref(5)
            real(sp):: err
            integer :: i, j, locdof(3)

            connectivity(1:3,1) = [1,2,3]
            connectivity(1:3,2) = [2,3,4]
            connectivity(1:3,3) = [3,4,5]

            mat(:,1) = [1,2,3]
            mat(:,2) = [2,1,4]
            mat(:,3) = [3,4,1]

            dense = 0._wp
            do i = 1, 3
                locdof(1:3) = connectivity(1:3,i)
                dense(locdof,locdof) = dense(locdof,locdof) + mat
            end do

            call dense2coo(dense,COO_full)
            call coo2csr(COO_full,CSR_full)
            call coo2csc(COO_full,CSC_full)
            dense_low = dense
            do i = 1, 5
                do j = i+1, 5
                    dense_low(i,j) = 0._wp
                end do
            end do
            call dense2coo(dense_low,COO_low)
            COO_low%storage = sparse_lower
            call coo2csr(COO_low,CSR_low)
            call coo2csc(COO_low,CSC_low)

            COO_full%data = 0._wp
            COO_low%data = 0._wp
            CSR_full%data = 0._wp
            CSR_low%data = 0._wp
            CSC_full%data = 0._wp
            CSC_low%data = 0._wp
            do i = 1, 3
                locdof(1:3) = connectivity(1:3,i)
                call COO_full%add(locdof,locdof,mat)
                call COO_low%add(locdof,locdof,mat)
                call CSR_full%add(locdof,locdof,mat)
                call CSR_low%add(locdof,locdof,mat)
                call CSC_full%add(locdof,locdof,mat)
                call CSC_low%add(locdof,locdof,mat)
            end do

            call check(error, all(CSR_full%data == COO_full%data) , "error in full CSR sp data" )
            if (allocated(error)) return

            call check(error, all(CSR_low%data == COO_low%data) , "error in low CSR sp data" )
            if (allocated(error)) return

            x = 1._wp
            y_ref = matmul(dense,x)

            y = 0._wp
            call spmv( CSR_full, x, y )
            call check(error, all(y == y_ref) , "error in full CSR sp spmv" )
            if (allocated(error)) return
            
            y = 0._wp
            call spmv( CSR_low, x, y ) 
            call check(error, all(y == y_ref) , "error in low CSR sp spmv" )
            if (allocated(error)) return

            y = 0._wp
            call spmv( CSC_full, x, y )
            call check(error, all(y == y_ref) , "error in full CSC sp spmv" )
            if (allocated(error)) return
            
            y = 0._wp
            call spmv( CSC_low, x, y ) 
            call check(error, all(y == y_ref) , "error in low CSC sp spmv" )
        end block
        block
            integer, parameter :: wp = dp
            integer :: connectivity(3,3)

            real(wp) :: dense(5,5), dense_low(5,5), mat(3,3)
            type(COO_dp_type) :: COO_full, COO_low
            type(CSR_dp_type) :: CSR_full, CSR_low
            type(CSC_dp_type) :: CSC_full, CSC_low
            real(wp)  :: x(5), y(5), y_ref(5)
            real(dp):: err
            integer :: i, j, locdof(3)

            connectivity(1:3,1) = [1,2,3]
            connectivity(1:3,2) = [2,3,4]
            connectivity(1:3,3) = [3,4,5]

            mat(:,1) = [1,2,3]
            mat(:,2) = [2,1,4]
            mat(:,3) = [3,4,1]

            dense = 0._wp
            do i = 1, 3
                locdof(1:3) = connectivity(1:3,i)
                dense(locdof,locdof) = dense(locdof,locdof) + mat
            end do

            call dense2coo(dense,COO_full)
            call coo2csr(COO_full,CSR_full)
            call coo2csc(COO_full,CSC_full)
            dense_low = dense
            do i = 1, 5
                do j = i+1, 5
                    dense_low(i,j) = 0._wp
                end do
            end do
            call dense2coo(dense_low,COO_low)
            COO_low%storage = sparse_lower
            call coo2csr(COO_low,CSR_low)
            call coo2csc(COO_low,CSC_low)

            COO_full%data = 0._wp
            COO_low%data = 0._wp
            CSR_full%data = 0._wp
            CSR_low%data = 0._wp
            CSC_full%data = 0._wp
            CSC_low%data = 0._wp
            do i = 1, 3
                locdof(1:3) = connectivity(1:3,i)
                call COO_full%add(locdof,locdof,mat)
                call COO_low%add(locdof,locdof,mat)
                call CSR_full%add(locdof,locdof,mat)
                call CSR_low%add(locdof,locdof,mat)
                call CSC_full%add(locdof,locdof,mat)
                call CSC_low%add(locdof,locdof,mat)
            end do

            call check(error, all(CSR_full%data == COO_full%data) , "error in full CSR dp data" )
            if (allocated(error)) return

            call check(error, all(CSR_low%data == COO_low%data) , "error in low CSR dp data" )
            if (allocated(error)) return

            x = 1._wp
            y_ref = matmul(dense,x)

            y = 0._wp
            call spmv( CSR_full, x, y )
            call check(error, all(y == y_ref) , "error in full CSR dp spmv" )
            if (allocated(error)) return
            
            y = 0._wp
            call spmv( CSR_low, x, y ) 
            call check(error, all(y == y_ref) , "error in low CSR dp spmv" )
            if (allocated(error)) return

            y = 0._wp
            call spmv( CSC_full, x, y )
            call check(error, all(y == y_ref) , "error in full CSC dp spmv" )
            if (allocated(error)) return
            
            y = 0._wp
            call spmv( CSC_low, x, y ) 
            call check(error, all(y == y_ref) , "error in low CSC dp spmv" )
        end block
        
    end subroutine

    subroutine test_coo2dense(error)
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: rows(*) = [3, 2, 2]
            integer, parameter :: cols(*) = [2, 3, 2]
            integer :: i, j, matrix
            type(COO_sp_type) :: coo
            real(sp), allocatable :: original(:,:), converted(:,:)

            ! Tall, wide and square inputs must retain both shape and values.
            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do

                call dense2coo(original, coo)
                call coo2dense(coo, converted)
                call check(error, all(shape(converted) == shape(original)), &
                    'coo2dense: wrong shape for sp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'coo2dense: wrong values for sp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: rows(*) = [3, 2, 2]
            integer, parameter :: cols(*) = [2, 3, 2]
            integer :: i, j, matrix
            type(COO_dp_type) :: coo
            real(dp), allocatable :: original(:,:), converted(:,:)

            ! Tall, wide and square inputs must retain both shape and values.
            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do

                call dense2coo(original, coo)
                call coo2dense(coo, converted)
                call check(error, all(shape(converted) == shape(original)), &
                    'coo2dense: wrong shape for dp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'coo2dense: wrong values for dp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
        block
            integer, parameter :: wp = sp
            integer, parameter :: rows(*) = [3, 2, 2]
            integer, parameter :: cols(*) = [2, 3, 2]
            integer :: i, j, matrix
            type(COO_csp_type) :: coo
            complex(sp), allocatable :: original(:,:), converted(:,:)

            ! Tall, wide and square inputs must retain both shape and values.
            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do
                original(1, size(original, 2)) = cmplx(7, -2, kind=wp)

                call dense2coo(original, coo)
                call coo2dense(coo, converted)
                call check(error, all(shape(converted) == shape(original)), &
                    'coo2dense: wrong shape for csp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'coo2dense: wrong values for csp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: rows(*) = [3, 2, 2]
            integer, parameter :: cols(*) = [2, 3, 2]
            integer :: i, j, matrix
            type(COO_cdp_type) :: coo
            complex(dp), allocatable :: original(:,:), converted(:,:)

            ! Tall, wide and square inputs must retain both shape and values.
            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do
                original(1, size(original, 2)) = cmplx(7, -2, kind=wp)

                call dense2coo(original, coo)
                call coo2dense(coo, converted)
                call check(error, all(shape(converted) == shape(original)), &
                    'coo2dense: wrong shape for cdp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'coo2dense: wrong values for cdp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
    end subroutine test_coo2dense

    subroutine test_csr2dense(error)
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            integer, parameter :: rows(*) = [3, 2, 2, 3]
            integer, parameter :: cols(*) = [2, 3, 2, 3]
            integer :: i, j, matrix
            type(COO_sp_type) :: coo
            type(CSR_sp_type) :: csr
            real(sp), allocatable :: original(:,:), converted(:,:)

            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do
                if (matrix == 4) then
                    ! Keep lower-triangle symmetric storage working as well.
                    original = reshape(real([1,0,2, 0,3,0, 0,0,4], wp), [3,3])
                end if

                call dense2coo(original, coo)
                if (matrix == 4) coo%storage = sparse_lower
                call coo2csr(coo, csr)
                call csr2dense(csr, converted)
                if (matrix == 4) then
                    original = reshape(real([1,0,2, 0,3,0, 2,0,4], wp), [3,3])
                end if
                call check(error, all(shape(converted) == shape(original)), &
                    'csr2dense: wrong shape for sp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'csr2dense: wrong values for sp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: rows(*) = [3, 2, 2, 3]
            integer, parameter :: cols(*) = [2, 3, 2, 3]
            integer :: i, j, matrix
            type(COO_dp_type) :: coo
            type(CSR_dp_type) :: csr
            real(dp), allocatable :: original(:,:), converted(:,:)

            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do
                if (matrix == 4) then
                    ! Keep lower-triangle symmetric storage working as well.
                    original = reshape(real([1,0,2, 0,3,0, 0,0,4], wp), [3,3])
                end if

                call dense2coo(original, coo)
                if (matrix == 4) coo%storage = sparse_lower
                call coo2csr(coo, csr)
                call csr2dense(csr, converted)
                if (matrix == 4) then
                    original = reshape(real([1,0,2, 0,3,0, 2,0,4], wp), [3,3])
                end if
                call check(error, all(shape(converted) == shape(original)), &
                    'csr2dense: wrong shape for dp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'csr2dense: wrong values for dp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
        block
            integer, parameter :: wp = sp
            integer, parameter :: rows(*) = [3, 2, 2, 3]
            integer, parameter :: cols(*) = [2, 3, 2, 3]
            integer :: i, j, matrix
            type(COO_csp_type) :: coo
            type(CSR_csp_type) :: csr
            complex(sp), allocatable :: original(:,:), converted(:,:)

            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do
                original(1, size(original, 2)) = cmplx(7, -2, kind=wp)
                if (matrix == 4) then
                    ! Keep lower-triangle symmetric storage working as well.
                    original = reshape(real([1,0,2, 0,3,0, 0,0,4], wp), [3,3])
                end if

                call dense2coo(original, coo)
                if (matrix == 4) coo%storage = sparse_lower
                call coo2csr(coo, csr)
                call csr2dense(csr, converted)
                if (matrix == 4) then
                    original = reshape(real([1,0,2, 0,3,0, 2,0,4], wp), [3,3])
                end if
                call check(error, all(shape(converted) == shape(original)), &
                    'csr2dense: wrong shape for csp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'csr2dense: wrong values for csp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
        block
            integer, parameter :: wp = dp
            integer, parameter :: rows(*) = [3, 2, 2, 3]
            integer, parameter :: cols(*) = [2, 3, 2, 3]
            integer :: i, j, matrix
            type(COO_cdp_type) :: coo
            type(CSR_cdp_type) :: csr
            complex(dp), allocatable :: original(:,:), converted(:,:)

            do matrix = 1, size(rows)
                allocate(original(rows(matrix), cols(matrix)))
                original = 0
                do j = 1, size(original, 2)
                    do i = 1, size(original, 1)
                        if (mod(i + j, 2) /= 0) original(i,j) = real(10*j + i, wp)
                    end do
                end do
                original(1, size(original, 2)) = cmplx(7, -2, kind=wp)
                if (matrix == 4) then
                    ! Keep lower-triangle symmetric storage working as well.
                    original = reshape(real([1,0,2, 0,3,0, 0,0,4], wp), [3,3])
                end if

                call dense2coo(original, coo)
                if (matrix == 4) coo%storage = sparse_lower
                call coo2csr(coo, csr)
                call csr2dense(csr, converted)
                if (matrix == 4) then
                    original = reshape(real([1,0,2, 0,3,0, 2,0,4], wp), [3,3])
                end if
                call check(error, all(shape(converted) == shape(original)), &
                    'csr2dense: wrong shape for cdp')
                if (allocated(error)) return
                call check(error, all(converted == original), &
                    'csr2dense: wrong values for cdp')
                if (allocated(error)) return
                deallocate(original)
            end do
        end block
    end subroutine test_csr2dense

    subroutine test_csc2dense(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(COO_sp_type) :: COO
            type(CSC_sp_type) :: CSC
            real(sp), allocatable :: dense_original(:,:), dense_converted(:,:)

            ! 1. Create a baseline dense 4x4 matrix
            allocate( dense_original(4,4) , source = &
                    reshape(real([1,0,0,5, &
                                  0,2,0,0, &
                                  0,6,3,0, &
                                  0,0,7,4],kind=wp),[4,4]) )

            ! 2. Convert Dense -> COO -> CSC (Standard pipeline)
            call dense2coo( dense_original , COO )
            call coo2csc( COO, CSC )

            ! 3. Execute the csc2dense conversion
            call csc2dense( CSC, dense_converted )

            ! 4. Verify the converted matrix perfectly matches the original
            call check(error, all(dense_original == dense_converted), "Error: csc2dense conversion failed for kind sp")
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(COO_dp_type) :: COO
            type(CSC_dp_type) :: CSC
            real(dp), allocatable :: dense_original(:,:), dense_converted(:,:)

            ! 1. Create a baseline dense 4x4 matrix
            allocate( dense_original(4,4) , source = &
                    reshape(real([1,0,0,5, &
                                  0,2,0,0, &
                                  0,6,3,0, &
                                  0,0,7,4],kind=wp),[4,4]) )

            ! 2. Convert Dense -> COO -> CSC (Standard pipeline)
            call dense2coo( dense_original , COO )
            call coo2csc( COO, CSC )

            ! 3. Execute the csc2dense conversion
            call csc2dense( CSC, dense_converted )

            ! 4. Verify the converted matrix perfectly matches the original
            call check(error, all(dense_original == dense_converted), "Error: csc2dense conversion failed for kind dp")
            if (allocated(error)) return
        end block
    end subroutine
    
end module

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_sparse_spmv, only : collect_suite
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("sparse", collect_suite) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
