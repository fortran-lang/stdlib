module test_sparse_spmv
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_sparse

    implicit none

contains


    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('coo', test_coo), &
            new_unittest('coo2ordered', test_coo2ordered), &
            new_unittest('csr', test_csr), &
            new_unittest('csc', test_csc), &
            new_unittest('ell', test_ell),  &
            new_unittest('sellc', test_sellc),  &
            new_unittest('symmetries', test_symmetries), &
            new_unittest('diagonal', test_diagonal), &
            new_unittest('add_get_values', test_add_get_values) &
        ]
    end subroutine

    subroutine test_coo(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(COO_sp_type) :: COO
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

            call spmv( COO, vec_x, vec_y2 )
            call check(error, all(vec_y1 == vec_y2) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y1 = 1._wp
            call spmv( COO, vec_y1, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(COO_dp_type) :: COO
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

            call spmv( COO, vec_x, vec_y2 )
            call check(error, all(vec_y1 == vec_y2) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y1 = 1._wp
            call spmv( COO, vec_y1, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
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

    subroutine test_csr(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(CSR_sp_type) :: CSR
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y(:)

            call CSR%malloc(4,5,10)
            CSR%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            CSR%col(:)    = [1,5,1,2,2,3,4,1,3,4]
            CSR%rowptr(:) = [1,3,5,8,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            call spmv( CSR, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            call spmv( CSR, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(CSR_dp_type) :: CSR
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y(:)

            call CSR%malloc(4,5,10)
            CSR%data(:)   = real([9,-3,4,7,8,-1,8,4,5,6],kind=wp)
            CSR%col(:)    = [1,5,1,2,2,3,4,1,3,4]
            CSR%rowptr(:) = [1,3,5,8,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            call spmv( CSR, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            call spmv( CSR, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_csc(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(CSC_sp_type) :: CSC
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y(:)

            call CSC%malloc(4,5,10)
            CSC%data(:)   = real([9,4,4,7,8,-1,5,8,6,-3],kind=wp)
            CSC%row(:)    = [1,2,4,2,3,3,4,3,4,1]
            CSC%colptr(:) = [1,4,6,8,10,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            call spmv( CSC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            call spmv( CSC, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(CSC_dp_type) :: CSC
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y(:)

            call CSC%malloc(4,5,10)
            CSC%data(:)   = real([9,4,4,7,8,-1,5,8,6,-3],kind=wp)
            CSC%row(:)    = [1,2,4,2,3,3,4,3,4,1]
            CSC%colptr(:) = [1,4,6,8,10,11]
            
            allocate( vec_x(5) , source = 1._wp )
            allocate( vec_y(4) , source = 0._wp )
            call spmv( CSC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            call spmv( CSC, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_ell(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(ELL_sp_type) :: ELL
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
            call spmv( ELL, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            call spmv( ELL, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(ELL_dp_type) :: ELL
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
            call spmv( ELL, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,11,15,15],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_y = 1._wp
            call spmv( ELL, vec_y, vec_x, op=sparse_op_transpose )
            call check(error, all(vec_x == real([17,15,4,14,-3],kind=wp)) )
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
            
            call spmv( SELLC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,22,27,23,27,48],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_x = real( [1,2,3,4,5,6] , kind=wp )
            call spmv( CSR, vec_x, vec_y , op = sparse_op_transpose )
            allocate( vec_y2(6) , source = 0._wp )
            call spmv( SELLC, vec_x, vec_y2 , op = sparse_op_transpose )
            
            call check(error, all(vec_y == vec_y2))
            if (allocated(error)) return

        end block
        block
            integer, parameter :: wp = dp
            type(SELLC_dp_type) :: SELLC
            type(CSR_dp_type)   :: CSR
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
            
            call spmv( SELLC, vec_x, vec_y )
            
            call check(error, all(vec_y == real([6,22,27,23,27,48],kind=wp)) )
            if (allocated(error)) return

            ! Test in-place transpose
            vec_x = real( [1,2,3,4,5,6] , kind=wp )
            call spmv( CSR, vec_x, vec_y , op = sparse_op_transpose )
            allocate( vec_y2(6) , source = 0._wp )
            call spmv( SELLC, vec_x, vec_y2 , op = sparse_op_transpose )
            
            call check(error, all(vec_y == vec_y2))
            if (allocated(error)) return

        end block
    end subroutine

    subroutine test_symmetries(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        block
            integer, parameter :: wp = sp
            type(COO_sp_type) :: COO
            type(CSR_sp_type) :: CSR
            type(ELL_sp_type) :: ELL
            real(sp), allocatable :: dense(:,:)
            real(sp), allocatable :: vec_x(:)
            real(sp), allocatable :: vec_y1(:), vec_y2(:), vec_y3(:), vec_y4(:)

            allocate( vec_x(4)  , source = 1._wp )
            allocate( vec_y1(4) , source = 0._wp )
            allocate( vec_y2(4) , source = 0._wp )
            allocate( vec_y3(4) , source = 0._wp )
            allocate( vec_y4(4) , source = 0._wp )

            allocate( dense(4,4) , source = &
                    reshape(real([1,0,0,0, &
                                  2,1,0,0, &
                                  0,2,1,0,&
                                  0,0,2,1],kind=wp),[4,4]) )

            call dense2coo( dense , COO )
            COO%storage = sparse_upper
            call coo2csr(COO, CSR)
            call csr2ell(CSR, ELL)

            dense(2,1) = 2._wp; dense(3,2) = 2._wp; dense(4,3) = 2._wp
            vec_y1 = matmul( dense, vec_x )
            call check(error, all(vec_y1 == [3,5,5,3]) )
            if (allocated(error)) return

            call spmv( COO , vec_x, vec_y2 )
            call check(error, all(vec_y1 == vec_y2) )
            if (allocated(error)) return

            call spmv( CSR , vec_x, vec_y3 )
            call check(error, all(vec_y1 == vec_y3) )
            if (allocated(error)) return

            call spmv( ELL , vec_x, vec_y4 )
            call check(error, all(vec_y1 == vec_y4) )
            if (allocated(error)) return
        end block
        block
            integer, parameter :: wp = dp
            type(COO_dp_type) :: COO
            type(CSR_dp_type) :: CSR
            type(ELL_dp_type) :: ELL
            real(dp), allocatable :: dense(:,:)
            real(dp), allocatable :: vec_x(:)
            real(dp), allocatable :: vec_y1(:), vec_y2(:), vec_y3(:), vec_y4(:)

            allocate( vec_x(4)  , source = 1._wp )
            allocate( vec_y1(4) , source = 0._wp )
            allocate( vec_y2(4) , source = 0._wp )
            allocate( vec_y3(4) , source = 0._wp )
            allocate( vec_y4(4) , source = 0._wp )

            allocate( dense(4,4) , source = &
                    reshape(real([1,0,0,0, &
                                  2,1,0,0, &
                                  0,2,1,0,&
                                  0,0,2,1],kind=wp),[4,4]) )

            call dense2coo( dense , COO )
            COO%storage = sparse_upper
            call coo2csr(COO, CSR)
            call csr2ell(CSR, ELL)

            dense(2,1) = 2._wp; dense(3,2) = 2._wp; dense(4,3) = 2._wp
            vec_y1 = matmul( dense, vec_x )
            call check(error, all(vec_y1 == [3,5,5,3]) )
            if (allocated(error)) return

            call spmv( COO , vec_x, vec_y2 )
            call check(error, all(vec_y1 == vec_y2) )
            if (allocated(error)) return

            call spmv( CSR , vec_x, vec_y3 )
            call check(error, all(vec_y1 == vec_y3) )
            if (allocated(error)) return

            call spmv( ELL , vec_x, vec_y4 )
            call check(error, all(vec_y1 == vec_y4) )
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
