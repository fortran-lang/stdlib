module test_specialfunctions_gamma
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
    use stdlib_error, only: state_type, STDLIB_VALUE_ERROR
    use stdlib_specialfunctions_gamma, only: gamma, log_gamma, log_factorial,  &
                                             lower_incomplete_gamma,           &
                                             upper_incomplete_gamma,           &
                                             log_lower_incomplete_gamma,       &
                                             log_upper_incomplete_gamma,       &
                                             regularized_gamma_p,              &
                                             regularized_gamma_q

    implicit none
    private

    public :: collect_specialfunctions_gamma

    real(sp), parameter :: tol_sp = sqrt(epsilon(1.0_sp))
    real(dp), parameter :: tol_dp = sqrt(epsilon(1.0_dp))

contains

    subroutine collect_specialfunctions_gamma(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [                                                          &
            new_unittest("log_factorial_iint8", test_logfact_iint8)            &

            , new_unittest("log_factorial_iint8",                    &
                           test_logfact_iint8)                       &
            , new_unittest("log_factorial_iint16",                    &
                           test_logfact_iint16)                       &
            , new_unittest("log_factorial_iint32",                    &
                           test_logfact_iint32)                       &
            , new_unittest("log_factorial_iint64",                    &
                           test_logfact_iint64)                       &

            , new_unittest("gamma_iint8",                            &
                           test_gamma_iint8)                         &
            , new_unittest("log_gamma_iint8",                        &
                           test_loggamma_iint8)                      &
            , new_unittest("gamma_iint16",                            &
                           test_gamma_iint16)                         &
            , new_unittest("log_gamma_iint16",                        &
                           test_loggamma_iint16)                      &
            , new_unittest("gamma_iint32",                            &
                           test_gamma_iint32)                         &
            , new_unittest("log_gamma_iint32",                        &
                           test_loggamma_iint32)                      &
            , new_unittest("gamma_iint64",                            &
                           test_gamma_iint64)                         &
            , new_unittest("log_gamma_iint64",                        &
                           test_loggamma_iint64)                      &
            , new_unittest("gamma_csp",                            &
                           test_gamma_csp)                         &
            , new_unittest("log_gamma_csp",                        &
                           test_loggamma_csp)                      &

            , new_unittest("lower_incomplete_gamma_iint8sp",     &
                           test_lincgamma_iint8sp)               &
            , new_unittest("log_lower_incomplete_gamma_iint8sp", &
                           test_log_lincgamma_iint8sp)           &
            , new_unittest("upper_incomplete_gamma_iint8sp",     &
                           test_uincgamma_iint8sp)               &
            , new_unittest("log_upper_incomplete_gamma_iint8sp", &
                           test_log_uincgamma_iint8sp)           &
            , new_unittest("regularized_gamma_p_iint8sp",        &
                           test_gamma_p_iint8sp)                 &
            , new_unittest("regularized_gamma_q_iint8sp",        &
                           test_gamma_q_iint8sp)                 &
            , new_unittest("lower_incomplete_gamma_iint16sp",     &
                           test_lincgamma_iint16sp)               &
            , new_unittest("log_lower_incomplete_gamma_iint16sp", &
                           test_log_lincgamma_iint16sp)           &
            , new_unittest("upper_incomplete_gamma_iint16sp",     &
                           test_uincgamma_iint16sp)               &
            , new_unittest("log_upper_incomplete_gamma_iint16sp", &
                           test_log_uincgamma_iint16sp)           &
            , new_unittest("regularized_gamma_p_iint16sp",        &
                           test_gamma_p_iint16sp)                 &
            , new_unittest("regularized_gamma_q_iint16sp",        &
                           test_gamma_q_iint16sp)                 &
            , new_unittest("lower_incomplete_gamma_iint32sp",     &
                           test_lincgamma_iint32sp)               &
            , new_unittest("log_lower_incomplete_gamma_iint32sp", &
                           test_log_lincgamma_iint32sp)           &
            , new_unittest("upper_incomplete_gamma_iint32sp",     &
                           test_uincgamma_iint32sp)               &
            , new_unittest("log_upper_incomplete_gamma_iint32sp", &
                           test_log_uincgamma_iint32sp)           &
            , new_unittest("regularized_gamma_p_iint32sp",        &
                           test_gamma_p_iint32sp)                 &
            , new_unittest("regularized_gamma_q_iint32sp",        &
                           test_gamma_q_iint32sp)                 &
            , new_unittest("lower_incomplete_gamma_iint64sp",     &
                           test_lincgamma_iint64sp)               &
            , new_unittest("log_lower_incomplete_gamma_iint64sp", &
                           test_log_lincgamma_iint64sp)           &
            , new_unittest("upper_incomplete_gamma_iint64sp",     &
                           test_uincgamma_iint64sp)               &
            , new_unittest("log_upper_incomplete_gamma_iint64sp", &
                           test_log_uincgamma_iint64sp)           &
            , new_unittest("regularized_gamma_p_iint64sp",        &
                           test_gamma_p_iint64sp)                 &
            , new_unittest("regularized_gamma_q_iint64sp",        &
                           test_gamma_q_iint64sp)                 &

            , new_unittest("lower_incomplete_gamma_rsp",           &
                           test_lincgamma_rsp)                     &
            , new_unittest("log_lower_incomplete_gamma_rsp",       &
                           test_log_lincgamma_rsp)                 &
            , new_unittest("upper_incomplete_gamma_rsp",           &
                           test_uincgamma_rsp)                     &
            , new_unittest("log_upper_incomplete_gamma_rsp",       &
                           test_log_uincgamma_rsp)                 &
            , new_unittest("regularized_gamma_p_rsp",              &
                           test_gamma_p_rsp)                       &
            , new_unittest("regularized_gamma_q_rsp",              &
                           test_gamma_q_rsp)                       &
            ]
    end subroutine collect_specialfunctions_gamma



    subroutine test_logfact_iint8(error)
        type(error_type), allocatable, intent(out) :: error        
        integer :: i
        
        integer, parameter :: xtest(*) = [0,1,2,4,5,7,12,20,100,500,7000,90000]
        real(sp), parameter :: res(*) = [0.0_sp, &
                                       0.0_sp, &
                                       0.69314718055994_sp, &
                                       3.17805383034794_sp, &
                                       4.78749174278204_sp, &
                                       8.52516136106541_sp, &
                                       1.998721449566e1_sp, &
                                       4.233561646075e1_sp, &
                                       3.637393755555e2_sp, &
                                       2.611330458460e3_sp, &
                                       5.498100377941e4_sp, &
                                       9.366874681600e5_sp]        

        integer(int8), parameter :: x(*)   = pack(xtest, xtest<huge(0_int8))
        real(sp), parameter :: ans(*) = pack(res  , xtest<huge(0_int8))
        integer, parameter :: n = size(x)

        do i = 1, n
            call check(error, log_factorial(x(i)), ans(i), "Integer kind "     &
                       //"int8 failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_logfact_iint8

    subroutine test_logfact_iint16(error)
        type(error_type), allocatable, intent(out) :: error        
        integer :: i
        
        integer, parameter :: xtest(*) = [0,1,2,4,5,7,12,20,100,500,7000,90000]
        real(sp), parameter :: res(*) = [0.0_sp, &
                                       0.0_sp, &
                                       0.69314718055994_sp, &
                                       3.17805383034794_sp, &
                                       4.78749174278204_sp, &
                                       8.52516136106541_sp, &
                                       1.998721449566e1_sp, &
                                       4.233561646075e1_sp, &
                                       3.637393755555e2_sp, &
                                       2.611330458460e3_sp, &
                                       5.498100377941e4_sp, &
                                       9.366874681600e5_sp]        

        integer(int16), parameter :: x(*)   = pack(xtest, xtest<huge(0_int16))
        real(sp), parameter :: ans(*) = pack(res  , xtest<huge(0_int16))
        integer, parameter :: n = size(x)

        do i = 1, n
            call check(error, log_factorial(x(i)), ans(i), "Integer kind "     &
                       //"int16 failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_logfact_iint16

    subroutine test_logfact_iint32(error)
        type(error_type), allocatable, intent(out) :: error        
        integer :: i
        
        integer, parameter :: xtest(*) = [0,1,2,4,5,7,12,20,100,500,7000,90000]
        real(sp), parameter :: res(*) = [0.0_sp, &
                                       0.0_sp, &
                                       0.69314718055994_sp, &
                                       3.17805383034794_sp, &
                                       4.78749174278204_sp, &
                                       8.52516136106541_sp, &
                                       1.998721449566e1_sp, &
                                       4.233561646075e1_sp, &
                                       3.637393755555e2_sp, &
                                       2.611330458460e3_sp, &
                                       5.498100377941e4_sp, &
                                       9.366874681600e5_sp]        

        integer(int32), parameter :: x(*)   = pack(xtest, xtest<huge(0_int32))
        real(sp), parameter :: ans(*) = pack(res  , xtest<huge(0_int32))
        integer, parameter :: n = size(x)

        do i = 1, n
            call check(error, log_factorial(x(i)), ans(i), "Integer kind "     &
                       //"int32 failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_logfact_iint32

    subroutine test_logfact_iint64(error)
        type(error_type), allocatable, intent(out) :: error        
        integer :: i
        
        integer, parameter :: xtest(*) = [0,1,2,4,5,7,12,20,100,500,7000,90000]
        real(sp), parameter :: res(*) = [0.0_sp, &
                                       0.0_sp, &
                                       0.69314718055994_sp, &
                                       3.17805383034794_sp, &
                                       4.78749174278204_sp, &
                                       8.52516136106541_sp, &
                                       1.998721449566e1_sp, &
                                       4.233561646075e1_sp, &
                                       3.637393755555e2_sp, &
                                       2.611330458460e3_sp, &
                                       5.498100377941e4_sp, &
                                       9.366874681600e5_sp]        

        integer(int64), parameter :: x(*)   = pack(xtest, xtest<huge(0_int64))
        real(sp), parameter :: ans(*) = pack(res  , xtest<huge(0_int64))
        integer, parameter :: n = size(x)

        do i = 1, n
            call check(error, log_factorial(x(i)), ans(i), "Integer kind "     &
                       //"int64 failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_logfact_iint64





    subroutine test_gamma_iint8(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 5
        integer :: i
        type(state_type) :: err


        integer(int8), parameter :: x(n) = [1_int8, 2_int8, 4_int8, 5_int8, 6_int8]
        integer(int8), parameter :: ans(n) = [1_int8, 1_int8, 6_int8, 24_int8, 120_int8]




        do i = 1, n

            call check(error, gamma(x(i)), ans(i), "Integer kind int8 failed")

        end do

        
    end subroutine test_gamma_iint8



    subroutine test_loggamma_iint8(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        type(state_type) :: err
        
        
        integer, parameter :: xtest(*) = [1,2,10,47,111,541,2021,42031]
        real(dp), parameter :: res(*) = [0.0_dp, &
                                       0.0_dp, &
                                       1.28018274e1_dp, &
                                       1.32952575e2_dp, &
                                       4.10322777e2_dp, &
                                       2.86151221e3_dp, &
                                       1.33586470e4_dp, &
                                       4.05433461e5_dp]
                                       
        integer,  parameter :: x(*) = pack(xtest,xtest<huge(0_int8))
        real(dp), parameter :: ans(*) = pack(res,xtest<huge(0_int8))
        integer,  parameter :: n = size(x)

        do i = 1, n
            print *, 'log ',log_gamma(x(i)),' ans=',ans(i),' tol=',tol_dp
            call check(error, log_gamma(x(i)), ans(i), "Integer kind int8 "  &
              //"failed", thr = tol_dp, rel = .true.)

        end do

    end subroutine test_loggamma_iint8


    subroutine test_gamma_iint16(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 5
        integer :: i
        type(state_type) :: err


        integer(int16), parameter :: x(n) = [1_int16, 2_int16, 4_int16, 5_int16, 8_int16]
        integer(int16), parameter :: ans(n) = [1_int16, 1_int16, 6_int16, 24_int16, 5040_int16]




        do i = 1, n

            call check(error, gamma(x(i)), ans(i), "Integer kind int16 failed")

        end do

        
    end subroutine test_gamma_iint16



    subroutine test_loggamma_iint16(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        type(state_type) :: err
        
        
        integer, parameter :: xtest(*) = [1,2,10,47,111,541,2021,42031]
        real(dp), parameter :: res(*) = [0.0_dp, &
                                       0.0_dp, &
                                       1.28018274e1_dp, &
                                       1.32952575e2_dp, &
                                       4.10322777e2_dp, &
                                       2.86151221e3_dp, &
                                       1.33586470e4_dp, &
                                       4.05433461e5_dp]
                                       
        integer,  parameter :: x(*) = pack(xtest,xtest<huge(0_int16))
        real(dp), parameter :: ans(*) = pack(res,xtest<huge(0_int16))
        integer,  parameter :: n = size(x)

        do i = 1, n
            print *, 'log ',log_gamma(x(i)),' ans=',ans(i),' tol=',tol_dp
            call check(error, log_gamma(x(i)), ans(i), "Integer kind int16 "  &
              //"failed", thr = tol_dp, rel = .true.)

        end do

    end subroutine test_loggamma_iint16


    subroutine test_gamma_iint32(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 5
        integer :: i
        type(state_type) :: err


        integer(int32), parameter :: x(n) = [1_int32, 2_int32, 4_int32, 8_int32, 13_int32]
        integer(int32), parameter :: ans(n) = [1_int32, 1_int32, 6_int32, 5040_int32, &
                                      479001600_int32]




        do i = 1, n

            call check(error, gamma(x(i)), ans(i), "Integer kind int32 failed")

        end do

        
    end subroutine test_gamma_iint32



    subroutine test_loggamma_iint32(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        type(state_type) :: err
        
        
        integer, parameter :: xtest(*) = [1,2,10,47,111,541,2021,42031]
        real(dp), parameter :: res(*) = [0.0_dp, &
                                       0.0_dp, &
                                       1.28018274e1_dp, &
                                       1.32952575e2_dp, &
                                       4.10322777e2_dp, &
                                       2.86151221e3_dp, &
                                       1.33586470e4_dp, &
                                       4.05433461e5_dp]
                                       
        integer,  parameter :: x(*) = pack(xtest,xtest<huge(0_int32))
        real(dp), parameter :: ans(*) = pack(res,xtest<huge(0_int32))
        integer,  parameter :: n = size(x)

        do i = 1, n
            print *, 'log ',log_gamma(x(i)),' ans=',ans(i),' tol=',tol_dp
            call check(error, log_gamma(x(i)), ans(i), "Integer kind int32 "  &
              //"failed", thr = tol_dp, rel = .true.)

        end do

    end subroutine test_loggamma_iint32


    subroutine test_gamma_iint64(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 5
        integer :: i
        type(state_type) :: err


        integer(int64), parameter :: x(n) = [1_int64, 2_int64, 4_int64, 13_int64, 21_int64]
        integer(int64), parameter :: ans(n) = [1_int64, 1_int64, 6_int64, 479001600_int64, &
                                      2432902008176640000_int64]



        do i = 1, n

            call check(error, gamma(x(i)), ans(i), "Integer kind int64 failed")

        end do

        
    end subroutine test_gamma_iint64



    subroutine test_loggamma_iint64(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        type(state_type) :: err
        
        
        integer, parameter :: xtest(*) = [1,2,10,47,111,541,2021,42031]
        real(dp), parameter :: res(*) = [0.0_dp, &
                                       0.0_dp, &
                                       1.28018274e1_dp, &
                                       1.32952575e2_dp, &
                                       4.10322777e2_dp, &
                                       2.86151221e3_dp, &
                                       1.33586470e4_dp, &
                                       4.05433461e5_dp]
                                       
        integer,  parameter :: x(*) = pack(xtest,xtest<huge(0_int64))
        real(dp), parameter :: ans(*) = pack(res,xtest<huge(0_int64))
        integer,  parameter :: n = size(x)

        do i = 1, n
            print *, 'log ',log_gamma(x(i)),' ans=',ans(i),' tol=',tol_dp
            call check(error, log_gamma(x(i)), ans(i), "Integer kind int64 "  &
              //"failed", thr = tol_dp, rel = .true.)

        end do

    end subroutine test_loggamma_iint64


    subroutine test_gamma_csp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 5
        integer :: i
        type(state_type) :: err


        complex(sp), parameter :: x(n) = [(0.25_sp, 0.25_sp),               &
                                     (0.5_sp, -0.5_sp),                &
                                     (1.0_sp, 1.0_sp),                 &
                                     (-1.254e1_sp, -9.87_sp),          &
                                     (0.0_sp, 1.0_sp)                  &
                                    ]

        complex(sp), parameter :: ans(n) =                                          &
                     [(1.6511332803889208_sp, -1.8378758749947890_sp), &
                      (0.81816399954174739_sp, 0.76331382871398262_sp),&
                     (0.49801566811835604_sp, -0.15494982830181069_sp),&
             (-2.18767396709283064e-21_sp, 2.77577940846953455e-21_sp),&
                    (-0.15494982830181067_sp, -0.49801566811835607_sp) &
             ]



        do i = 1, n
            
            err = state_type(STDLIB_VALUE_ERROR,'Complex sp failed: x=',x(i), &
                             ' gamma=',gamma(x(i)), &
                             'expected=',ans(i), &
                             ' tol=',tol_sp)

            call check(error, gamma(x(i)), ans(i), err%print(),&
                       thr = tol_sp, rel = .true.)

        end do
        
    end subroutine test_gamma_csp



    subroutine test_loggamma_csp(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i
        type(state_type) :: err
        

        complex(sp), parameter :: x(*) = [(0.25_sp, 0.25_sp),               &
                                     (0.5_sp, -0.5_sp),                &
                                     (1.0_sp, 1.0_sp),                 &
                                     (-1.254e1_sp, -9.87_sp)]

        complex(sp), parameter :: ans(*) =                                          &
                    [(0.90447450949333889_sp, -0.83887024394321282_sp),&
                     (0.11238724280962311_sp, 0.75072920212205074_sp), &
                    (-0.65092319930185634_sp, -0.30164032046753320_sp),&
                   (-4.7091788015763380e1_sp, 1.4804627819235690e1_sp)]
        
        integer, parameter :: n = size(x)

        do i = 1, n

            err = state_type(STDLIB_VALUE_ERROR,'Complex sp failed: x=',x(i), &
                             ' log(gamma)=',log_gamma(x(i)), &
                             'expected=',ans(i), &
                             ' tol=',tol_sp)

            call check(error, log_gamma(x(i)), ans(i), err%print(),&
                       thr = tol_sp, rel = .true.)

        end do

    end subroutine test_loggamma_csp






    subroutine test_lincgamma_iint8sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int8), parameter :: p(n) = [1_int8, 2_int8, 3_int8, 2_int8]
        real(sp), parameter :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.3934693402873667_sp,              &
                                       0.86411177459956675_sp,             &
                                       -2.5210237047438023e3_sp,           &
                                       1.9823919215326045e5_sp]

        do i = 1, n

            call check(error, lower_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Lower incomplete gamma function with p(kind=int8) and "       &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do

    end subroutine test_lincgamma_iint8sp



    subroutine test_log_lincgamma_iint8sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int8) :: p(n) = [1_int8, 2_int8, 3_int8, 2_int8]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.93275212956718857_sp,            &
                                       -0.14605314979599791_sp,            &
                                        7.8324203300567640_sp,             &
                                        1.2197229621760137e1_sp]

        do i = 1, n

            call check(error, log_lower_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of lower incomplete gamma function with "             &
              //"p(kind=int8) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_log_lincgamma_iint8sp



    subroutine test_uincgamma_iint8sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int8) :: p(n) = [1_int8, 2_int8, 3_int8, 2_int8]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.13588822540043325_sp,             &
                                       2.5230237047438022E3_sp,            &
                                       -1.9823819215326045e5_sp]

        do i = 1, n

            call check(error, upper_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int8) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_uincgamma_iint8sp



    subroutine test_log_uincgamma_iint8sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int8) :: p(n) = [1_int8, 2_int8, 3_int8, 2_int8]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.5_sp, -1.9959226032237259_sp,&
                                       7.8332133440562161_sp,              &
                                       1.2197224577336219e1_sp]

        do i = 1, n

            call check(error, log_upper_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int8) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do
    end subroutine test_log_uincgamma_iint8sp




    subroutine test_gamma_p_iint8sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int8) :: p(n) = [1_int8, 1_int8, 3_int8, 3_int8]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.39346934028736658_sp,             &
                                       0.77686983985157017_sp,             &
                                       1.4387677966970687e-2_sp,           &
                                       0.67915280113786593_sp]

        do i = 1, n

            call check(error, regularized_gamma_p(p(i), x(i)), ans(i),         &
              "Regularized gamma P function with p(kind=int8) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_p_iint8sp



    subroutine test_gamma_q_iint8sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int8) :: p(n) = [1_int8, 1_int8, 3_int8, 3_int8]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.22313016014842983_sp,             &
                                       0.98561232203302931_sp,             &
                                       0.32084719886213407_sp]

        do i = 1, n

            call check(error, regularized_gamma_q(p(i), x(i)), ans(i),         &
              "Regularized gamma Q function with p(kind=int8) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_q_iint8sp


    subroutine test_lincgamma_iint16sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int16), parameter :: p(n) = [1_int16, 2_int16, 3_int16, 2_int16]
        real(sp), parameter :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.3934693402873667_sp,              &
                                       0.86411177459956675_sp,             &
                                       -2.5210237047438023e3_sp,           &
                                       1.9823919215326045e5_sp]

        do i = 1, n

            call check(error, lower_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Lower incomplete gamma function with p(kind=int16) and "       &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do

    end subroutine test_lincgamma_iint16sp



    subroutine test_log_lincgamma_iint16sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int16) :: p(n) = [1_int16, 2_int16, 3_int16, 2_int16]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.93275212956718857_sp,            &
                                       -0.14605314979599791_sp,            &
                                        7.8324203300567640_sp,             &
                                        1.2197229621760137e1_sp]

        do i = 1, n

            call check(error, log_lower_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of lower incomplete gamma function with "             &
              //"p(kind=int16) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_log_lincgamma_iint16sp



    subroutine test_uincgamma_iint16sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int16) :: p(n) = [1_int16, 2_int16, 3_int16, 2_int16]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.13588822540043325_sp,             &
                                       2.5230237047438022E3_sp,            &
                                       -1.9823819215326045e5_sp]

        do i = 1, n

            call check(error, upper_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int16) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_uincgamma_iint16sp



    subroutine test_log_uincgamma_iint16sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int16) :: p(n) = [1_int16, 2_int16, 3_int16, 2_int16]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.5_sp, -1.9959226032237259_sp,&
                                       7.8332133440562161_sp,              &
                                       1.2197224577336219e1_sp]

        do i = 1, n

            call check(error, log_upper_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int16) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do
    end subroutine test_log_uincgamma_iint16sp




    subroutine test_gamma_p_iint16sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int16) :: p(n) = [1_int16, 1_int16, 3_int16, 3_int16]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.39346934028736658_sp,             &
                                       0.77686983985157017_sp,             &
                                       1.4387677966970687e-2_sp,           &
                                       0.67915280113786593_sp]

        do i = 1, n

            call check(error, regularized_gamma_p(p(i), x(i)), ans(i),         &
              "Regularized gamma P function with p(kind=int16) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_p_iint16sp



    subroutine test_gamma_q_iint16sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int16) :: p(n) = [1_int16, 1_int16, 3_int16, 3_int16]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.22313016014842983_sp,             &
                                       0.98561232203302931_sp,             &
                                       0.32084719886213407_sp]

        do i = 1, n

            call check(error, regularized_gamma_q(p(i), x(i)), ans(i),         &
              "Regularized gamma Q function with p(kind=int16) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_q_iint16sp


    subroutine test_lincgamma_iint32sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int32), parameter :: p(n) = [1_int32, 2_int32, 3_int32, 2_int32]
        real(sp), parameter :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.3934693402873667_sp,              &
                                       0.86411177459956675_sp,             &
                                       -2.5210237047438023e3_sp,           &
                                       1.9823919215326045e5_sp]

        do i = 1, n

            call check(error, lower_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Lower incomplete gamma function with p(kind=int32) and "       &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do

    end subroutine test_lincgamma_iint32sp



    subroutine test_log_lincgamma_iint32sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int32) :: p(n) = [1_int32, 2_int32, 3_int32, 2_int32]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.93275212956718857_sp,            &
                                       -0.14605314979599791_sp,            &
                                        7.8324203300567640_sp,             &
                                        1.2197229621760137e1_sp]

        do i = 1, n

            call check(error, log_lower_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of lower incomplete gamma function with "             &
              //"p(kind=int32) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_log_lincgamma_iint32sp



    subroutine test_uincgamma_iint32sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int32) :: p(n) = [1_int32, 2_int32, 3_int32, 2_int32]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.13588822540043325_sp,             &
                                       2.5230237047438022E3_sp,            &
                                       -1.9823819215326045e5_sp]

        do i = 1, n

            call check(error, upper_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int32) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_uincgamma_iint32sp



    subroutine test_log_uincgamma_iint32sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int32) :: p(n) = [1_int32, 2_int32, 3_int32, 2_int32]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.5_sp, -1.9959226032237259_sp,&
                                       7.8332133440562161_sp,              &
                                       1.2197224577336219e1_sp]

        do i = 1, n

            call check(error, log_upper_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int32) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do
    end subroutine test_log_uincgamma_iint32sp




    subroutine test_gamma_p_iint32sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int32) :: p(n) = [1_int32, 1_int32, 3_int32, 3_int32]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.39346934028736658_sp,             &
                                       0.77686983985157017_sp,             &
                                       1.4387677966970687e-2_sp,           &
                                       0.67915280113786593_sp]

        do i = 1, n

            call check(error, regularized_gamma_p(p(i), x(i)), ans(i),         &
              "Regularized gamma P function with p(kind=int32) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_p_iint32sp



    subroutine test_gamma_q_iint32sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int32) :: p(n) = [1_int32, 1_int32, 3_int32, 3_int32]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.22313016014842983_sp,             &
                                       0.98561232203302931_sp,             &
                                       0.32084719886213407_sp]

        do i = 1, n

            call check(error, regularized_gamma_q(p(i), x(i)), ans(i),         &
              "Regularized gamma Q function with p(kind=int32) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_q_iint32sp


    subroutine test_lincgamma_iint64sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int64), parameter :: p(n) = [1_int64, 2_int64, 3_int64, 2_int64]
        real(sp), parameter :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.3934693402873667_sp,              &
                                       0.86411177459956675_sp,             &
                                       -2.5210237047438023e3_sp,           &
                                       1.9823919215326045e5_sp]

        do i = 1, n

            call check(error, lower_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Lower incomplete gamma function with p(kind=int64) and "       &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do

    end subroutine test_lincgamma_iint64sp



    subroutine test_log_lincgamma_iint64sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int64) :: p(n) = [1_int64, 2_int64, 3_int64, 2_int64]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.93275212956718857_sp,            &
                                       -0.14605314979599791_sp,            &
                                        7.8324203300567640_sp,             &
                                        1.2197229621760137e1_sp]

        do i = 1, n

            call check(error, log_lower_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of lower incomplete gamma function with "             &
              //"p(kind=int64) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_log_lincgamma_iint64sp



    subroutine test_uincgamma_iint64sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int64) :: p(n) = [1_int64, 2_int64, 3_int64, 2_int64]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.13588822540043325_sp,             &
                                       2.5230237047438022E3_sp,            &
                                       -1.9823819215326045e5_sp]

        do i = 1, n

            call check(error, upper_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int64) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_uincgamma_iint64sp



    subroutine test_log_uincgamma_iint64sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int64) :: p(n) = [1_int64, 2_int64, 3_int64, 2_int64]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, -5.0_sp, -10.0_sp]

        real(sp), parameter :: ans(n) = [-0.5_sp, -1.9959226032237259_sp,&
                                       7.8332133440562161_sp,              &
                                       1.2197224577336219e1_sp]

        do i = 1, n

            call check(error, log_upper_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=int64) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do
    end subroutine test_log_uincgamma_iint64sp




    subroutine test_gamma_p_iint64sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int64) :: p(n) = [1_int64, 1_int64, 3_int64, 3_int64]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.39346934028736658_sp,             &
                                       0.77686983985157017_sp,             &
                                       1.4387677966970687e-2_sp,           &
                                       0.67915280113786593_sp]

        do i = 1, n

            call check(error, regularized_gamma_p(p(i), x(i)), ans(i),         &
              "Regularized gamma P function with p(kind=int64) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_p_iint64sp



    subroutine test_gamma_q_iint64sp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        integer(int64) :: p(n) = [1_int64, 1_int64, 3_int64, 3_int64]
        real(sp) :: x(n) = [0.5_sp, 1.5_sp, 0.5_sp, 3.5_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.22313016014842983_sp,             &
                                       0.98561232203302931_sp,             &
                                       0.32084719886213407_sp]

        do i = 1, n

            call check(error, regularized_gamma_q(p(i), x(i)), ans(i),         &
              "Regularized gamma Q function with p(kind=int64) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_q_iint64sp





    subroutine test_lincgamma_rsp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        real(sp) :: p(n) = [1.0_sp, 2.0_sp, 3.1_sp, 6.5_sp]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, 5.0_sp, 3.2_sp]

        real(sp), parameter :: ans(n) = [0.3934693402873667_sp,              &
                                       0.86411177459956675_sp,             &
                                       1.8980559470963281_sp,              &
                                       2.0043549563092636e1_sp]

        do i = 1, n

            call check(error, lower_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Lower incomplete gamma function with p(kind=sp) and "       &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do

    end subroutine test_lincgamma_rsp



    subroutine test_log_lincgamma_rsp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        real(sp) :: p(n) = [1.0_sp, 2.0_sp, 3.1_sp, 6.5_sp]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, 5.0_sp, 3.2_sp]

        real(sp), parameter :: ans(n) = [-0.93275212956718857_sp,            &
                                       -0.14605314979599791_sp,            &
                                        0.64083017662175706_sp,            &
                                        2.9979073844388951_sp]

        do i = 1, n

            call check(error, log_lower_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of lower incomplete gamma function with "             &
              //"p(kind=sp) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_log_lincgamma_rsp



    subroutine test_uincgamma_rsp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        real(sp) :: p(n) = [1.0_sp, 2.0_sp, 3.1_sp, 6.5_sp]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, 5.0_sp, 3.2_sp]

        real(sp), parameter :: ans(n) = [0.60653065971263342_sp,             &
                                       0.13588822540043325_sp,             &
                                       0.29956433129614910_sp,             &
                                       2.6784172825195172e2_sp]

        do i = 1, n

            call check(error, upper_incomplete_gamma(p(i), x(i)), ans(i),      &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=sp) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do

    end subroutine test_uincgamma_rsp



    subroutine test_log_uincgamma_rsp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        real(sp) :: p(n) = [1.0_sp, 2.0_sp, 3.1_sp, 6.5_sp]
        real(sp) :: x(n) = [0.5_sp, 3.5_sp, 5.0_sp, 3.2_sp]

        real(sp), parameter :: ans(n) = [-0.5_sp, -1.9959226032237259_sp,&
                                       -1.2054260888453405_sp,             &
                                       5.5903962398338761_sp]

        do i = 1, n

            call check(error, log_upper_incomplete_gamma(p(i), x(i)), ans(i),  &
              "Logarithm of upper incomplete gamma function with "             &
              //"p(kind=sp) and x(kind=sp) failed", thr = tol_sp,  &
              rel = .true.)

        end do
    end subroutine test_log_uincgamma_rsp



    subroutine test_gamma_p_rsp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        real(sp) :: p(n) = [1.3_sp, 1.3_sp, 3.7_sp, 3.7_sp]
        real(sp) :: x(n) = [0.5_sp, 2.1_sp, 2.6_sp, 5.1_sp]

        real(sp), parameter :: ans(n) = [0.26487356764588505_sp,             &
                                       0.81011791338807457_sp,             &
                                       0.32198359288949589_sp,             &
                                       0.79435732817518852_sp]

        do i = 1, n

            call check(error, regularized_gamma_p(p(i), x(i)), ans(i),         &
              "Regularized gamma P function with p(kind=sp) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_p_rsp



    subroutine test_gamma_q_rsp(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 4
        integer :: i
        real(sp) :: p(n) = [1.3_sp, 1.3_sp, 3.7_sp, 3.7_sp]
        real(sp) :: x(n) = [0.5_sp, 2.1_sp, 2.6_sp, 5.1_sp]

        real(sp), parameter :: ans(n) = [0.73512643235411495_sp,             &
                                       0.18988208661192543_sp,             &
                                       0.67801640711050411_sp,             &
                                       0.20564267182481148_sp]

        do i = 1, n

            call check(error, regularized_gamma_q(p(i), x(i)), ans(i),         &
              "Regularized gamma Q function with p(kind=sp) and "          &
              //"x(kind=sp) failed", thr = tol_sp, rel = .true.)

        end do
    end subroutine test_gamma_q_rsp

end module test_specialfunctions_gamma



program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_specialfunctions_gamma, only : collect_specialfunctions_gamma
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [new_testsuite("Gamma special function",                      &
                 collect_specialfunctions_gamma)]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
