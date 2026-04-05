module test_loadtxt
    use stdlib_kinds, only: int32, sp, dp
    use stdlib_io, only: loadtxt, savetxt
    use testdrive, only: new_unittest, unittest_type, error_type, check
    implicit none

    private
    public :: collect_loadtxt
contains

    !> Collect all exported unit tests
    subroutine collect_loadtxt(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
                    new_unittest("loadtxt_int32", test_loadtxt_int32), &
                    new_unittest("loadtxt_sp", test_loadtxt_sp), &
                    new_unittest("loadtxt_sp_huge", test_loadtxt_sp_huge), &
                    new_unittest("loadtxt_sp_tiny", test_loadtxt_sp_tiny), &
                    new_unittest("loadtxt_dp", test_loadtxt_dp), &
                    new_unittest("loadtxt_dp_max_skip", test_loadtxt_dp_max_skip), &
                    new_unittest("loadtxt_dp_huge", test_loadtxt_dp_huge), &
                    new_unittest("loadtxt_dp_tiny", test_loadtxt_dp_tiny), &
                    new_unittest("loadtxt_complex", test_loadtxt_complex), &
                    new_unittest("loadtxt_dp_comm_skip_max", test_loadtxt_dp_comm_skip_max) &
                    ]

    end subroutine collect_loadtxt

    subroutine test_loadtxt_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: input(:, :), expected(:, :)
        real(sp), allocatable :: harvest(:, :)
        integer :: n
        allocate (harvest(10, 10))
        allocate (input(10, 10))
        allocate (expected(10, 10))
        do n = 1, 10
            call random_number(harvest)
            input = int(harvest * 100)
            call savetxt('test_int32.txt', input)
            call loadtxt('test_int32.txt', expected)
            call check(error, all(input == expected), 'Default list directed read failed')
            if (allocated(error)) return
            !
            call savetxt('test_int32.txt', input, delimiter=',')
            call loadtxt('test_int32.txt', expected, delimiter=',')
            call check(error, all(input == expected), 'User specified delimiter `,` read failed')
            if (allocated(error)) return
            !
            call savetxt('test_int32.txt', input, delimiter='-')
            call loadtxt('test_int32.txt', expected, delimiter='-')
            call check(error, all(input == expected), 'User specified delimiter `-` read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_int32

    subroutine test_loadtxt_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp), allocatable :: input(:, :), expected(:, :)
        integer :: n

        allocate (input(10, 10))
        allocate (expected(10, 10))
        do n = 1, 10
            call random_number(input)
            input = input - 0.5
            call savetxt('test_sp.txt', input)
            call loadtxt('test_sp.txt', expected)
            call check(error, all(input == expected), 'Default format read failed')
            if (allocated(error)) return
            !
            call savetxt('test_sp.txt', input, delimiter=',')
            call loadtxt('test_sp.txt', expected, delimiter=',')
            call check(error, all(input == expected), 'User specified delimiter `,` read failed')
            if (allocated(error)) return
            !
            call savetxt('test_sp.txt', input, delimiter=';')
            call loadtxt('test_sp.txt', expected, delimiter=';')
            call check(error, all(input == expected), 'User specified delimiter `;` read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_sp

    subroutine test_loadtxt_sp_huge(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp), allocatable :: input(:, :), expected(:, :)
        integer :: n

        allocate (input(10, 10))
        allocate (expected(10, 10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * huge(input)
            call savetxt('test_sp_huge.txt', input)
            call loadtxt('test_sp_huge.txt', expected)
            call check(error, all(input == expected), 'Default format read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_sp_huge

    subroutine test_loadtxt_sp_tiny(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp), allocatable :: input(:, :), expected(:, :)
        integer :: n

        allocate (input(10, 10))
        allocate (expected(10, 10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * tiny(input)
            call savetxt('test_sp_tiny.txt', input)
            call loadtxt('test_sp_tiny.txt', expected)
            call check(error, all(input == expected), 'Default format read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_sp_tiny

    subroutine test_loadtxt_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:, :), expected(:, :)
        integer :: n
        real(dp) :: eps
        allocate (input(10, 10))
        allocate (expected(10, 10))
        eps = epsilon(1._dp)
        do n = 1, 10
            call random_number(input)
            input = input - 0.5
            call savetxt('test_dp.txt', input)
            call loadtxt('test_dp.txt', expected)
            call check(error, all(abs(input - expected) < eps), 'Default format read failed')
            if (allocated(error)) return
            call savetxt('test_dp.txt', input, delimiter=',')
            call loadtxt('test_dp.txt', expected, delimiter=',')
            call check(error, all(abs(input - expected) < eps), 'User specified delimiter read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_dp

    subroutine test_loadtxt_dp_comm_skip_max(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: expected(:, :)
        integer :: unit
        real(dp) :: eps
        character, parameter :: nl = new_line('a')
        character(len=*), parameter :: s = "# x(arb. units)     double (a.u.)           x²/5"//nl//&
        &"#"//nl//&
        &"# Multiline header"//nl//&
        &"5.0000000000000000E-001    1.0000000000000000E+000  5.0000000000000003E-002"//nl//&
        &nl//&
        &"1.5555555555555556E+000  3.1111111111111112E+000  4.8395061728395061E-001"//nl//&
        &"  2.6111111111111112E+000  5.2222222222222223E+000  1.3635802469135803E+000 # leading spaces"//nl//&
        &"3.6666666666666670E+000 7.3333333333333339E+000  2.6888888888888891E+000 "//nl//&
        &"4.7222222222222223E+000  9.4444444444444446E+000  4.4598765432098766E+000"//nl//&
        &"# footer"
        character(len=*), parameter :: fname = 'test_dp_comm_skip_max.dat'
        real(dp) :: input(5, 3)
        input = reshape([5.0000000000000000e-1_dp, 1.0000000000000000e+0_dp, 5.0000000000000000e-2_dp, &
      & 1.5555555555555556e+0_dp, 3.1111111111111112e+0_dp, 4.8395061728395061e-1_dp,&
      & 2.6111111111111112e+0_dp, 5.2222222222222223e+0_dp, 1.3635802469135803e+0_dp,&
      & 3.6666666666666670e+0_dp, 7.3333333333333339e+0_dp, 2.6888888888888891e+0_dp,&
      & 4.7222222222222223e+0_dp, 9.4444444444444446e+0_dp, 4.4598765432098766e+0_dp], &
      & shape(input), order=[2, 1])

        allocate (expected(10, 10))
        eps = epsilon(1._dp)
        open (newunit=unit, file=fname)
        write (unit, '(a)') s
        close (unit)

        ! Test default values
        call loadtxt(fname, expected) ! Read all data
        call check(error, all(abs(input - expected) < eps), 'Default read failed')
        if (allocated(error)) return
        ! Test skiplines option
        call loadtxt(fname, expected, skiplines=4) ! Skip comment and first line
        call check(error, all(abs(input(2:, :) - expected) < eps), 'skiplines read failed')
        if (allocated(error)) return
        ! Test max_rows option
        call loadtxt(fname, expected, max_rows=4) ! Skip comment and first line
        call check(error, all(abs(input(:4, :) - expected) < eps), 'max_rows read failed')
        if (allocated(error)) return
        ! Test usecols option
        call loadtxt(fname, expected, usecols=[3, 1, 1, 2]) ! Skip comment and first line
        call check(error, all(abs(input(:, 3) - expected(:, 1)) < eps) &
                   .or. all(abs(input(:, 1) - expected(:, 2)) < eps) &
                   .or. all(abs(input(:, 1) - expected(:, 3)) < eps) &
                   .or. all(abs(input(:, 2) - expected(:, 4)) < eps), 'usecols read failed')
        if (allocated(error)) return

    end subroutine test_loadtxt_dp_comm_skip_max

    subroutine test_loadtxt_dp_max_skip(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:, :), expected(:, :)
        integer :: n, m
        real(dp) :: eps
        eps = epsilon(1._dp)
        allocate (input(10, 10))

        do m = 0, 5
            do n = 1, 11
                call random_number(input)
                input = input - 0.5
                call savetxt('test_dp_max_skip.txt', input)
                call loadtxt('test_dp_max_skip.txt', expected, skiplines=m, max_rows=n)
                call check(error, all(abs(input(m + 1:min(n + m, 10), :) - expected) < eps),&
                    &'max_rows and skiplines read failed')
                if (allocated(error)) return
            end do
        end do

    end subroutine test_loadtxt_dp_max_skip

    subroutine test_loadtxt_dp_huge(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:, :), expected(:, :)
        integer :: n
        integer, allocatable :: j(:)
        real(dp) :: eps
        eps = 10 * epsilon(1._dp) * huge(1._dp)

        allocate (input(10, 10))
        allocate (expected(10, 10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * huge(input)
            call savetxt('test_dp_huge.txt', input)
            call loadtxt('test_dp_huge.txt', expected)
            call check(error, all(abs(input - expected) < eps), 'Huge read failed')
            if (allocated(error)) then
                ! j = maxloc(abs(input - expected))
                ! print *, maxval(abs(input - expected)), input(j(1), j(2)), expected(j(1), j(2)), eps
                return
            end if
        end do

    end subroutine test_loadtxt_dp_huge

    subroutine test_loadtxt_dp_tiny(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:, :), expected(:, :)
        integer :: n
        real(dp) :: eps
        eps = epsilon(1._dp)

        allocate (input(10, 10))
        allocate (expected(10, 10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * tiny(input)
            call savetxt('test_dp_tiny.txt', input)
            call loadtxt('test_dp_tiny.txt', expected)
            call check(error, all(abs(input - expected) < eps), 'Default format read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_dp_tiny

    subroutine test_loadtxt_complex(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        complex(dp), allocatable :: input(:, :), expected(:, :)
        real(dp), allocatable :: re(:, :), im(:, :)
        integer :: n
        real(dp) :: eps
        eps = epsilon(1._dp)

        allocate (re(10, 10))
        allocate (im(10, 10))
        allocate (input(10, 10))
        allocate (expected(10, 10))

        do n = 1, 10
            call random_number(re)
            call random_number(im)
            input = cmplx(re, im)
            call savetxt('test_complex.txt', input)
            call loadtxt('test_complex.txt', expected)
            call check(error, all(abs(input - expected) < eps))
            if (allocated(error)) return
            !
            call savetxt('test_complex.txt', input, delimiter=',')
            call loadtxt('test_complex.txt', expected, delimiter=',')
            call check(error, all(abs(input - expected) < eps))
            if (allocated(error)) return
            !
            call savetxt('test_complex.txt', input, delimiter=';')
            call loadtxt('test_complex.txt', expected, delimiter=';')
            call check(error, all(abs(input - expected) < eps))
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_complex

end module test_loadtxt

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_loadtxt, only: collect_loadtxt
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0
    testsuites = [ &
                 new_testsuite("loadtxt", collect_loadtxt) &
                 ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
