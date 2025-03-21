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
            new_unittest("loadtxt_complex", test_loadtxt_complex) &
        ]

    end subroutine collect_loadtxt


    subroutine test_loadtxt_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: input(:,:), expected(:,:)
        real(sp), allocatable :: harvest(:,:)
        integer :: n
        allocate(harvest(10,10))
        allocate(input(10,10))
        allocate(expected(10,10))
        do n = 1, 10
            call random_number(harvest)
            input = int(harvest * 100)
            call savetxt('test_int32.txt', input)
            call loadtxt('test_int32.txt', expected)
            call check(error, all(input == expected),'Default list directed read failed')
            if (allocated(error)) return
            call loadtxt('test_int32.txt', expected, fmt='*')
            call check(error, all(input == expected),'User specified list directed read faile')
            if (allocated(error)) return
            call savetxt('test_int32.txt', input, delimiter=',')
            call loadtxt('test_int32.txt', expected, delimiter=',')
            call check(error, all(input == expected),'User specified delimiter `,` read failed')
            if (allocated(error)) return
            call savetxt('test_int32.txt', input, delimiter='-')
            call loadtxt('test_int32.txt', expected, delimiter='-')
            call check(error, all(input == expected),'User specified delimiter `-` read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_int32


    subroutine test_loadtxt_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp), allocatable :: input(:,:), expected(:,:)
        character(len=*), parameter :: FMT_REAL_SP = '(es15.8e2)'
        integer :: n

        allocate(input(10,10))
        allocate(expected(10,10))
        do n = 1, 10
            call random_number(input)
            input = input - 0.5
            call savetxt('test_sp.txt', input)
            call loadtxt('test_sp.txt', expected)
            call check(error, all(input == expected),'Default format read failed')
            if (allocated(error)) return
            call loadtxt('test_sp.txt', expected, fmt='*')
            call check(error, all(input == expected),'List directed read failed')
            if (allocated(error)) return
            call loadtxt('test_sp.txt', expected, fmt="(*"//FMT_REAL_sp(1:len(FMT_REAL_sp)-1)//",1x))")
            call check(error, all(input == expected),'User specified format failed')
            if (allocated(error)) return
            call savetxt('test_sp.txt', input, delimiter=',')
            call loadtxt('test_sp.txt', expected, delimiter=',')
            call check(error, all(input == expected),'User specified delimiter `,` read failed')
            if (allocated(error)) return
            call savetxt('test_sp.txt', input, delimiter=';')
            call loadtxt('test_sp.txt', expected, delimiter=';')
            call check(error, all(input == expected),'User specified delimiter `;` read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_sp


    subroutine test_loadtxt_sp_huge(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp), allocatable :: input(:,:), expected(:,:)
        integer :: n
        character(len=*), parameter :: FMT_REAL_SP = '(es15.8e2)'
        
        allocate(input(10,10))
        allocate(expected(10,10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * huge(input)
            call savetxt('test_sp_huge.txt', input)
            call loadtxt('test_sp_huge.txt', expected)
            call check(error, all(input == expected),'Default format read failed')
            if (allocated(error)) return
            call loadtxt('test_sp_huge.txt', expected, fmt='*')
            call check(error, all(input == expected),'List directed read failed')
            if (allocated(error)) return
            call loadtxt('test_sp_huge.txt', expected, fmt="(*"//FMT_REAL_sp(1:len(FMT_REAL_sp)-1)//",1x))")
            call check(error, all(input == expected),'User specified format failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_sp_huge


    subroutine test_loadtxt_sp_tiny(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp), allocatable :: input(:,:), expected(:,:)
        integer :: n
        character(len=*), parameter :: FMT_REAL_SP = '(es15.8e2)'

        allocate(input(10,10))
        allocate(expected(10,10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * tiny(input)
            call savetxt('test_sp_tiny.txt', input)
            call loadtxt('test_sp_tiny.txt', expected)
            call check(error, all(input == expected),'Default format read failed')
            if (allocated(error)) return
            call loadtxt('test_sp_tiny.txt', expected, fmt='*')
            call check(error, all(input == expected),'List directed read failed')
            if (allocated(error)) return
            call loadtxt('test_sp_tiny.txt', expected, fmt="(*"//FMT_REAL_sp(1:len(FMT_REAL_sp)-1)//",1x))")
            call check(error, all(input == expected),'User specified format failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_sp_tiny


    subroutine test_loadtxt_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:,:), expected(:,:)
        integer :: n
        character(len=*), parameter :: FMT_REAL_DP = '(es24.16e3)'

        allocate(input(10,10))
        allocate(expected(10,10))

        do n = 1, 10
            call random_number(input)
            input = input - 0.5
            call savetxt('test_dp.txt', input)
            call loadtxt('test_dp.txt', expected)
            call check(error, all(input == expected),'Default format read failed')
            if (allocated(error)) return
            call loadtxt('test_dp.txt', expected, fmt='*')
            call check(error, all(input == expected),'List directed read failed')
            if (allocated(error)) return
            call loadtxt('test_dp.txt', expected, fmt="(*"//FMT_REAL_dp(1:len(FMT_REAL_dp)-1)//",1x))")
            call check(error, all(input == expected),'User specified format failed')
            if (allocated(error)) return
            call savetxt('test_dp.txt', input, delimiter=',')
            call loadtxt('test_dp.txt', expected, delimiter=',')
            call check(error, all(input == expected),'User specified delimiter read failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_dp


    subroutine test_loadtxt_dp_max_skip(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:,:), expected(:,:)
        integer :: n, m
        character(len=*), parameter :: FMT_REAL_DP = '(es24.16e3)'

        allocate(input(10,10))

        do m = 0, 5
            do n = 1, 11
                call random_number(input)
                input = input - 0.5
                call savetxt('test_dp_max_skip.txt', input)
                call loadtxt('test_dp_max_skip.txt', expected, skiprows=m, max_rows=n)
                call check(error, all(input(m+1:min(n+m,10),:) == expected),'Default format read failed')
                if (allocated(error)) return
                call loadtxt('test_dp_max_skip.txt', expected, skiprows=m, max_rows=n, fmt='*')
                call check(error, all(input(m+1:min(n+m,10),:) == expected),'List directed read failed')
                if (allocated(error)) return
                call loadtxt('test_dp_max_skip.txt', expected, fmt="(*"//FMT_REAL_dp(1:len(FMT_REAL_dp)-1)//",1x))")
                call check(error, all(input == expected),'User specified format failed')
                deallocate(expected)
                if (allocated(error)) return
            end do
        end do

    end subroutine test_loadtxt_dp_max_skip


    subroutine test_loadtxt_dp_huge(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:,:), expected(:,:)
        integer :: n
        character(len=*), parameter :: FMT_REAL_DP = '(es24.16e3)'

        allocate(input(10,10))
        allocate(expected(10,10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * huge(input)
            call savetxt('test_dp_huge.txt', input)
            call loadtxt('test_dp_huge.txt', expected)
            call check(error, all(input == expected),'Default format read failed')
            if (allocated(error)) return
            call loadtxt('test_dp_huge.txt', expected, fmt='*')
            call check(error, all(input == expected),'List directed read failed')
            if (allocated(error)) return
            call loadtxt('test_dp_huge.txt', expected, fmt="(*"//FMT_REAL_dp(1:len(FMT_REAL_dp)-1)//",1x))")
            call check(error, all(input == expected),'User specified format failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_dp_huge


    subroutine test_loadtxt_dp_tiny(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:,:), expected(:,:)
        integer :: n
        character(len=*), parameter :: FMT_REAL_DP = '(es24.16e3)'
        
        allocate(input(10,10))
        allocate(expected(10,10))

        do n = 1, 10
            call random_number(input)
            input = (input - 0.5) * tiny(input)
            call savetxt('test_dp_tiny.txt', input)
            call loadtxt('test_dp_tiny.txt', expected)
            call check(error, all(input == expected),'Default format read failed')
            if (allocated(error)) return
            call loadtxt('test_dp_tiny.txt', expected, fmt='*')
            call check(error, all(input == expected),'List directed read failed')
            if (allocated(error)) return
            call loadtxt('test_dp_tiny.txt', expected, fmt="(*"//FMT_REAL_dp(1:len(FMT_REAL_dp)-1)//",1x))")
            call check(error, all(input == expected),'User specified format failed')
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_dp_tiny


    subroutine test_loadtxt_complex(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        complex(dp), allocatable :: input(:,:), expected(:,:)
        real(dp), allocatable :: re(:,:), im(:,:)
        integer :: n
        character(len=*), parameter :: FMT_COMPLEX_DP = '(es24.16e3,1x,es24.16e3)'

        allocate(re(10,10))
        allocate(im(10,10))
        allocate(input(10,10))
        allocate(expected(10,10))

        do n = 1, 10
            call random_number(re)
            call random_number(im)
            input = cmplx(re, im)
            call savetxt('test_complex.txt', input)
            call loadtxt('test_complex.txt', expected)
            call check(error, all(input == expected))
            call loadtxt('test_complex.txt', expected, fmt="(*"//FMT_COMPLEX_dp(1:len(FMT_COMPLEX_dp)-1)//",1x))")
            call check(error, all(input == expected))
            if (allocated(error)) return
            call savetxt('test_complex.txt', input, delimiter=',')
            call loadtxt('test_complex.txt', expected, delimiter=',')
            call check(error, all(input == expected))
            if (allocated(error)) return
            call savetxt('test_complex.txt', input, delimiter=';')
            call loadtxt('test_complex.txt', expected, delimiter=';')
            call check(error, all(input == expected))
            if (allocated(error)) return
        end do

    end subroutine test_loadtxt_complex

end module test_loadtxt


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_loadtxt, only : collect_loadtxt
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0
    testsuites = [ &
        new_testsuite("loadtxt", collect_loadtxt) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if

end program tester
