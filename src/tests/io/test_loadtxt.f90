module test_loadtxt
    use stdlib_kinds, only: int32, sp, dp
    use stdlib_io, only: loadtxt, savetxt
    use stdlib_test, only: new_unittest, unittest_type, error_type, check
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
            new_unittest("loadtxt_dp", test_loadtxt_dp), &
            new_unittest("loadtxt_complex", test_loadtxt_complex) &
        ]

    end subroutine collect_loadtxt


    subroutine test_loadtxt_int32(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: input(:,:), expected(:,:)

        call loadtxt("array1.dat", input)
        call savetxt("array1_new.dat", input)
        call loadtxt("array1_new.dat", expected)
        call check(error, all(input == expected))
        if (.not. allocated(error)) return

        call loadtxt("array2.dat", input)
        call savetxt("array2_new.dat", input)
        call loadtxt("array2_new.dat", expected)
        call check(error, all(input == expected))
        if (.not. allocated(error)) return

    end subroutine test_loadtxt_int32


    subroutine test_loadtxt_sp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(sp), allocatable :: input(:,:), expected(:,:)

        call loadtxt("array3.dat", input)
        call savetxt("array3_new.dat", input)
        call loadtxt("array3_new.dat", expected)
        call check(error, all(input == expected))
        if (.not. allocated(error)) return

        call loadtxt("array4.dat", input)
        call savetxt("array4_new.dat", input)
        call loadtxt("array4_new.dat", expected)
        call check(error, all(input == expected))
        if (.not. allocated(error)) return

    end subroutine test_loadtxt_sp


    subroutine test_loadtxt_dp(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error
        real(dp), allocatable :: input(:,:), expected(:,:)

        call loadtxt("array3.dat", input)
        call savetxt("array3_new.dat", input)
        call loadtxt("array3_new.dat", expected)
        call check(error, all(input == expected))
        if (.not. allocated(error)) return

        call loadtxt("array4.dat", input)
        call savetxt("array4_new.dat", input)
        call loadtxt("array4_new.dat", expected)
        call check(error, all(input == expected))
        if (.not. allocated(error)) return

    end subroutine test_loadtxt_dp


    subroutine test_loadtxt_complex(error)
      !> Error handling
      type(error_type), allocatable, intent(out) :: error
      complex(dp), allocatable :: input(:,:), expected(:,:)

      call loadtxt("array5.dat", input)
      call savetxt("array5_new.dat", input)
      call loadtxt("array5_new.dat", expected)
      call check(error, all(input == expected))
      if (.not. allocated(error)) return

  end subroutine test_loadtxt_complex

end module test_loadtxt


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use stdlib_test, only : run_testsuite, new_testsuite, testsuite_type
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
