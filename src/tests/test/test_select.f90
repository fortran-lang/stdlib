! SPDX-Identifier: MIT

module test_select
  use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
  use stdlib_test, only : new_unittest, unittest_type, error_type, check, &
    & run_testsuite, new_testsuite, testsuite_type, select_suite, run_selected
  implicit none
  private

  public :: collect_select


contains


  !> Collect all exported unit tests
  subroutine collect_select(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("always-pass", always_pass), &
      new_unittest("always-fail", always_fail, should_fail=.true.), &
      new_unittest("run-good-suite", test_run_good_suite), &
      new_unittest("run-bad-suite", test_run_bad_suite), &
      new_unittest("run-selected", test_run_selected), &
      new_unittest("select-missing", test_select_missing) &
      ]

  end subroutine collect_select


  subroutine always_pass(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, 0)

  end subroutine always_pass


  subroutine always_fail(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    call check(error, 1, "Always failing test")

  end subroutine always_fail


  !> Stub test suite collector defining passing unit tests
  subroutine stub_collect(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("always-pass", always_pass), &
      new_unittest("always-fail", always_fail, should_fail=.true.) &
      ]

  end subroutine stub_collect


  !> Bad test suite collector defining flaky unit tests
  subroutine stub_collect_bad(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("always-pass", always_pass, should_fail=.true.), &
      new_unittest("always-fail", always_fail) &
      ]

  end subroutine stub_collect_bad


  subroutine test_run_good_suite(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: unit, stat

    open(status='scratch', newunit=unit)

    stat = 7
    call run_testsuite(stub_collect, unit, stat)
    call check(error, stat, 7)

    close(unit)

  end subroutine test_run_good_suite


  subroutine test_run_bad_suite(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: unit, stat

    open(status='scratch', newunit=unit)

    stat = 3
    call run_testsuite(stub_collect_bad, unit, stat)
    call check(error, stat, 5)

    close(unit)

  end subroutine test_run_bad_suite


  subroutine test_run_selected(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: unit, stat

    open(status='scratch', newunit=unit)

    stat = 1
    call run_selected(stub_collect, "always-fail", unit, stat)
    call check(error, stat, 1)

    close(unit)

  end subroutine test_run_selected


  subroutine test_select_missing(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: unit, stat

    open(status='scratch', newunit=unit)

    call run_selected(stub_collect, "not-available", unit, stat)
    call check(error, stat < 0)

    close(unit)

  end subroutine test_select_missing


end module test_select


program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use stdlib_test, only : run_testsuite, new_testsuite, testsuite_type
  use test_select, only : collect_select
  implicit none
  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0

  testsuites = [ &
    new_testsuite("select", collect_select) &
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
