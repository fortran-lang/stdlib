module test_sleep
  use, intrinsic :: iso_fortran_env, only : int64, real64
  use stdlib_system, only : sleep
  use testdrive, only: new_unittest, unittest_type, error_type, check
  implicit none

  private
  public :: collect_sleep

  integer, parameter :: millisec = 100

contains

  !> Collect all exported unit tests
  subroutine collect_sleep(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest('sleep', test_sleep_) &
    ]

  end subroutine collect_sleep


  subroutine test_sleep_(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer(int64) :: tic, toc, trate
    real(real64) :: t_ms

    call system_clock(count_rate=trate)

    call system_clock(count=tic)
    call sleep(millisec)
    call system_clock(count=toc)

    t_ms = (toc - tic) * 1000._real64 / trate

    call check(error, t_ms, real(millisec, real64), thr=1.5_real64, rel=.true.)

  end subroutine test_sleep_

end module test_sleep


program tester
  use, intrinsic :: iso_fortran_env, only: error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type
  use test_sleep, only: collect_sleep
  implicit none
  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0

  testsuites = [ &
    new_testsuite('sleep', collect_sleep) &
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
