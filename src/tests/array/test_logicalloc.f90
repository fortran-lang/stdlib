! SPDX-Identifier: MIT

module test_logicalloc
  use stdlib_array, only : trueloc, falseloc
  use stdlib_string_type, only : string_type, len
  use testdrive, only : new_unittest, unittest_type, error_type, check
  implicit none
  private

  public :: collect_logicalloc

contains

  !> Collect all exported unit tests
  subroutine collect_logicalloc(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: testsuite(:)

    testsuite = [ &
      new_unittest("trueloc-where", test_trueloc_where), &
      new_unittest("trueloc-merge", test_trueloc_merge), &
      new_unittest("falseloc-where", test_falseloc_where), &
      new_unittest("falseloc-merge", test_falseloc_merge) &
      ]
  end subroutine collect_logicalloc

  subroutine test_trueloc_where(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)

    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      bvec(trueloc(bvec > 0)) = 0.0

      cvec = avec
      where(cvec > 0) cvec = 0.0

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
  end subroutine test_trueloc_where

  subroutine test_trueloc_merge(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)

    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      bvec(trueloc(bvec > 0)) = 0.0

      cvec = avec
      cvec(:) = merge(0.0, cvec, cvec > 0)

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
  end subroutine test_trueloc_merge

  subroutine test_falseloc_where(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)

    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      bvec(falseloc(bvec > 0)) = 0.0

      cvec = avec
      where(.not.(cvec > 0)) cvec = 0.0

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
  end subroutine test_falseloc_where

  subroutine test_falseloc_merge(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)

    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      bvec(falseloc(bvec > 0)) = 0.0

      cvec = avec
      cvec(:) = merge(cvec, 0.0, cvec > 0)

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
  end subroutine test_falseloc_merge

end module test_logicalloc


program tester
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only : run_testsuite, new_testsuite, testsuite_type
  use test_logicalloc, only : collect_logicalloc
  implicit none
  integer :: stat, is
  type(testsuite_type), allocatable :: testsuites(:)
  character(len=*), parameter :: fmt = '("#", *(1x, a))'

  stat = 0

  testsuites = [ &
    new_testsuite("logicalloc", collect_logicalloc) &
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
