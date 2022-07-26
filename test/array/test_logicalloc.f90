! SPDX-Identifier: MIT

module test_logicalloc
  use stdlib_array, only : trueloc, falseloc
  use stdlib_kinds, only : dp, i8 => int64
  use stdlib_strings, only : to_string
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
      new_unittest("trueloc-empty", test_trueloc_empty), &
      new_unittest("trueloc-all", test_trueloc_all), &
      new_unittest("trueloc-where", test_trueloc_where), &
      new_unittest("trueloc-merge", test_trueloc_merge), &
      new_unittest("trueloc-pack", test_trueloc_pack), &
      new_unittest("falseloc-empty", test_falseloc_empty), &
      new_unittest("falseloc-all", test_falseloc_all), &
      new_unittest("falseloc-where", test_falseloc_where), &
      new_unittest("falseloc-merge", test_falseloc_merge), &
      new_unittest("falseloc-pack", test_falseloc_pack) &
      ]
  end subroutine collect_logicalloc

  subroutine test_trueloc_empty(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:)

    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)

      bvec = avec
      bvec(trueloc(bvec < 0)) = 0.0

      call check(error, all(bvec == avec))
      deallocate(avec, bvec)
      if (allocated(error)) exit
    end do
  end subroutine test_trueloc_empty

  subroutine test_trueloc_all(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:)

    do ndim = 100, 12000, 100
      allocate(avec(-ndim/2:ndim))

      call random_number(avec)

      avec(trueloc(avec > 0, lbound(avec, 1))) = 0.0

      call check(error, all(avec == 0.0))
      deallocate(avec)
      if (allocated(error)) exit
    end do
  end subroutine test_trueloc_all

  subroutine test_trueloc_where(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)
    real(dp) :: tl, tw

    tl = 0.0_dp
    tw = 0.0_dp
    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      tl = tl - timing()
      bvec(trueloc(bvec > 0)) = 0.0
      tl = tl + timing()

      cvec = avec
      tw = tw - timing()
      where(cvec > 0) cvec = 0.0
      tw = tw + timing()

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
    call report("trueloc", tl, "where", tw)
  end subroutine test_trueloc_where

  subroutine test_trueloc_merge(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)
    real(dp) :: tl, tm

    tl = 0.0_dp
    tm = 0.0_dp
    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      tl = tl - timing()
      bvec(trueloc(bvec > 0)) = 0.0
      tl = tl + timing()

      cvec = avec
      tm = tm - timing()
      cvec(:) = merge(0.0, cvec, cvec > 0)
      tm = tm + timing()

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
    call report("trueloc", tl, "merge", tm)
  end subroutine test_trueloc_merge

  subroutine test_trueloc_pack(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)
    real(dp) :: tl, tp

    tl = 0.0_dp
    tp = 0.0_dp
    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      tl = tl - timing()
      bvec(trueloc(bvec > 0)) = 0.0
      tl = tl + timing()

      cvec = avec
      tp = tp - timing()
      block
        integer :: i
        cvec(pack([(i, i=1, size(cvec))], cvec > 0)) = 0.0
      end block
      tp = tp + timing()

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
    call report("trueloc", tl, "pack", tp)
  end subroutine test_trueloc_pack

  subroutine test_falseloc_empty(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:)

    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)

      bvec = avec
      bvec(falseloc(bvec > 0)) = 0.0

      call check(error, all(bvec == avec))
      deallocate(avec, bvec)
      if (allocated(error)) exit
    end do
  end subroutine test_falseloc_empty

  subroutine test_falseloc_all(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:)

    do ndim = 100, 12000, 100
      allocate(avec(-ndim/2:ndim))

      call random_number(avec)

      avec(falseloc(avec < 0, lbound(avec, 1))) = 0.0

      call check(error, all(avec == 0.0))
      deallocate(avec)
      if (allocated(error)) exit
    end do
  end subroutine test_falseloc_all

  subroutine test_falseloc_where(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)
    real(dp) :: tl, tw

    tl = 0.0_dp
    tw = 0.0_dp
    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      tl = tl - timing()
      bvec(falseloc(bvec > 0)) = 0.0
      tl = tl + timing()

      cvec = avec
      tw = tw - timing()
      where(.not.(cvec > 0)) cvec = 0.0
      tw = tw + timing()

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
    call report("falseloc", tl, "where", tw)
  end subroutine test_falseloc_where

  subroutine test_falseloc_merge(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)
    real(dp) :: tl, tm

    tl = 0.0_dp
    tm = 0.0_dp
    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      tl = tl - timing()
      bvec(falseloc(bvec > 0)) = 0.0
      tl = tl + timing()

      cvec = avec
      tm = tm - timing()
      cvec(:) = merge(cvec, 0.0, cvec > 0)
      tm = tm + timing()

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
    call report("falseloc", tl, "merge", tm)
  end subroutine test_falseloc_merge

  subroutine test_falseloc_pack(error)
    !> Error handling
    type(error_type), allocatable, intent(out) :: error

    integer :: ndim
    real, allocatable :: avec(:), bvec(:), cvec(:)
    real(dp) :: tl, tp

    tl = 0.0_dp
    tp = 0.0_dp
    do ndim = 100, 12000, 100
      allocate(avec(ndim))

      call random_number(avec)
      avec(:) = avec - 0.5

      bvec = avec
      tl = tl - timing()
      bvec(falseloc(bvec > 0)) = 0.0
      tl = tl + timing()

      cvec = avec
      tp = tp - timing()
      block
        integer :: i
        cvec(pack([(i, i=1, size(cvec))], cvec < 0)) = 0.0
      end block
      tp = tp + timing()

      call check(error, all(bvec == cvec))
      deallocate(avec, bvec, cvec)
      if (allocated(error)) exit
    end do
    call report("falseloc", tl, "pack", tp)
  end subroutine test_falseloc_pack

  subroutine report(l1, t1, l2, t2)
    character(len=*), intent(in) :: l1, l2
    real(dp), intent(in) :: t1, t2
    character(len=*), parameter :: fmt = "f6.4"

    !$omp critical
    print '(2x, "[Timing]", *(1x, g0))', &
      l1//":", to_string(t1, fmt)//"s", &
      l2//":", to_string(t2, fmt)//"s", &
      "ratio:", to_string(t1/t2, "f4.1")
    !$omp end critical
  end subroutine report

  function timing() result(time)
    real(dp) :: time

    integer(i8) :: time_count, time_rate, time_max
    call system_clock(time_count, time_rate, time_max)
    time = real(time_count, dp)/real(time_rate, dp)
  end function timing

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
