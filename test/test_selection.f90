! Specify kinds/types for the input array in select and arg_select
! The index arrays are of all INT_KINDS_TYPES


module test_selection

    use stdlib_kinds
    use stdlib_selection, only: select, arg_select
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none

    private
    public :: collect_selection

contains

    !> Collect all exported unit tests
    subroutine collect_selection(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
              new_unittest("test_select_1_iint8_int8", test_select_1_iint8_int8) &
              , new_unittest("test_select_1_iint8_int8", test_select_1_iint8_int8) &
              , new_unittest("test_select_1_iint8_int16", test_select_1_iint8_int16) &
              , new_unittest("test_select_1_iint8_int32", test_select_1_iint8_int32) &
              , new_unittest("test_select_1_iint8_int64", test_select_1_iint8_int64) &
              , new_unittest("test_select_1_iint16_int8", test_select_1_iint16_int8) &
              , new_unittest("test_select_1_iint16_int16", test_select_1_iint16_int16) &
              , new_unittest("test_select_1_iint16_int32", test_select_1_iint16_int32) &
              , new_unittest("test_select_1_iint16_int64", test_select_1_iint16_int64) &
              , new_unittest("test_select_1_iint32_int8", test_select_1_iint32_int8) &
              , new_unittest("test_select_1_iint32_int16", test_select_1_iint32_int16) &
              , new_unittest("test_select_1_iint32_int32", test_select_1_iint32_int32) &
              , new_unittest("test_select_1_iint32_int64", test_select_1_iint32_int64) &
              , new_unittest("test_select_1_iint64_int8", test_select_1_iint64_int8) &
              , new_unittest("test_select_1_iint64_int16", test_select_1_iint64_int16) &
              , new_unittest("test_select_1_iint64_int32", test_select_1_iint64_int32) &
              , new_unittest("test_select_1_iint64_int64", test_select_1_iint64_int64) &
              , new_unittest("test_select_1_rsp_int8", test_select_1_rsp_int8) &
              , new_unittest("test_select_1_rsp_int16", test_select_1_rsp_int16) &
              , new_unittest("test_select_1_rsp_int32", test_select_1_rsp_int32) &
              , new_unittest("test_select_1_rsp_int64", test_select_1_rsp_int64) &
              , new_unittest("test_select_1_rdp_int8", test_select_1_rdp_int8) &
              , new_unittest("test_select_1_rdp_int16", test_select_1_rdp_int16) &
              , new_unittest("test_select_1_rdp_int32", test_select_1_rdp_int32) &
              , new_unittest("test_select_1_rdp_int64", test_select_1_rdp_int64) &

              , new_unittest("test_arg_select_1_iint8_int8", test_arg_select_1_iint8_int8) &
              , new_unittest("test_arg_select_1_iint8_int16", test_arg_select_1_iint8_int16) &
              , new_unittest("test_arg_select_1_iint8_int32", test_arg_select_1_iint8_int32) &
              , new_unittest("test_arg_select_1_iint8_int64", test_arg_select_1_iint8_int64) &
              , new_unittest("test_arg_select_1_iint16_int8", test_arg_select_1_iint16_int8) &
              , new_unittest("test_arg_select_1_iint16_int16", test_arg_select_1_iint16_int16) &
              , new_unittest("test_arg_select_1_iint16_int32", test_arg_select_1_iint16_int32) &
              , new_unittest("test_arg_select_1_iint16_int64", test_arg_select_1_iint16_int64) &
              , new_unittest("test_arg_select_1_iint32_int8", test_arg_select_1_iint32_int8) &
              , new_unittest("test_arg_select_1_iint32_int16", test_arg_select_1_iint32_int16) &
              , new_unittest("test_arg_select_1_iint32_int32", test_arg_select_1_iint32_int32) &
              , new_unittest("test_arg_select_1_iint32_int64", test_arg_select_1_iint32_int64) &
              , new_unittest("test_arg_select_1_iint64_int8", test_arg_select_1_iint64_int8) &
              , new_unittest("test_arg_select_1_iint64_int16", test_arg_select_1_iint64_int16) &
              , new_unittest("test_arg_select_1_iint64_int32", test_arg_select_1_iint64_int32) &
              , new_unittest("test_arg_select_1_iint64_int64", test_arg_select_1_iint64_int64) &
              , new_unittest("test_arg_select_1_rsp_int8", test_arg_select_1_rsp_int8) &
              , new_unittest("test_arg_select_1_rsp_int16", test_arg_select_1_rsp_int16) &
              , new_unittest("test_arg_select_1_rsp_int32", test_arg_select_1_rsp_int32) &
              , new_unittest("test_arg_select_1_rsp_int64", test_arg_select_1_rsp_int64) &
              , new_unittest("test_arg_select_1_rdp_int8", test_arg_select_1_rdp_int8) &
              , new_unittest("test_arg_select_1_rdp_int16", test_arg_select_1_rdp_int16) &
              , new_unittest("test_arg_select_1_rdp_int32", test_arg_select_1_rdp_int32) &
              , new_unittest("test_arg_select_1_rdp_int64", test_arg_select_1_rdp_int64) &
          ]

    end subroutine collect_selection

      subroutine test_select_1_iint8_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int8) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint8_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint8_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int8: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint8_int8: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int8: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int8: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int8: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint8_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint8_int8: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint8_int8: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int8: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int8: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int8: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int8: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int8: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint8_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int16) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint8_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint8_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int16: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint8_int16: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int16: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int16: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int16: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint8_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint8_int16: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint8_int16: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int16: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int16: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int16: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int16: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int16: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint8_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int32) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint8_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint8_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int32: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint8_int32: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int32: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int32: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int32: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint8_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint8_int32: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint8_int32: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int32: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int32: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int32: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int32: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int32: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint8_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int64) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint8_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint8_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint8_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int64: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint8_int64: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int64: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int64: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint8_int64: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint8_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint8_int64: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint8_int64: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int64: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint8_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int64: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int64: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int64: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint8_int64: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint16_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int8) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint16_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint16_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int8: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint16_int8: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int8: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int8: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int8: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint16_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint16_int8: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint16_int8: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int8: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int8: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int8: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int8: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int8: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint16_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int16) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint16_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint16_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int16: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint16_int16: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int16: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int16: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int16: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint16_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint16_int16: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint16_int16: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int16: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int16: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int16: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int16: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int16: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint16_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int32) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint16_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint16_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int32: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint16_int32: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int32: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int32: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int32: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint16_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint16_int32: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint16_int32: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int32: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int32: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int32: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int32: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int32: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint16_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int64) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint16_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint16_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint16_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int64: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint16_int64: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int64: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int64: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint16_int64: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint16_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint16_int64: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint16_int64: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int64: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint16_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int64: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int64: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int64: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint16_int64: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint32_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int8) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint32_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint32_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int8: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint32_int8: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int8: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int8: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int8: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint32_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint32_int8: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint32_int8: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int8: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int8: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int8: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int8: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int8: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint32_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int16) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint32_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint32_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int16: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint32_int16: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int16: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int16: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int16: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint32_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint32_int16: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint32_int16: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int16: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int16: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int16: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int16: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int16: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint32_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int32) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint32_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint32_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int32: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint32_int32: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int32: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int32: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int32: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint32_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint32_int32: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint32_int32: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int32: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int32: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int32: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int32: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int32: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint32_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int64) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint32_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint32_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint32_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int64: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint32_int64: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int64: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int64: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint32_int64: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint32_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint32_int64: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint32_int64: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int64: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint32_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int64: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int64: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int64: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint32_int64: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint64_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int8) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint64_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint64_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int8: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint64_int8: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int8: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int8: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int8: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint64_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint64_int8: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint64_int8: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int8: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int8: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int8: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int8: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int8: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint64_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int16) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint64_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint64_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int16: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint64_int16: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int16: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int16: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int16: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint64_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint64_int16: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint64_int16: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int16: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int16: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int16: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int16: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int16: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint64_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int32) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint64_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint64_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int32: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint64_int32: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int32: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int32: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int32: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint64_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint64_int32: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint64_int32: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int32: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int32: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int32: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int32: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int32: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_iint64_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int64) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_iint64_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_iint64_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_iint64_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int64: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_iint64_int64: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int64: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int64: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_iint64_int64: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_iint64_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_iint64_int64: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_iint64_int64: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int64: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_iint64_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int64: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int64: random data left-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int64: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_iint64_int64: random data&
                      & left-right-constrained select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rsp_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int8) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rsp_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rsp_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int8: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rsp_int8: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int8: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int8: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int8: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rsp_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rsp_int8: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rsp_int8: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int8: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int8: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int8: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int8: random data right-constrained select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int8: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rsp_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int16) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rsp_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rsp_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int16: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rsp_int16: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int16: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int16: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int16: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rsp_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rsp_int16: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rsp_int16: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int16: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int16: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int16: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int16: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int16: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rsp_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int32) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rsp_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rsp_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int32: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rsp_int32: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int32: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int32: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int32: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rsp_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rsp_int32: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rsp_int32: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int32: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int32: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int32: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int32: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int32: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rsp_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int64) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rsp_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rsp_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rsp_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int64: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rsp_int64: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int64: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int64: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rsp_int64: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rsp_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rsp_int64: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rsp_int64: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int64: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rsp_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int64: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int64: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int64: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rsp_int64: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rdp_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int8) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rdp_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rdp_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int8: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rdp_int8: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int8: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int8: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int8: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rdp_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rdp_int8: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rdp_int8: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int8: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int8: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int8: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int8: random data right-constrained select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int8: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rdp_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int16) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rdp_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rdp_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int16: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rdp_int16: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int16: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int16: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int16: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rdp_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rdp_int16: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rdp_int16: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int16: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int16: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int16: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int16: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int16: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rdp_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int32) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rdp_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rdp_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int32: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rdp_int32: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int32: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int32: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int32: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rdp_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rdp_int32: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rdp_int32: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int32: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int32: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int32: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int32: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int32: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_select_1_rdp_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), x_copy(N), mat(Nm), mat_copy(Nm), len1(1), len2(2), &
              kth_smallest, random_vals(Nr), one = 1
          integer(int64) :: i, p, up_rank, down_rank, mid_rank
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)

          ! x contains the numbers 1**2, 2**2, .... 10**2, with mixed-up order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          ! Check that the ith-ranked entry of x really is i**2
          do i = 1, size(x, kind=ip)
              x_copy = x
              call select(x_copy, i, kth_smallest)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = 1, size(x, kind=ip), 1
              call select(x_copy, i, kth_smallest, left=i)
              call check( error,  (kth_smallest == i**2), " test_select_1_rdp_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the array
          ! is partially sorted due to previous calls to quickselect
          x_copy = x
          do i = size(x, kind=ip), 1, -1
              call select(x_copy, i, kth_smallest, right=i)
              call check( error, (kth_smallest == i**2), " test_select_1_rdp_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below can catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              call select(long_array, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest == one), " test_select_1_rdp_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          mat_copy = mat
          call select(mat_copy, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int64: mat test 1")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 2_ip, kth_smallest)
          call check(error, kth_smallest == 1, " test_select_1_rdp_int64: mat test 2")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, size(mat, kind=ip)+1_ip-4_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int64: mat test 3")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 5_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int64: mat test 4")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 6_ip, kth_smallest)
          call check(error, kth_smallest == 4, " test_select_1_rdp_int64: mat test 5")
          if(allocated(error)) return
          mat_copy = mat
          call select(mat_copy, 7_ip, kth_smallest)
          call check(error, kth_smallest == 5, " test_select_1_rdp_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          call select(len1, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -3, " test_select_1_rdp_int64: array with size 2, test 1")
          if(allocated(error)) return
          len2 = [-3, -5]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -5, " test_select_1_rdp_int64: array with size 2, test 2")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 1_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int64: array with size 2, test 3")
          if(allocated(error)) return
          len2 = [-1, -1]*one
          call select(len2, 2_ip, kth_smallest)
          call check(error, kth_smallest == -1, " test_select_1_rdp_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest rank, for all these p
          ! (avoid end-points to enable constrained search tests)
          do p = 3, Nr-2

              ! Repeat for different random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  call select(random_vals, p, kth_smallest)

                  test1 = kth_smallest == random_vals(p)
                  test2 = all(random_vals(1:(p-1)) <= random_vals(p))
                  test3 = all(random_vals(p) <= &
                      random_vals((p+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int64: random data regular select")
                  if(allocated(error)) return

                  ! Constrained search above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call select(random_vals, up_rank, kth_smallest, left=p)

                  test1 = kth_smallest == random_vals(up_rank)
                  test2 = all(random_vals(1:(up_rank-1)) <= random_vals(up_rank))
                  test3 = all(random_vals(up_rank) <= &
                      random_vals((up_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int64: random data left-constrained select")
                  if(allocated(error)) return

                  ! Constrained search below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call select(random_vals, down_rank, kth_smallest, right=p)

                  test1 = kth_smallest == random_vals(down_rank)
                  test2 = all(random_vals(1:(down_rank-1)) <= &
                      random_vals(down_rank))
                  test3 = all(random_vals(down_rank) <= &
                      random_vals((down_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int64: random data right-constrained&
                      & select")
                  if(allocated(error)) return

                  ! Constrained search between up-ind and down-ind, proving left
                  ! and right. Make 'mid_rank' either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call select(random_vals, mid_rank, kth_smallest, &
                      left=down_rank, right=up_rank)

                  test1 = kth_smallest == random_vals(mid_rank)
                  test2 = all(random_vals(1:(mid_rank-1)) <= &
                      random_vals(mid_rank))
                  test3 = all(random_vals(mid_rank) <= &
                      random_vals((mid_rank+1):size(random_vals, kind=ip)))
                  call check(error, (test1 .and. test2 .and. test3), "test_select_1_rdp_int64: random data left-right-constrained&
                      & select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine


      subroutine test_arg_select_1_iint8_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)
          integer(int8), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint8_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint8_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint8_int8: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint8_int8: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int8: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int8: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int8: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint8_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint8_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint8_int8: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint8_int8: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int8: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int8: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int8: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int8: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int8: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint8_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)
          integer(int16), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint8_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int16: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint8_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint8_int16: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint8_int16: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int16: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int16: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int16: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint8_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint8_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint8_int16: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint8_int16: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int16: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int16: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int16: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int16: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int16: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint8_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)
          integer(int32), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint8_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int32: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint8_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint8_int32: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint8_int32: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int32: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int32: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int32: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint8_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint8_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint8_int32: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint8_int32: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int32: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int32: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int32: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int32: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int32: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint8_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int8) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int8), allocatable :: long_array(:)
          integer(int64), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint8_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint8_int64: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint8_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint8_int64: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint8_int64: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int64: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int64: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint8_int64: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint8_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint8_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint8_int64: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint8_int64: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int64: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint8_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int64: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int64: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int64: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint8_int64: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint16_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)
          integer(int8), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint16_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int8: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint16_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint16_int8: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint16_int8: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int8: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int8: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int8: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint16_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint16_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint16_int8: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint16_int8: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int8: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int8: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int8: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int8: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int8: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint16_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)
          integer(int16), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint16_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int16: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int16: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint16_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint16_int16: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint16_int16: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int16: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int16: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int16: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint16_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint16_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint16_int16: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint16_int16: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int16: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int16: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int16: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int16: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int16: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint16_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)
          integer(int32), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint16_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int32: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int32: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint16_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint16_int32: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint16_int32: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int32: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int32: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int32: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint16_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint16_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint16_int32: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint16_int32: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int32: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int32: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int32: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int32: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int32: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint16_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int16) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int16), allocatable :: long_array(:)
          integer(int64), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint16_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int64: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint16_int64: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint16_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint16_int64: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint16_int64: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int64: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int64: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint16_int64: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint16_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint16_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint16_int64: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint16_int64: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int64: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint16_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int64: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int64: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int64: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint16_int64: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint32_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)
          integer(int8), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint32_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int8: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint32_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint32_int8: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint32_int8: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int8: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int8: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int8: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint32_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint32_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint32_int8: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint32_int8: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int8: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int8: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int8: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int8: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int8: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint32_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)
          integer(int16), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint32_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int16: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int16: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint32_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint32_int16: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint32_int16: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int16: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int16: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int16: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint32_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint32_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint32_int16: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint32_int16: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int16: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int16: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int16: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int16: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int16: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint32_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)
          integer(int32), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint32_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int32: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int32: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint32_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint32_int32: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint32_int32: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int32: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int32: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int32: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint32_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint32_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint32_int32: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint32_int32: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int32: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int32: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int32: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int32: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int32: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint32_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int32) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int32), allocatable :: long_array(:)
          integer(int64), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint32_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int64: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint32_int64: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint32_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint32_int64: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint32_int64: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int64: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int64: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint32_int64: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint32_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint32_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint32_int64: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint32_int64: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int64: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint32_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int64: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int64: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int64: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint32_int64: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint64_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)
          integer(int8), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint64_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int8: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint64_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint64_int8: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint64_int8: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int8: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int8: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int8: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint64_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint64_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint64_int8: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint64_int8: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int8: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int8: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int8: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int8: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int8: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint64_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)
          integer(int16), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint64_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int16: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int16: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint64_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint64_int16: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint64_int16: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int16: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int16: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int16: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint64_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint64_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint64_int16: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint64_int16: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int16: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int16: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int16: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int16: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int16: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint64_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)
          integer(int32), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint64_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int32: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int32: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint64_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint64_int32: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint64_int32: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int32: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int32: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int32: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint64_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint64_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint64_int32: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint64_int32: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int32: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int32: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int32: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int32: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int32: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_iint64_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          integer(int64) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          integer(int64), allocatable :: long_array(:)
          integer(int64), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_iint64_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int64: kth smallest entry with left&
                  & specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_iint64_int64: kth smallest entry with right&
                  & specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_iint64_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_iint64_int64: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_iint64_int64: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int64: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int64: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_iint64_int64: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_iint64_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_iint64_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_iint64_int64: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_iint64_int64: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int64: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_iint64_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int64: random data regular&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int64: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int64: random data&
                      & right-constrained arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_iint64_int64: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rsp_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)
          integer(int8), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rsp_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rsp_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rsp_int8: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rsp_int8: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int8: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int8: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int8: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rsp_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rsp_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rsp_int8: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rsp_int8: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int8: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int8: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int8: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int8: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int8: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rsp_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)
          integer(int16), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rsp_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rsp_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rsp_int16: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rsp_int16: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int16: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int16: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int16: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rsp_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rsp_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rsp_int16: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rsp_int16: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int16: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int16: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int16: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int16: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int16: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rsp_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)
          integer(int32), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rsp_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rsp_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rsp_int32: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rsp_int32: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int32: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int32: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int32: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rsp_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rsp_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rsp_int32: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rsp_int32: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int32: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int32: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int32: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int32: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int32: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rsp_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(sp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(sp), allocatable :: long_array(:)
          integer(int64), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rsp_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rsp_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rsp_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rsp_int64: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rsp_int64: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int64: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int64: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rsp_int64: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rsp_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rsp_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rsp_int64: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rsp_int64: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int64: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rsp_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int64: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int64: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int64: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rsp_int64: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rdp_int8(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int8
          integer(int8), parameter :: N = 10, Nm = 8
          integer(int8), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int8), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int8), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)
          integer(int8), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rdp_int8: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int8: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int8: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rdp_int8: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rdp_int8: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rdp_int8: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int8: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int8: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int8: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rdp_int8: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rdp_int8: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rdp_int8: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rdp_int8: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int8: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int8: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int8: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int8: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int8: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int8: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rdp_int16(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int16
          integer(int16), parameter :: N = 10, Nm = 8
          integer(int16), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int16), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int16), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)
          integer(int16), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rdp_int16: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int16: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int16: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rdp_int16: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rdp_int16: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rdp_int16: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int16: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int16: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int16: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rdp_int16: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rdp_int16: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rdp_int16: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rdp_int16: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int16: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int16: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int16: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int16: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int16: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int16: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rdp_int32(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int32
          integer(int32), parameter :: N = 10, Nm = 8
          integer(int32), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int32), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int32), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)
          integer(int32), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rdp_int32: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int32: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int32: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rdp_int32: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rdp_int32: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rdp_int32: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int32: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int32: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int32: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rdp_int32: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rdp_int32: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rdp_int32: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rdp_int32: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int32: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int32: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int32: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int32: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int32: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int32: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine
      subroutine test_arg_select_1_rdp_int64(error)
          type(error_type), allocatable, intent(out) :: error

          integer, parameter :: ip = int64
          integer(int64), parameter :: N = 10, Nm = 8
          integer(int64), parameter :: near_huge = HUGE(N) - 1_ip ! Segfaults without the -1_ip
          integer(int64), parameter :: Nreps = 2  ! Number of repetitions of random sampling
          integer(int64), parameter :: Nr = 25_ip ! Size of random array, must be < HUGE(N)

          real(dp) :: x(N), mat(Nm), len1(1), len2(2), random_vals(Nr), one=1

          integer(ip) :: indx(N), indx_copy(N), indx_mat(Nm), indx_mat_copy(Nm), &
              indx_len1(1), indx_len2(2), indx_r(Nr)
          real(dp) :: random_doubles(Nr) ! Deliberately double precision for all cases
          integer(ip) :: i, j, p, up_rank, down_rank, mid_rank, kth_smallest
          logical :: test1, test2, test3
          real(dp), allocatable :: long_array(:)
          integer(int64), allocatable :: long_array_index(:)

          ! Make x contain 1**2, 2**2, .... 10**2, but mix up the order
          x = (/( i**2, i=1, size(x, kind=ip) )/)
          x(5:2:-1) = x(2:5)
          x(10:8:-1) = x(8:10)

          indx = (/(i, i = 1, size(x, kind=ip))/)

          ! Check that the ith ranked entry of x equals i**2
          do i = 1, size(x, kind=ip)
              indx_copy = indx
              call arg_select(x, indx, i, kth_smallest)
              call check(error, x(kth_smallest) == i**2, " test_arg_select_1_rdp_int64: kth smallest entry should be i**2")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "left" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = 1, size(x, kind=ip), 1
              call arg_select(x, indx_copy,  i, kth_smallest, left=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int64: kth smallest entry with left specified")
              if(allocated(error)) return
          end do

          ! Check that it works when we specify "right" and know that the index
          ! array is partially sorted due to previous calls to arg_select
          indx_copy = indx
          do i = size(x, kind=ip), 1, -1
              call arg_select(x, indx_copy, i, kth_smallest, right=i)
              call check(error, (x(kth_smallest) == i**2), " test_arg_select_1_rdp_int64: kth smallest entry with right specified")
              if(allocated(error)) return
          end do

          ! The test below would catch overflow in naive calculation of the middle index, like discussed here:
          ! https://ai.googleblog.com/2006/06/extra-extra-read-all-about-it-nearly.html
          ! But don't do it if near_huge is large, to avoid allocating a big array and slowing the tests
          if(near_huge < 200) then
              allocate(long_array(near_huge))
              allocate(long_array_index(near_huge))
              long_array = 0 * one
              long_array(1:3) = one
              long_array_index = (/( i, i = 1_ip, size(long_array, kind=ip) )/)
              call arg_select(long_array, long_array_index, near_huge - 2_ip, kth_smallest)
              call check( error, (kth_smallest < 4), " test_arg_select_1_rdp_int64: designed to catch overflow in middle index")
              if(allocated(error)) return
              deallocate(long_array, long_array_index)
          end if

          ! Simple tests
          mat = one * [3, 2, 7, 4, 5, 1, 4, -1]
          indx_mat = (/( i, i = 1, size(mat, kind=ip) )/)

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 1_ip, kth_smallest)
          call check(error, mat(kth_smallest) == -1, " test_arg_select_1_rdp_int64: mat test 1")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 2_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 1, " test_arg_select_1_rdp_int64: mat test 2")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, size(mat, kind=ip)+1_ip-4_ip, &
              kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int64: mat test 3")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 5_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int64: mat test 4")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 6_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 4, " test_arg_select_1_rdp_int64: mat test 5")
          if(allocated(error)) return

          indx_mat_copy = indx_mat
          call arg_select(mat, indx_mat_copy, 7_ip, kth_smallest)
          call check(error, mat(kth_smallest) == 5, " test_arg_select_1_rdp_int64: mat test 6")
          if(allocated(error)) return

          ! Check it works for size(a) == 1
          len1(1) = -1 * one
          indx_len1(1) = 1
          call arg_select(len1, indx_len1, 1_ip, kth_smallest)
          call check(error, len1(kth_smallest) == -1, " test_arg_select_1_rdp_int64: array with size 1")
          if(allocated(error)) return

          ! Check it works for size(a) == 2
          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -3, " test_arg_select_1_rdp_int64: array with size 2, test 1")
          if(allocated(error)) return

          len2 = [-3, -5] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -5, " test_arg_select_1_rdp_int64: array with size 2, test 2")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 1_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int64: array with size 2, test 3")
          if(allocated(error)) return

          len2 = [-1, -1] * one
          indx_len2 = [1_ip, 2_ip]
          call arg_select(len2, indx_len2, 2_ip, kth_smallest)
          call check(error, len2(kth_smallest) == -1, " test_arg_select_1_rdp_int64: array with size 2, test 4")
          if(allocated(error)) return

          !
          ! Test using random data
          !
          ! Search for the p-th smallest, for all these p (avoid end-points to
          ! enable additional tests using "left", "right" arguments)
          do p = 3, Nr-2

              ! Repeat for many random samples to try to expose any errors
              do i = 1, Nreps

                  ! Make random numbers of the correct type
                  call random_number(random_doubles)
                  random_vals = random_doubles * Nr

                  indx_r = (/( j, j = 1, size(random_vals, kind=ip) )/)

                  ! Standard arg_select
                  call arg_select(random_vals, indx_r, p, kth_smallest)

                  test1 = random_vals(kth_smallest) == random_vals(indx_r(p))
                  test2 = all(random_vals(indx_r(1:(p-1))) <= &
                      random_vals(indx_r(p)))
                  test3 = all(random_vals(indx_r(p)) <= &
                      random_vals(indx_r((p+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int64: random data regular arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank above 'p', providing 'left'
                  up_rank = p + (Nr-p)/2_ip ! Deliberate integer division
                  call arg_select(random_vals, indx_r, up_rank, &
                      kth_smallest, left=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(up_rank))
                  test2 = all(random_vals(indx_r(1:(up_rank-1))) <= &
                      random_vals(indx_r(up_rank)))
                  test3 = all(random_vals(indx_r(up_rank)) <= &
                      random_vals(indx_r((up_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int64: random data left-constrained&
                      & arg_select")
                  if(allocated(error)) return


                  ! Constrained search for a rank below p, providing 'right'
                  down_rank = p - (p/2_ip)
                  call arg_select(random_vals, indx_r, down_rank, &
                      kth_smallest, right=p)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(down_rank))
                  test2 = all(random_vals(indx_r(1:(down_rank-1))) <= &
                      random_vals(indx_r(down_rank)))
                  test3 = all(random_vals(indx_r(down_rank)) <= &
                      random_vals(indx_r((down_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int64: random data right-constrained&
                      & arg_select")
                  if(allocated(error)) return

                  ! Constrained search for a rank between up-ind and down-ind,
                  ! proving left and right. 'mid_rank' is either above or below p
                  mid_rank = p - p/3_ip*mod(i,2_ip) + (Nr-p)/3_ip*(1_ip-mod(i,2_ip))
                  call arg_select(random_vals, indx_r, mid_rank, &
                      kth_smallest, left=down_rank, right=up_rank)

                  test1 = random_vals(kth_smallest) == &
                      random_vals(indx_r(mid_rank))
                  test2 = all(random_vals(indx_r(1:(mid_rank-1))) <= &
                      random_vals(indx_r(mid_rank)))
                  test3 = all(random_vals(indx_r(mid_rank)) <= &
                      random_vals(indx_r((mid_rank+1):size(random_vals, kind=ip))))
                  call check(error, (test1 .and. test2 .and. test3), "test_arg_select_1_rdp_int64: random data&
                      & left-right-constrained arg_select")
                  if(allocated(error)) return

              end do
          end do

      end subroutine

end module

program tester
    use, intrinsic :: iso_fortran_env, only: compiler_version, error_unit
    use testdrive, only: new_testsuite, run_testsuite, testsuite_type
    use test_selection, only: collect_selection

    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("selection", collect_selection) &
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
