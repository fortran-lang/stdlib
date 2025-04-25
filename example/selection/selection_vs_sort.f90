program selection_vs_sort
  use stdlib_kinds, only: int64
  use stdlib_selection, only: select, arg_select
  use stdlib_sorting, only: sort
  implicit none

  call compare_select_sort_for_median(1)
  call compare_select_sort_for_median(11)
  call compare_select_sort_for_median(101)
  call compare_select_sort_for_median(1001)
  call compare_select_sort_for_median(10001)
  call compare_select_sort_for_median(100001)

contains
  subroutine compare_select_sort_for_median(N)
    integer, intent(in) :: N

    integer :: i, k, result_arg_select, indx(N), indx_local(N)
    real :: random_vals(N), local_random_vals(N)
    integer, parameter :: test_reps = 100
    integer(int64) :: t0, t1
    real :: result_sort, result_select
    integer(int64) :: time_sort, time_select, time_arg_select
    logical :: select_test_passed, arg_select_test_passed

! Ensure N is odd
    if (mod(N, 2) /= 1) stop

    time_sort = 0
    time_select = 0
    time_arg_select = 0

    select_test_passed = .true.
    arg_select_test_passed = .true.

    indx = (/(i, i=1, N)/)

    k = (N + 1)/2 ! Deliberate integer division

    do i = 1, test_reps
      call random_number(random_vals)

! Compute the median with sorting
      local_random_vals = random_vals
      call system_clock(t0)
      call sort(local_random_vals)
      result_sort = local_random_vals(k)
      call system_clock(t1)
      time_sort = time_sort + (t1 - t0)

! Compute the median with selection, assuming N is odd
      local_random_vals = random_vals
      call system_clock(t0)
      call select(local_random_vals, k, result_select)
      call system_clock(t1)
      time_select = time_select + (t1 - t0)

! Compute the median with arg_select, assuming N is odd
      local_random_vals = random_vals
      indx_local = indx
      call system_clock(t0)
      call arg_select(local_random_vals, indx_local, k, result_arg_select)
      call system_clock(t1)
      time_arg_select = time_arg_select + (t1 - t0)

      if (result_select /= result_sort) select_test_passed = .FALSE.
      if (local_random_vals(result_arg_select) /= result_sort) arg_select_test_passed = .FALSE.
    end do

    print *, "select    ; N=", N, '; ', merge('PASS', 'FAIL', select_test_passed), &
      '; Relative-speedup-vs-sort:', (1.0*time_sort)/(1.0*time_select)
    print *, "arg_select; N=", N, '; ', merge('PASS', 'FAIL', arg_select_test_passed), &
      '; Relative-speedup-vs-sort:', (1.0*time_sort)/(1.0*time_arg_select)

  end subroutine

end program
