program example_unique
  use stdlib_kinds, only: dp
  use stdlib_sorting, only: unique
  implicit none

  real(dp), allocatable :: A(:)
  real(dp), allocatable :: B(:)

  A = [4.0_dp, 5.0_dp, 4.0_dp, 3.0_dp, -1.0_dp, 4.0_dp, 2.0_dp]

  B = unique(A, .true.)
  print *, B ![-1.0_dp, 2.0_dp, 3.0_dp, 4.0_dp, 5.0_dp]

  B = unique(A, .false.)
  print *, B ![4.0_dp, 5.0_dp, 3.0_dp, -1.0_dp, 2.0_dp]
end program example_unique
