program example_determinant
  use stdlib_kinds, only: dp
  use stdlib_linalg, only: det, linalg_state_type
  implicit none

  real(dp) :: d

  ! Compute determinate of a real matrix
  d = det(reshape([real(dp)::1,2,3,4],[2,2]))

  print *, d ! a*d-b*c = -2.0

end program example_determinant
