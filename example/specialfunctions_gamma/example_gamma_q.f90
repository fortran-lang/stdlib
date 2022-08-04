program example_gamma_q
  use stdlib_specialfunctions_gamma, only: rgq => regularized_gamma_q
  implicit none

  print *, rgq(3.0, 5.0)

! 0.124652028
end program example_gamma_q
