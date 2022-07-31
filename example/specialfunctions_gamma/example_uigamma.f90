program example_uigamma
  use stdlib_specialfunctions_gamma, only: uig => upper_incomplete_gamma
  implicit none

  print *, uig(3, -5.0)

!2523.02295

  print *, uig(2.3, 5.0)

!6.95552528E-02
end program example_uigamma
