program example_starts_with
  use stdlib_strings, only: starts_with
  implicit none
  print'(l1)', starts_with("pattern", "pat")  ! T
  print'(l1)', starts_with("pattern", "ern")  ! F
end program example_starts_with
