program example_ends_with
  use stdlib_strings, only: ends_with
  implicit none
  print'(a)', ends_with("pattern", "ern")  ! T
  print'(a)', ends_with("pattern", "pat")  ! F
end program example_ends_with
