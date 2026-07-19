program example_regex_regcomp
  use stdlib_regex, only: regex_type, regcomp
  implicit none
  type(regex_type) :: re
  integer :: stat

  call regcomp(re, "(cat|dog)s?", stat)
  if (stat /= 0) error stop "Invalid regex pattern"
  print *, "Pattern compiled successfully."
end program example_regex_regcomp
