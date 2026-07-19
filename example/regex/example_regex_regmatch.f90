program example_regex_regmatch
  use stdlib_regex, only: regex_type, regcomp, regmatch
  implicit none
  type(regex_type) :: re
  logical :: found
  integer :: stat, ms, me

  ! Find a sequence of digits
  call regcomp(re, "[0-9]+", stat)
  call regmatch(re, "foo123bar", found, ms, me)
  print "(a,l1,a,i0,a,i0)", "found = ", found, ", ms = ", ms, ", me = ", me

  ! Anchored match
  call regcomp(re, "^hello", stat)
  call regmatch(re, "hello world", found)
  print "(a,l1)", "found = ", found
  call regmatch(re, "say hello", found)
  print "(a,l1)", "found = ", found

  ! Alternation with optional suffix
  call regcomp(re, "(cat|dog)s?", stat)
  call regmatch(re, "I like cats", found, ms, me)
  print "(a,l1,a,i0,a,i0)", "found = ", found, ", ms = ", ms, ", me = ", me

end program example_regex_regmatch
