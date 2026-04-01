program test_regex
  use stdlib_regex
  implicit none

  type(regex_type) :: re
  integer :: stat, match_start, match_end
  logical :: is_match

  print *, "=== Testing Fortran Regex (Thompson NFA) ==="

  ! Test 1: Basic characters
  call regcomp(re, "abc", stat)
  print *, "regcomp 'abc': status = ", stat
  call regmatch(re, "xyz_abc_def", is_match, match_start, match_end)
  print *, "Match 'xyz_abc_def' -> ", is_match, match_start, match_end

  ! Test 2: Star operator
  call regcomp(re, "a*b", stat)
  call regmatch(re, "aaaab", is_match, match_start, match_end)
  print *, "Match 'aaaab' with 'a*b' -> ", is_match, match_start, match_end

  ! Test 3: Plus and character classes
  call regcomp(re, "[0-9]+", stat)
  call regmatch(re, "foo123bar", is_match, match_start, match_end)
  print *, "Match 'foo123bar' with '[0-9]+' -> ", is_match, match_start, match_end
  
  ! Test 4: Alternation and grouping
  call regcomp(re, "(dog|cat)s?", stat)
  call regmatch(re, "I have cats and dogs.", is_match, match_start, match_end)
  print *, "Match 'cats' with '(dog|cat)s?' -> ", is_match, match_start, match_end
  
  ! Test 5: Anchors
  call regcomp(re, "^foo", stat)
  call regmatch(re, "bar foo", is_match)
  print *, "Match 'bar foo' with '^foo' -> ", is_match
  call regmatch(re, "foo bar", is_match)
  print *, "Match 'foo bar' with '^foo' -> ", is_match

  print *, "=== End Tests ==="
end program test_regex
