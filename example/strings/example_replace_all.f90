program example_replace_all
  use stdlib_string_type, only: string_type, assignment(=), write (formatted)
  use stdlib_strings, only: replace_all
  implicit none
  type(string_type) :: string

  string = "hurdles here, hurdles there, hurdles everywhere"
! string <-- "hurdles here, hurdles there, hurdles everywhere"

  print'(dt)', replace_all(string, "hurdles", "learn from")
! "learn from here, learn from there, learn from everywhere"

  string = replace_all(string, "hurdles", "technology")
! string <-- "technology here, technology there, technology everywhere"

end program example_replace_all
