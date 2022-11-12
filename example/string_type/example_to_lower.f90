program example_to_lower
  use stdlib_string_type
  implicit none
  type(string_type) :: string, lowercase_string

  string = "Lowercase This String"
! string <-- "Lowercase This String"

  lowercase_string = to_lower(string)
! string <-- "Lowercase This String"
! lowercase_string <-- "lowercase this string"
end program example_to_lower
