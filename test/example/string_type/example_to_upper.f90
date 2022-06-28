program example_to_upper
  use stdlib_string_type
  implicit none
  type(string_type) :: string, uppercase_string

  string = "Uppercase This String"
! string <-- "Uppercase This String"

  uppercase_string = to_upper(string)
! string <-- "Uppercase This String"
! uppercase_string <-- "UPPERCASE THIS STRING"
end program example_to_upper
