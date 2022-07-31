program example_reverse
  use stdlib_string_type
  implicit none
  type(string_type) :: string, reverse_string

  string = "Reverse This String"
! string <-- "Reverse This String"

  reverse_string = reverse(string)
! string <-- "Reverse This String"
! reverse_string <-- "gnirtS sihT esreveR"
end program example_reverse
