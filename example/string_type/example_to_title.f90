program example_to_title
  use stdlib_string_type
  implicit none
  type(string_type) :: string, titlecase_string

  string = "titlecase this string."
! string <-- "titlecase this string."

  titlecase_string = to_title(string)
! string <-- "titlecase this string."
! titlecase_string <-- "Titlecase This String."
end program example_to_title
