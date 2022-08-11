program example_to_title
  use stdlib_ascii, only: to_title
  implicit none
  print *, to_title("hello there!") ! returns "Hello There!"
  print *, to_title("'enquoted'") ! returns "'Enquoted'"
  print *, to_title("1st")  ! returns "1st"
end program example_to_title
