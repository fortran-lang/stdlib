program example_reverse
  use stdlib_strings, only: reverse
  implicit none
  print'(a)', reverse("Hello, World!") ! returns "!dlroW ,olleH"
end program example_reverse
