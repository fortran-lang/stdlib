program example_reverse
  use stdlib_ascii, only: reverse
  implicit none
  print'(a)', reverse("Hello, World!") ! returns "!dlroW ,olleH"
end program example_reverse
