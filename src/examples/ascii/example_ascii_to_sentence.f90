program example_to_sentence
  use stdlib_ascii, only: to_sentence
  implicit none
  print *, to_sentence("hello!") ! returns "Hello!"
  print *, to_sentence("'enquoted'") ! returns "'Enquoted'"
  print *, to_sentence("1st")  ! returns "1st"
end program example_to_sentence
