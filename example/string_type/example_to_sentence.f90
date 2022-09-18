program example_to_sentence
  use stdlib_string_type
  implicit none
  type(string_type) :: string, sentencecase_string

  string = "sentencecase this string."
! string <-- "sentencecase this string."

  sentencecase_string = to_sentence(string)
! string <-- "sentencecase this string."
! sentencecase_string <-- "Sentencecase this string."
end program example_to_sentence
