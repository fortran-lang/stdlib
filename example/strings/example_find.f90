program example_find
  use stdlib_string_type, only: string_type, assignment(=)
  use stdlib_strings, only: find
  implicit none
  type(string_type) :: string

  string = "needle in the character-stack"

  print *, find(string, "needle")                       ! 1
  print *, find(string, ["a", "c"], [3, 2])             ! [27, 20]
  print *, find("qwqwqwq", "qwq", 3, [.false., .true.]) ! [0, 5]

end program example_find
