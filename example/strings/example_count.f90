program example_count
  use stdlib_string_type, only: string_type, assignment(=)
  use stdlib_strings, only: count
  implicit none
  type(string_type) :: string

  string = "How much wood would a woodchuck chuck if a woodchuck could chuck wood?"

  print *, count(string, "wood")                                  ! 4
  print *, count(string, ["would", "chuck", "could"])             ! [1, 4, 1]
  print *, count("a long queueueueue", "ueu", [.false., .true.])  ! [2, 4]

end program example_count
