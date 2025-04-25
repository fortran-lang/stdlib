program example_join
  use stdlib_strings, only: join
  implicit none

  character(len=:), allocatable :: line
  character(*), parameter :: words(3) = [character(7) :: "Hello", "World", "Fortran"]
  
  ! Default separator (space)
  line = join(words)
  print *, "'" // line // "'"   !! 'Hello World Fortran'

  ! Custom separator
  line = join(words, "_")
  print *, "'" // line // "'"  !! 'Hello_World_Fortran'

  ! Custom 2-character separator
  line = join(words, ", ")
  print *, "'" // line // "'"  !! 'Hello, World, Fortran'

end program example_join
