program example_constructor
  use stdlib_stringlist_type, only: stringlist_type
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type) :: stringlist

  stringlist = stringlist_type()
! stringlist <-- { } (empty stringlist)

  stringlist = stringlist_type(["#1", "#2", "#3"])
! stringlist <-- {"#1", "#2", "#3"}

  stringlist = stringlist_type([string_type("#1"), string_type("#2")])
! stringlist <-- {"#1", "#2"}

end program example_constructor
