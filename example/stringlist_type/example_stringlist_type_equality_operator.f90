program example_equality_operator
  use stdlib_stringlist_type, only: stringlist_type, fidx, list_head, operator(==)
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)          :: stringlist
  type(string_type), allocatable :: stringarray(:)
  logical                        :: res

!> inserting 4 elements to the stringlist
  call stringlist%insert_at(fidx(1), "#1")
  call stringlist%insert_at(list_head, "#2")
  call stringlist%insert_at(fidx(1), "#3")
  call stringlist%insert_at(list_head, "#4")
! stringlist <-- {"#4", "#3", "#2", "#1"}

!> creating an array of 4 string_type elements
  stringarray = [string_type("#4"), string_type("#3"), string_type("#2"), string_type("#1")]

  res = (stringarray == stringlist)
! res <-- .true.

  res = (stringlist == ["#4", "#3", "#2", "#1"])
! res <-- .true.

  print'(a)', stringlist == ["#4", "#3", "#1"]
! .false.

end program example_equality_operator
