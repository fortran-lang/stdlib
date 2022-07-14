program example_inequality_operator
  use stdlib_stringlist_type, only: stringlist_type, bidx, list_tail, operator(/=)
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)          :: stringlist
  type(string_type), allocatable :: stringarray(:)
  logical                        :: res

!> inserting 4 elements to the stringlist
  call stringlist%insert_at(bidx(1), "#1")
  call stringlist%insert_at(list_tail, "#2")
  call stringlist%insert_at(bidx(1), "#3")
  call stringlist%insert_at(list_tail, "#4")
! stringlist <-- {"#1", "#2", "#3", "#4"}

!> creating an array of 4 string_type elements
  stringarray = [string_type("#1"), string_type("#2"), string_type("#3"), string_type("#4")]

  res = (stringarray /= stringlist)
! res <-- .false.

  res = (stringlist /= ["#111", "#222", "#333", "#444"])
! res <-- .true.

  print'(a)', stringlist /= ["#4", "#3", "#1"]
! .true.

end program example_inequality_operator
