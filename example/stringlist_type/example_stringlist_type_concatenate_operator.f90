program example_concatenate_operator
  use stdlib_stringlist_type, only: stringlist_type, operator(//)
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)          :: first_stringlist, second_stringlist
  type(string_type), allocatable :: stringarray(:)

  first_stringlist = first_stringlist//"Element No. one"
! first_stringlist <-- {"Element No. one"}

  second_stringlist = string_type("Element No. two")//first_stringlist
! second_stringlist <-- {Element No. two, "Element No. one"}

!> Creating an array of 2 string_type elements
  stringarray = [string_type("Element No. three"), string_type("Element No. four")]

  second_stringlist = first_stringlist//stringarray
! second_stringlist <-- {"Element No. one", "Element No. three", "Element No. four"}

  second_stringlist = ["#1", "#2"]//second_stringlist
! second_stringlist <-- {"#1", "#2", "Element No. one", "Element No. three", "Element No. four"}

  first_stringlist = first_stringlist//second_stringlist
! first_stringlist <-- {"Element No. one", "#1", "#2", "Element No. one", "Element No. three", "Element No. four"}

end program example_concatenate_operator
