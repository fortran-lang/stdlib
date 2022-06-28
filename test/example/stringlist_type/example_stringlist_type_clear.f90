program example_clear
  use stdlib_stringlist_type, only: stringlist_type, fidx
  implicit none

  type(stringlist_type) :: stringlist

!> inserting 2 elements to the stringlist
  call stringlist%insert_at(fidx(1), "Element No. one")
  call stringlist%insert_at(fidx(1), "Element No. two")
! stringlist <-- {"Element No. two", "Element No. one"}

  call stringlist%clear()
! stringlist <-- { } (empty stringlist)

!> inserting 1 element to the stringlist
  call stringlist%insert_at(fidx(1), "Element No. one")
! stringlist <-- {"Element No. one"}

end program example_clear
