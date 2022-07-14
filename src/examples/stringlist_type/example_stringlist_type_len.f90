program example_len
  use stdlib_stringlist_type, only: stringlist_type, bidx
  implicit none

  type(stringlist_type) :: stringlist
  integer               :: output

  output = stringlist%len()
! output <-- 0

!> inserting 2 elements to the stringlist
  call stringlist%insert_at(bidx(1), "Element No. one")
  call stringlist%insert_at(bidx(1), "Element No. two")
! stringlist <-- {"Element No. one", "Element No. two"}

  print'(a)', stringlist%len()
! 2

end program example_len
