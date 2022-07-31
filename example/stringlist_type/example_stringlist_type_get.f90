program example_get
  use stdlib_stringlist_type, only: stringlist_type, fidx, bidx
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type) :: stringlist
  type(string_type)     :: output

!> inserting 4 elements to the stringlist
  call stringlist%insert_at(fidx(1), "Element No. one")
  call stringlist%insert_at(fidx(1), "Element No. two")
  call stringlist%insert_at(fidx(1), "Element No. three")
  call stringlist%insert_at(fidx(1), "Element No. four")
! stringlist <-- {"Element No. four", "Element No. three", "Element No. two", "Element No. one"}

  output = stringlist%get(fidx(1))
! output <-- "Element No. four"

  output = stringlist%get(bidx(1))
! output <-- "Element No. one"

!> accessing out of bounds index
  output = stringlist%get(bidx(5))
! output <-- ""
  output = stringlist%get(fidx(0))
! output <-- ""

end program example_get
