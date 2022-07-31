program example_insert_at
  use stdlib_stringlist_type, only: stringlist_type, stringlist_index_type, fidx, bidx
  use stdlib_string_type, only: string_type
  implicit none

  type(stringlist_type)       :: stringlist
  type(stringlist_index_type) :: index

  index = fidx(1)
  call stringlist%insert_at(index, "Element No. one")
! stringlist <-- {"Element No. one"}

  index = bidx(1)
  call stringlist%insert_at(index, string_type("Element No. two"))
! stringlist <-- {"Element No. one", "Element No. two"}

  call stringlist%insert_at(fidx(2), string_type("Element No. three"))
! stringlist <-- {"Element No. one", "Element No. three", "Element No. two"}

  call stringlist%insert_at(bidx(1), "Element No. four")
! stringlist <-- {"Element No. one", "Element No. three", "Element No. two", "Element No. four"}

end program example_insert_at
