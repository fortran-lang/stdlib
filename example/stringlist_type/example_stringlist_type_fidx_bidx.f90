program example_fidx_bidx
  use stdlib_stringlist_type, only: stringlist_index_type, fidx, bidx
  implicit none

  type(stringlist_index_type) :: index

  index = fidx(1)
! forward index 1

  index = bidx(3)
! backward index 3

end program example_fidx_bidx
