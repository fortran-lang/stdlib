program example_to_string
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_all = '111111111111111111111111111111111'
  type(bitset_64) :: set0
  character(:), allocatable :: new_string
  call set0%init(33)
  call set0%not()
  call set0%to_string(new_string)
  if (new_string == bits_all) then
    write (*, *) "TO_STRING transferred BITS0 properly"// &
      " into NEW_STRING."
  end if
end program example_to_string
