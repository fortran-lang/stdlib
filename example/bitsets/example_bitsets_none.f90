program example_none
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_0 = '0000000000000000000'
  type(bitset_large) :: set0
  call set0%from_string(bits_0)
  if (set0%none()) then
    write (*, *) "FROM_STRING interpreted "// &
      "BITS_0's value properly."
  end if
  call set0%set(5)
  if (.not. set0%none()) then
    write (*, *) "NONE interpreted SET0's value properly."
  end if
end program example_none
