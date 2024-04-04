program example_bits
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_0 = '0000000000000000000'
  type(bitset_64) :: set0
  call set0%from_string(bits_0)
  if (set0%bits() == 19) then
    write (*, *) "FROM_STRING interpreted "// &
      "BITS_0's size properly."
  end if
end program example_bits
