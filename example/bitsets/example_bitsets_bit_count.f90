program example_bit_count
  use stdlib_bitsets
  character(*), parameter :: &
    bits_0 = '0000000000000000000'
  type(bitset_64) :: set0
  call set0%from_string(bits_0)
  if (set0%bit_count() == 0) then
    write (*, *) "FROM_STRING interpreted "// &
      "BITS_0's value properly."
  end if
  call set0%set(5)
  if (set0%bit_count() == 1) then
    write (*, *) "BIT_COUNT interpreted SET0's value properly."
  end if
end program example_bit_count
