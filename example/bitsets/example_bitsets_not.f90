program example_not
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0
  call set0%init(155)
  if (set0%none()) then
    write (*, *) "FROM_STRING interpreted "// &
      "BITS_0's value properly."
  end if
  call set0%not()
  if (set0%all()) then
    write (*, *) "ALL interpreted SET0's value properly."
  end if
end program example_not
