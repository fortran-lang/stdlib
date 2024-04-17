program example_any
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_0 = '0000000000000000000'
  type(bitset_64) :: set0
  call set0%from_string(bits_0)
  if (.not. set0%any()) then
    write (*, *) "FROM_STRING interpreted "// &
      "BITS_0's value properly."
  end if
  call set0%set(5)
  if (set0%any()) then
    write (*, *) "ANY interpreted SET0's value properly."
  end if
end program example_any
