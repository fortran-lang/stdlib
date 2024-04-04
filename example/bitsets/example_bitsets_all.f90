program example_all
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_all = '111111111111111111111111111111111'
  type(bitset_64) :: set0
  call set0%from_string(bits_all)
  if (.not. set0%all()) then
    error stop "FROM_STRING failed to interpret"// &
      "BITS_ALL's value properly."
  else
    write (*, *) "FROM_STRING transferred BITS_ALL properly"// &
      " into set0."
  end if
end program example_all
