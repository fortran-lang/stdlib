program example_extract
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0, set1
  call set0%init(166)
  call set0%set(100, 150)
  call extract(set1, set0, 100, 150)
  if (set1%bits() == 51) &
    write (*, *) 'SET1 has the proper size.'
  if (set1%all()) write (*, *) 'SET1 has the proper values.'
end program example_extract
