program example_set
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0
  call set0%init(166)
  if (set0%none()) write (*, *) 'SET0 is properly initialized.'
  call set0%set(165)
  if (set0%test(165)) write (*, *) 'Bit 165 is set.'
  call set0%set(0, 164)
  if (set0%all()) write (*, *) 'All bits are set.'
end program example_set
