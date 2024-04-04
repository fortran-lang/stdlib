program example_flip
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0
  call set0%init(166)
  if (set0%none()) write (*, *) 'SET0 is properly initialized.'
  call set0%flip(165)
  if (set0%test(165)) write (*, *) 'Bit 165 is flipped.'
  call set0%flip(0, 164)
  if (set0%all()) write (*, *) 'All bits are flipped.'
end program example_flip
