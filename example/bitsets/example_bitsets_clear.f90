program example_clear
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0
  call set0%init(166)
  call set0%not()
  if (set0%all()) write (*, *) 'SET0 is properly initialized.'
  call set0%clear(165)
  if (.not. set0%test(165)) write (*, *) 'Bit 165 is cleared.'
  call set0%clear(0, 164)
  if (set0%none()) write (*, *) 'All bits are cleared.'
end program example_clear
