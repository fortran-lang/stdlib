program example_test
  use stdlib_bitsets
  type(bitset_large) :: set0
  call set0%init(166)
  call set0%not()
  if (set0%all()) write (*, *) 'SET0 is properly initialized.'
  call set0%clear(165)
  if (.not. set0%test(165)) write (*, *) 'Bit 165 is cleared.'
  call set0%set(165)
  if (set0%test(165)) write (*, *) 'Bit 165 is set.'
end program example_test
