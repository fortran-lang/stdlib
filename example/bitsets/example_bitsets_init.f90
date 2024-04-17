program example_init
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0
  call set0%init(166)
  if (set0%bits() == 166) &
    write (*, *) 'SET0 has the proper size.'
  if (set0%none()) write (*, *) 'SET0 is properly initialized.'
end program example_init
