program example_or
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0, set1
  call set0%init(166)
  call set1%init(166)
  call or(set0, set1) ! none none
  if (set0%none()) write (*, *) 'First test of OR worked.'
  call set0%not()
  call or(set0, set1) ! all none
  if (set0%all()) write (*, *) 'Second test of OR worked.'
  call set0%not()
  call set1%not()
  call or(set0, set1) ! none all
  if (set0%all()) write (*, *) 'Third test of OR worked.'
  call set0%not()
  call or(set0, set1) ! all all
  if (set0%all()) write (*, *) 'Fourth test of OR worked.'
end program example_or
