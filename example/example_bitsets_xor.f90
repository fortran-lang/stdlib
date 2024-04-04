program example_xor
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0, set1
  call set0%init(166)
  call set1%init(166)
  call xor(set0, set1) ! none none
  if (set0%none()) write (*, *) 'First test of XOR worked.'
  call set0%not()
  call xor(set0, set1) ! all none
  if (set0%all()) write (*, *) 'Second test of XOR worked.'
  call set0%not()
  call set1%not()
  call xor(set0, set1) ! none all
  if (set0%all()) write (*, *) 'Third test of XOR worked.'
  call set0%not()
  call xor(set0, set1) ! all all
  if (set0%none()) write (*, *) 'Fourth test of XOR worked.'
end program example_xor
