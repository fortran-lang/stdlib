program example_and_not
  use stdlib_bitsets
  implicit none
  type(bitset_large) :: set0, set1
  call set0%init(166)
  call set1%init(166)
  call and_not(set0, set1) ! none none
  if (set0%none()) write (*, *) 'First test of AND_NOT worked.'
  call set0%not()
  call and_not(set0, set1) ! all none
  if (set0%all()) write (*, *) 'Second test of AND_NOT worked.'
  call set0%not()
  call set1%not()
  call and_not(set0, set1) ! none all
  if (set0%none()) write (*, *) 'Third test of AND_NOT worked.'
  call set0%not()
  call and_not(set0, set1) ! all all
  if (set0%none()) write (*, *) 'Fourth test of AND_NOT worked.'
end program example_and_not
