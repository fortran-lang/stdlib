program example_gt
  use stdlib_bitsets
  implicit none
  type(bitset_64) :: set0, set1, set2
  call set0%init(33)
  call set1%init(33)
  call set2%init(33)
  call set1%set(0)
  call set2%set(32)
  if (set1 > set0 .and. set2 > set1 .and. set2 > set0 .and. &
      .not. set0 > set0 .and. .not. set0 > set1 .and. .not. &
      set1 > set2) then
    write (*, *) 'Passed 64 bit greater than tests.'
  else
    error stop 'Failed 64 bit greater than tests.'
  end if
end program example_gt
