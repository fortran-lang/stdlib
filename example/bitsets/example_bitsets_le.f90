program example_le
  use stdlib_bitsets
  implicit none
  type(bitset_64) :: set0, set1, set2
  call set0%init(33)
  call set1%init(33)
  call set2%init(33)
  call set1%set(0)
  call set2%set(32)
  if (set0 <= set1 .and. set1 <= set2 .and. set0 <= set2 .and. &
      set0 <= set0 .and. set1 <= set1 .and. set2 <= set2 .and. &
      .not. set1 <= set0 .and. .not. set2 <= set0 .and. .not. &
      set2 <= set1) then
    write (*, *) 'Passed 64 bit less than or equal tests.'
  else
    error stop 'Failed 64 bit less than or equal tests.'
  end if
end program example_le
