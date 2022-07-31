program example_assignment
  use stdlib_bitsets
  use stdlib_kinds, only: int8, int32
  implicit none
  logical(int8)  :: logical1(64) = .true.
  logical(int32), allocatable :: logical2(:)
  type(bitset_64) :: set0, set1
  set0 = logical1
  if (set0%bits() /= 64) then
    error stop &
      ' initialization with logical(int8) failed to set'// &
      ' the right size.'
  else if (.not. set0%all()) then
    error stop ' initialization with'// &
      ' logical(int8) failed to set the right values.'
  else
    write (*, *) 'Initialization with logical(int8) succeeded.'
  end if
  set1 = set0
  if (set1 == set0) &
    write (*, *) 'Initialization by assignment succeeded'
  logical2 = set1
  if (all(logical2)) then
    write (*, *) 'Initialization of logical(int32) succeeded.'
  end if
end program example_assignment
