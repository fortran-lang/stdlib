program example_input
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_0 = '000000000000000000000000000000000', &
    bits_1 = '000000000000000000000000000000001', &
    bits_33 = '100000000000000000000000000000000'
  integer :: unit
  type(bitset_64) :: set0, set1, set2, set3, set4, set5
  call set0%from_string(bits_0)
  call set1%from_string(bits_1)
  call set2%from_string(bits_33)
  open (newunit=unit, file='test.bin', status='replace', &
        form='unformatted', action='write')
  call set2%output(unit)
  call set1%output(unit)
  call set0%output(unit)
  close (unit)
  open (newunit=unit, file='test.bin', status='old', &
        form='unformatted', action='read')
  call set5%input(unit)
  call set4%input(unit)
  call set3%input(unit)
  close (unit)
  if (set3 /= set0 .or. set4 /= set1 .or. set5 /= set2) then
    error stop 'Transfer to and from units using '// &
      ' output and input failed.'
  else
    write (*, *) 'Transfer to and from units using '// &
      'output and input succeeded.'
  end if
end program example_input
