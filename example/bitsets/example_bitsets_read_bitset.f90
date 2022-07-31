program example_read_bitset
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_0 = 'S33B000000000000000000000000000000000', &
    bits_1 = 'S33B000000000000000000000000000000001', &
    bits_2 = 'S33B100000000000000000000000000000000'
  character(:), allocatable :: test_0, test_1, test_2
  integer :: unit, status
  type(bitset_64) :: set0, set1, set2, set3, set4, set5
  call set0%read_bitset(bits_0, status)
  call set1%read_bitset(bits_1, status)
  call set2%read_bitset(bits_2, status)
  call set0%write_bitset(test_0, status)
  call set1%write_bitset(test_1, status)
  call set2%write_bitset(test_2, status)
  if (bits_0 == test_0 .and. bits_1 == test_1 .and. &
      bits_2 == test_2) then
    write (*, *) 'READ_BITSET to WRITE_BITSET strings worked.'
  end if
  open (newunit=unit, file='test.txt', status='replace', &
        form='formatted', action='write')
  call set2%write_bitset(unit, advance='no')
  call set1%write_bitset(unit, advance='no')
  call set0%write_bitset(unit)
  close (unit)
  open (newunit=unit, file='test.txt', status='old', &
        form='formatted', action='read')
  call set3%read_bitset(unit, advance='no')
  call set4%read_bitset(unit, advance='no')
  call set5%read_bitset(unit)
  if (set3 == set0 .and. set4 == set1 .and. set5 == set2) then
    write (*, *) 'WRITE_BITSET to READ_BITSET through unit worked.'
  end if
end program example_read_bitset
