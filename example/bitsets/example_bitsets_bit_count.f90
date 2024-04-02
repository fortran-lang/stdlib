program example_bit_count
  use stdlib_bitsets
  implicit none
  character(*), parameter :: &
    bits_0 = '0000000000000000000'
  type(bitset_64) :: set0
  type(bitset_large) :: set1
  logical, allocatable :: logi(:)
  
  call set0%from_string(bits_0)
  if (set0%bit_count() == 0) then
    write (*, *) "FROM_STRING interpreted "// &
      "BITS_0's value properly."
  end if
  call set0%set(5)
  if (set0%bit_count() == 1) then
    write (*, *) "BIT_COUNT interpreted SET0's value properly."
  end if
  
  allocate( logi(1000), source=.false.)
  logi(1::7) = .true.
  set1 = logi
  if (set1%bit_count() == count(logi)) then
    write (*, *) "BIT_COUNT interpreted SET1's value properly."
  end if
end program example_bit_count
