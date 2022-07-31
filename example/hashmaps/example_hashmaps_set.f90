program example_set
  use stdlib_hashmap_wrappers, only: &
    get, key_type, set
  use iso_fortran_env, only: int8
  implicit none
  integer(int8), allocatable :: value(:), result(:)
  type(key_type) :: key
  integer(int8) :: i
  allocate (value(1:15))
  do i = 1, 15
    value(i) = i
  end do
  call set(key, value)
  call get(key, result)
  print *, 'RESULT == VALUE = ', all(value == result)
end program example_set
