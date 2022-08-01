program example_copy_other
  use stdlib_hashmap_wrappers, only: &
    copy_other, other_type
  use iso_fortran_env, only: int8
  implicit none
  type(other_type) :: other_in, other_out
  integer(int8) :: i
  type dummy_type
    integer(int8) :: value(15)
  end type
  type(dummy_type) :: dummy_val
  do i = 1, 15
    dummy_val%value(i) = i
  end do
  allocate (other_in%value, source=dummy_val)
  call copy_other(other_in, other_out)
  select type (out => other_out%value)
  type is (dummy_type)
    print *, "other_in == other_out = ", &
      all(dummy_val%value == out%value)
  end select
end program example_copy_other
