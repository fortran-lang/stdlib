program example_free_other
  use stdlib_hashmap_wrappers, only: &
    copy_other, free_other, other_type
  use iso_fortran_env, only: int8
  implicit none
  type dummy_type
    integer(int8) :: value(15)
  end type dummy_type
  type(dummy_type) :: dummy_val
  type(other_type) :: other_in, other_out
  integer(int8) :: i
  do i = 1, 15
    dummy_val%value(i) = i
  end do
  allocate (other_in%value, source=dummy_val)
  call copy_other(other_in, other_out)
  call free_other(other_out)
end program example_free_other
