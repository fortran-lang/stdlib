program example_copy_key
  use stdlib_hashmap_wrappers, only: &
    copy_key, operator(==), key_type, set
  use iso_fortran_env, only: int8
  implicit none
  integer(int8) :: i, value(15)
  type(key_type) :: old_key, new_key
  value = [(i, i=1, 15)]
  call set(old_key, value)
  call copy_key(old_key, new_key)
  print *, "old_key == new_key = ", old_key == new_key
end program example_copy_key
