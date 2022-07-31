program example_free_key
  use stdlib_hashmap_wrappers, only: &
    copy_key, free_key, key_type, set
  use iso_fortran_env, only: int8
  implicit none
  integer(int8) :: i, value(15)
  type(key_type) :: old_key, new_key
  value = [(i, i=1, 15)]
  call set(old_key, value)
  call copy_key(old_key, new_key)
  call free_key(old_key)
end program example_free_key
