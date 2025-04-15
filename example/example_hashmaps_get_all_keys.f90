program example_hashmaps_get_all_keys
  use stdlib_kinds, only: int32
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: get, key_type, set
  implicit none
  type(chaining_hashmap_type) :: map
  type(key_type)   :: key

  type(key_type), allocatable :: keys(:)
  integer(int32) :: i
  
  character(:), allocatable :: str

  ! adding key-value pairs to the map
  call set(key, "initial key")
  call map%map_entry(key, "value 1")

  call set(key, "second key")
  call map%map_entry(key, "value 2")

  call set(key, "last key")
  call map%map_entry(key, "value 3")

  ! getting all the keys in the map
  call map%get_all_keys(keys)

  print '("Number of keys in the hashmap = ", I0)', size(keys)
  !Number of keys in the hashmap = 3

  do i = 1, size(keys)
    call get( keys(i), str )  
    print '("Value of key ", I0, " = ", A)', i, str
  end do
  !Value of key 1 = initial key
  !Value of key 2 = second key
  !Value of key 3 = last key

end program example_hashmaps_get_all_keys
