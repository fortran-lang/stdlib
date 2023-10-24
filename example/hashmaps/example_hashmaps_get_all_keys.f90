program example_hashmaps_get_all_keys
  use stdlib_kinds, only: int32
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, &
                                     key_type, other_type, set
  implicit none
  type(chaining_hashmap_type) :: map
  type(key_type)   :: key
  type(other_type) :: other

  type(key_type), allocatable :: keys(:)
  integer(int32) :: i

  call map%init(fnv_1_hasher)

  ! adding key-value pairs to the map
  call set(key, "initial key")
  call set(other, "value 1")
  call map%map_entry(key, other)

  call set(key, "second key")
  call set(other, "value 2")
  call map%map_entry(key, other)

  call set(key, "last key")
  call set(other, "value 3")
  call map%map_entry(key, other)

  ! getting all the keys in the map
  call map%get_all_keys(keys)

  print '("Number of keys in the hashmap = ", I0)', size(keys)
  !Number of keys in the hashmap = 3

  do i = 1, size(keys)
    print '("Value of key ", I0, " = ", A)', i, key_to_char(keys(i))
  end do
  !Value of key 1 = initial key
  !Value of key 2 = second key
  !Value of key 3 = last key

contains
  !Converts key type to character type
  pure function key_to_char(key) result(str)
    type(key_type), intent(in) :: key
    character(:), allocatable :: str
    character(:), allocatable :: str_mold

    allocate( character(len=size(key%value)) :: str_mold )
    str = transfer(key%value, str_mold)
  end function key_to_char
end program example_hashmaps_get_all_keys
