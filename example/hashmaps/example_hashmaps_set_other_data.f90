program example_set_other_data
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: open_hashmap_type, chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: key_type, set
  
  implicit none
  logical :: exists
  type(chaining_hashmap_type) :: map
  class(*), allocatable   :: data
  type(key_type) :: key
    
  ! Initialize hashmap with 2^10 slots.
  ! Hashmap will dynamically increase size if needed.
  call map%init(slots_bits=10)

  call set(key, [5, 7, 4, 13])
  
  call map%map_entry(key, 'A value')
  
  call map%set_other_data(key, 'Another value', exists)
  
  print *, 'The entry to have its other data replaced exists = ', exists
  
  call map%get_other_data(key, data, exists)
  
  print *, 'Get_other_data was successful = ', exists
  
  ! Hashmaps return an unlimited polymorphic type as other.  
  ! Must be included in a select type operation to do further operations.
  select type (data)
  type is (character(*))
    print *, 'Value is = ', data
  class default
    print *, 'Invalid data type in other'
  end select
  
end program example_set_other_data
