program example_set_other_data
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, &
                                     fnv_1a_hasher, key_type, other_type, set
  implicit none
  logical :: exists
  type(open_hashmap_type) :: map
  class(*), allocatable   :: data
    
  ! Initialize hashmap with 2^10 slots.
  ! Hashmap will dynamically increase size if needed.
  call map%init(fnv_1_hasher, slots_bits=10)

  call map%map_entry([5, 7, 4, 13], 'A value')
  
  call map%set_other_data([5, 7, 4, 13], 'Another value', exists)
  
  print *, 'The entry to have its other data replaced exists = ', exists
  
  call map%get_other_data( [5, 7, 4, 13], data)
  
  ! Hashmaps return an unlimited polymorphic type as other.  
  ! Must be included in a select type operation to do further operations.
  select type (data)
  type is (character(*))
    print *, 'Value is = ', data
  class default
    print *, 'Invalid data type in other'
  end select
  
end program example_set_other_data
