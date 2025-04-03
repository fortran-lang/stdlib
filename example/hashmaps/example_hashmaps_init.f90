program example_init
  use stdlib_hashmaps, only: chaining_hashmap_type, open_hashmap_type
  use stdlib_hashmap_wrappers, only: fnv_1_hasher
  implicit none
  type(chaining_hashmap_type) :: map
  logical :: present
  
  
  !If default values are used, then init can be typically be skipped as the first map_entry call will initialize the map using default values.  
  call map%map_entry('key', 'value')
  call map%key_test('key', present)
  print *, "Key exists without explicit init call =  ", present
  
  ! Init can be called to clear all items in a map. 
  call map%init()
  call map%key_test('key', present)
  print *, "Key exists after re-initalization =  ", present
  
  ! User can optional specify hasher type and slots_bits instead of using default values.  
  ! Number of slots in the hashmap will initially equal 2**slots_bits.   
  ! The hashmap will automatically re-size as needed; however for better performance, a rule of thumb is to size so that number of slots is ~2X expected number of entries.
  ! In this example with slots_bits=10, there will initially be 1024 slots in the map.  
  call map%init(hasher=fnv_1_hasher, slots_bits=10)
  call map%map_entry('key', 'value')
  
end program example_init
