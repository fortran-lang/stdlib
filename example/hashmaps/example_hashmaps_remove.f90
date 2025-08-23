program example_remove
  use stdlib_kinds, only: int8, int64
  use stdlib_hashmaps, only: open_hashmap_type
  use stdlib_hashmap_wrappers, only: key_type, set
  implicit none
  type(open_hashmap_type) :: map
  type(key_type)      :: key
  logical             :: existed
  integer             :: int_scalar
  
  ! Initialize hashmap with 2^10 slots.
  ! Hashmap will dynamically increase size if needed.
  call map%init(slots_bits=10)
  ! Explicitly set key type using set function
  call set(key, [1, 2, 3])
  call map%map_entry(key, 4.0)
  call map%remove(key, existed)
  print *, "Removed key existed = ", existed
  
  ! Using map_entry and remove int32 generic interface.
  call map%map_entry([1, 2, 3], 4.0)
  call map%remove([1, 2, 3], existed)
  print *, "Removed key existed = ", existed
  
  ! Integer scalars need to be passed as an array.
  int_scalar = 1
  call map%map_entry( [int_scalar], 4.0)
  call map%remove( [int_scalar], existed)
  print *, "Removed key existed = ", existed
  
  ! Using map_entry and remove character generic interface.
  call map%map_entry('key_string', 4.0)
  call map%remove('key_string', existed)
  print *, "Removed key existed = ", existed
  
  ! Use transfer to int8 arrays for unsupported key types.
  call map%map_entry( transfer( [1_int64, 2_int64], [0_int8] ), 4.0)
  call map%remove( transfer( [1_int64,2_int64], [0_int8] ), existed)
  print *, "Removed key existed = ", existed
end program example_remove
