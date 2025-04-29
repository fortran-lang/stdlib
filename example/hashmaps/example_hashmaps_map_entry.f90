program example_map_entry
  use, intrinsic:: iso_fortran_env, only: int8, int64
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: key_type, set
  implicit none
  type(chaining_hashmap_type) :: map
  type(key_type)      :: key
  logical             :: conflict
  
  type :: array_data_wrapper
      integer, allocatable :: array(:)
  end type
  
  type(array_data_wrapper) :: array_example
  
  integer :: unsupported_key(3,3)
  
  ! Initialize hashmap with 2^10 slots.
  ! Hashmap will dynamically increase size if needed.
  call map%init(slots_bits=10)
  
  ! Explicitly set key using set function
  call set(key, [1, 2, 3])
  call map%map_entry(key, 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! Using the set function is not required.  Can input key into the map_entry key fied.  
  call map%map_entry( [4, 5, 6], 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! Scalars can also be used as keys.
  call map%map_entry( 1, 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! Any type of scalar or rank 1 array can be used as a key.  
  call map%map_entry( 'key_string', 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! A rank 2 or higher array can used as a key by transfering to an int8 array.
  call map%map_entry( transfer( unsupported_key, [0_int8] ), 4, conflict)
  print *, 'CONFLICT = ', conflict
  
! Keys can be mapped alone without a corresponding value (other) for 'Set' type functionality.
  call map%map_entry( [7, 8, 9], conflict=conflict)
  print *, 'CONFLICT = ', conflict
  
! Currently only scalar data can be mapped.   
! Arrays will need a wrapper.  
  array_example % array = [1,2,3,4,5]
  call map % map_entry( [10,11,12], array_example, conflict=conflict )
  print *, 'CONFLICT = ', conflict
  
end program example_map_entry
