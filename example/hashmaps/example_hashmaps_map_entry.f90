program example_map_entry
  use, intrinsic:: iso_fortran_env, only: int8, int64
  use stdlib_hashmaps, only: chaining_hashmap_type
  use stdlib_hashmap_wrappers, only: key_type, set
  implicit none
  type(chaining_hashmap_type) :: map
  type(key_type)      :: key
  logical             :: conflict
  integer             :: int_scalar
  
  type :: array_data_wrapper
      integer, allocatable :: array(:)
  end type
  
  type(array_data_wrapper) :: array_example
  
  ! Initialize hashmap with 2^10 slots.
  ! Hashmap will dynamically increase size if needed.
  call map%init(slots_bits=10)
  
  ! Explicitly set key using set function
  call set(key, [1, 2, 3])
  call map%map_entry(key, 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! Using map_entry int32 array interface
  call map%map_entry( [4, 5, 6], 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! Integer scalars need to be passed as an array.
  int_scalar = 1
  call map%map_entry( [int_scalar], 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! Using map_entry character interface
  call map%map_entry( 'key_string', 4, conflict)
  print *, 'CONFLICT = ', conflict
  
  ! Transfer unsupported key types to int8 arrays.
  call map%map_entry( transfer( [1_int64, 2_int64, 3_int64], [0_int8] ), 4, conflict)
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
