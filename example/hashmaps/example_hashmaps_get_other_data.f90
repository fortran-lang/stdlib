program example_get_other_data
  use stdlib_kinds, only: int8, int64
  use stdlib_hashmaps, only: chaining_hashmap_type, int_index
  use stdlib_hashmap_wrappers, only: key_type, set
  implicit none
  logical                     :: conflict
  type(key_type)              :: key
  type(chaining_hashmap_type) :: map
  type dummy_type
    integer                   :: value(4)
  end type dummy_type
  type(dummy_type) :: dummy
  class(*), allocatable       :: data
  integer(int8), allocatable  :: key_array(:)
  integer                     :: int_scalar
  
  ! Hashmap functions are setup to store scalar value types (other).  Use a dervied
  ! type wrapper to store arrays.
  dummy%value = [4, 3, 2, 1]
  
  ! Explicitly set key type using set function
  call set(key, [0, 1])
  call map%map_entry(key, dummy, conflict)
  if (.not. conflict) then
    call map%get_other_data(key, data)
  else
    error stop 'Key is already present in the map.'
  end if
  
  ! Get_other_data returns data as an unlimited polymorphic scalar.  
  ! To use this type in other operations, there must be a select type operation.
  select type (data)
  type is (dummy_type)
    print *, 'Other data % value = ', data%value
  class default
    print *, 'Invalid data type in other'
  end select
  
! Also can use map_entry and get_other_data generic key interfaces.   
! This is an exmple with integer arrays.  
  call map%map_entry( [2,3], dummy, conflict)
  if (.not. conflict) then
    call map%get_other_data( [2,3], data)
  else
    error stop 'Key is already present in the map.'
  end if
  
  select type (data)
  type is (dummy_type)
    print *, 'Other data % value = ', data%value
  class default
    print *, 'Invalid data type in other'
  end select
  
  ! Integer scalar keys need to be passed as an array.   
  int_scalar = 2
  call map%map_entry( [int_scalar], dummy, conflict)
  if (.not. conflict) then
    call map%get_other_data( [int_scalar], data)
  else
    error stop 'Key is already present in the map.'
  end if
  
  select type (data)
  type is (dummy_type)
    print *, 'Other data % value = ', data%value
  class default
    print *, 'Invalid data type in other'
  end select
  
  ! Example using character type key interface
  call map%map_entry( 'key_string', dummy, conflict)
  if (.not. conflict) then
    call map%get_other_data( 'key_string', data)
  else
    error stop 'Key is already present in the map.'
  end if
  
  select type (data)
  type is (dummy_type)
    print *, 'Other data % value = ', data%value
  class default
    print *, 'Invalid data type in other'
  end select
  
! Transfer to int8 arrays to generate key for unsupported types.  
  key_array = transfer( [0_int64, 1_int64], [0_int8] )
  call map%map_entry( key_array, dummy, conflict)
  if (.not. conflict) then
    call map%get_other_data( key_array, data)
  else
    error stop 'Key is already present in the map.'
  end if
  
  select type (data)
  type is (dummy_type)
    print *, 'Other data % value = ', data%value
  class default
    print *, 'Invalid data type in other'
  end select
  
end program example_get_other_data
