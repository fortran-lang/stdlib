program example_get_other_data
  use stdlib_kinds, only: int8
  use stdlib_hashmaps, only: chaining_hashmap_type, int_index
  use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type, set, get
  logical                     :: conflict
  type(key_type)              :: key
  type(other_type)            :: other
  type(chaining_hashmap_type) :: map
  type dummy_type
    integer(int8) :: value(4)
  end type dummy_type
  type(dummy_type) :: dummy
  class(*), allocatable :: data
  dummy%value = [4_int8, 3_int8, 2_int8, 1_int8]
  allocate (data, source=dummy)
  call map%init(fnv_1_hasher)
  call set(key, [0_int8, 1_int8, 2_int8, 3_int8, 4_int8])
  call set(other, data)
  call map%map_entry(key, other, conflict)
  if (.not. conflict) then
    call map%get_other_data(key, other)
  else
    error stop 'Key is already present in the map.'
  end if
  call get(other, data)
  select type (data)
  type is (dummy_type)
    print *, 'Other data % value = ', data%value
  class default
    print *, 'Invalid data type in other'
  end select
end program example_get_other_data
