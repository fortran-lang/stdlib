    program example_hashmaps_get_other_scalar
      use stdlib_hashmap_wrappers, only: &
          get_other_scalar, other_type, set
      use stdlib_kinds, only: int32
      implicit none
      integer(int32) :: value, result
      type(other_type) :: other
      value = 15
      call set( other, value )
      call get_other_scalar( other, result )
      print *, 'RESULT == VALUE = ', ( value == result )
    end program demo_get

