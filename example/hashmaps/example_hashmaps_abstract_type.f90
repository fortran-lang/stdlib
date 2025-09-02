
! For procedure interfaces, consider using abstract hashmap_type for interface definition. 
! This allows the procedure to be used for both chaining and open hashmap types.
    
program example_abstract_type
    use stdlib_hashmaps, only: chaining_hashmap_type, open_hashmap_type, hashmap_type
  
    implicit none
  
    integer                     :: out_value
    type(chaining_hashmap_type) :: chaining_map
    type(open_hashmap_type)     :: open_map
    
    ! Chaining map call
    call put_int(chaining_map, '1', 1)
    call get_int(chaining_map, '1', out_value)
    print *, "Chaining out value is ", out_value
    
    ! Open map call
    call put_int(open_map, '1', 1)
    call get_int(open_map, '1', out_value)
    print *, "Open out value is ", out_value
  
    contains
    
    subroutine put_int(map, key, value)
        class(hashmap_type), intent(inout) :: map
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        
        call map%map_entry(key, value)
    end subroutine put_int
    
    
    subroutine get_int(map, key, value)
        class(hashmap_type), intent(inout) :: map
        character(len=*), intent(in) :: key
        integer, intent(out) :: value
        class(*), allocatable :: data
        
        call map%get_other_data( key, data)
  
        select type (data)
        type is (integer)
            value = data
        class default
            print *, 'Invalid data type in other'
        end select
    end subroutine get_int

        
end program example_abstract_type
