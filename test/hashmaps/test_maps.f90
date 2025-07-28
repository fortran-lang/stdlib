module test_hashmaps
  use stdlib_hashmaps, only: hashmap_type, chaining_hashmap_type, open_hashmap_type
  use stdlib_hashmap_wrappers, only: set, key_type, hasher_fun, fnv_1_hasher, fnv_1a_hasher, &
      seeded_nmhash32_hasher, seeded_nmhash32x_hasher, seeded_water_hasher
  use stdlib_kinds, only: int8
  use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
 
  implicit none
  
  contains
    
  ! Top level test drive collector
  subroutine collect_hashmap_tests(testsuite)
    type(unittest_type), allocatable, intent(out) :: testsuite(:)
    testsuite = [ &
      new_unittest("default_chaining", test_default_chaining), &
      new_unittest("chaining_fnv1", test_chaining_fnv1), &
      new_unittest("chaining_fnv1a", test_chaining_fnv1a), &
      new_unittest("chaining_snm32", test_chaining_snm32), &
      new_unittest("chaining_snm32x", test_chaining_snm32x), &
      new_unittest("chaining_swh", test_chaining_swh), &
      new_unittest("default_open", test_default_open), &
      new_unittest("open_fnv1", test_open_fnv1), &
      new_unittest("open_fnv1a", test_open_fnv1a), &
      new_unittest("open_snm32", test_open_snm32), &
      new_unittest("open_snm32x", test_open_snm32x), &
      new_unittest("open_swh", test_open_swh) &
    ]
  end subroutine collect_hashmap_tests
  
  !!! Driver routines for the test configs. 
  ! Chaining map tests
  subroutine test_default_chaining(error)
    type(error_type), allocatable, intent(out) :: error
    type(chaining_hashmap_type) :: chain_map
    call run_hashmap_tests(error, chain_map, "default_chaining")
  end subroutine test_default_chaining
  
  subroutine test_chaining_fnv1(error)
    type(error_type), allocatable, intent(out) :: error
    type(chaining_hashmap_type) :: chain_map
    call run_hashmap_tests(error, chain_map, "chaining_fnv1", fnv_1_hasher)
  end subroutine test_chaining_fnv1
  
  subroutine test_chaining_fnv1a(error)
    type(error_type), allocatable, intent(out) :: error
    type(chaining_hashmap_type) :: chain_map
    call run_hashmap_tests(error, chain_map, "chaining_fnv1a", fnv_1a_hasher)
  end subroutine test_chaining_fnv1a
    
  subroutine test_chaining_snm32(error)
    type(error_type), allocatable, intent(out) :: error
    type(chaining_hashmap_type) :: chain_map
    call run_hashmap_tests(error, chain_map, "chaining_snm32", seeded_nmhash32_hasher)
  end subroutine test_chaining_snm32
      
  subroutine test_chaining_snm32x(error)
    type(error_type), allocatable, intent(out) :: error
    type(chaining_hashmap_type) :: chain_map
    call run_hashmap_tests(error, chain_map, "chaining_snm32x", seeded_nmhash32x_hasher)
  end subroutine test_chaining_snm32x
        
  subroutine test_chaining_swh(error)
    type(error_type), allocatable, intent(out) :: error
    type(chaining_hashmap_type) :: chain_map
    call run_hashmap_tests(error, chain_map, "chaining_swh", seeded_water_hasher)
  end subroutine test_chaining_swh
  
  ! Open map tests
  subroutine test_default_open(error)
    type(error_type), allocatable, intent(out) :: error
    type(open_hashmap_type) :: open_map
    call run_hashmap_tests(error, open_map, "default_open" )
  end subroutine test_default_open
  
  subroutine test_open_fnv1(error)
    type(error_type), allocatable, intent(out) :: error
    type(open_hashmap_type) :: open_map
    call run_hashmap_tests(error, open_map, "open_fnv1", fnv_1_hasher)
  end subroutine test_open_fnv1
  
  subroutine test_open_fnv1a(error)
    type(error_type), allocatable, intent(out) :: error
    type(open_hashmap_type) :: open_map
    call run_hashmap_tests(error, open_map, "open_fnv1a", fnv_1a_hasher)
  end subroutine test_open_fnv1a
    
  subroutine test_open_snm32(error)
    type(error_type), allocatable, intent(out) :: error
    type(open_hashmap_type) :: open_map
    call run_hashmap_tests(error, open_map, "open_snm32", seeded_nmhash32_hasher)
  end subroutine test_open_snm32
      
  subroutine test_open_snm32x(error)
    type(error_type), allocatable, intent(out) :: error
    type(open_hashmap_type) :: open_map
    call run_hashmap_tests(error, open_map, "open_snm32x", seeded_nmhash32x_hasher)
  end subroutine test_open_snm32x
        
  subroutine test_open_swh(error)
    type(error_type), allocatable, intent(out) :: error
    type(open_hashmap_type) :: open_map
    call run_hashmap_tests(error, open_map, "open_swh", seeded_water_hasher)
  end subroutine test_open_swh


  ! Common test routine used for all tests.
  subroutine run_hashmap_tests(error, map, name, hasher)
    type(error_type), allocatable, intent(out) :: error
    class(hashmap_type), intent(inout) :: map
    character(len=*), intent(in) :: name
    procedure(hasher_fun), optional :: hasher
    
    integer :: i
    logical :: conflict, exists, existed
    class(*), allocatable :: data
        
    integer, parameter :: test_size = 1000  ! Default size is 2^7 = 128 slots.  1000 is big enough to require several map resizes.
    
    ! Initialize hashmap with the specified hasher if provided.  Otherwise will be default initialization.
    if (present(hasher)) call map%init(hasher=hasher, slots_bits=7)
    
    !! Key interface test
    block
        type(key_type), allocatable :: keys(:)
        type(key_type) :: key
        
        do i = 1, test_size
            ! Map entry
            call set(key, [i])
            call map%map_entry(key, i, conflict)
            call check(error, .not.conflict, "Failure on key interface map_entry for "//trim(name))
      
            ! Verify key exists
            call map % key_test( key, exists )
            call check(error, exists, "Key doesn't exist after mapping on key interface for "//trim(name))
        
            ! Get data and verify it is correct.
            call map % get_other_data( key, data, exists )
            call check(error, exists, "Failure on key interface for get_other_data for"//trim(name))
            select type(data)
                type is (integer)
                    call check(error, data == i, "Failure on key interface data check for"//trim(name))
                class default
                    call test_failed(error, "Key interface get_other_data didn't return an integer for "//trim(name))
            end select
        
            ! Set key to a new value    
            call map % set_other_data( key, (i+test_size), exists )
            call check(error, exists, "Failure on key interface set_other_data for"//trim(name))
        
            ! Get updated value and verify it is correct.
            call map % get_other_data( key, data, exists )
            call check(error, exists, "Failure on key interface for get_other_data after set_other_data for"//trim(name))
            select type(data)
                type is (integer)
                    call check(error, data == (i+test_size), "Failure on key interface set_other_data data check for"//trim(name))
                class default
                    call test_failed(error, "Key interface set_other_data get_other_data didn't return an integer for "//trim(name))
                end select
            
            ! Check entry count and very it matches expected entry count
            call check( error, map % entries() == i, "Failure on key interface add entery count for "//trim(name) )
        end do

        ! Check get all keys routine
        call map%get_all_keys(keys)
        call check(error, size(keys) == test_size, "Failure on key interface for get_all_keys for "//trim(name))
        
        ! Check remove and get all keys function
        do i = 1, test_size
            call set(key, [i])
        
            call map % remove(key, existed)
            call check(error, existed,  "Failure on key interface for remove for "//trim(name))
        
            call map % key_test( key, exists )
            call check(error, .not.exists, "Key exists after removal on key interface for "//trim(name))
        
            call check( error, map % entries() == (test_size-i), "Failure on key interface remove entery count for "//trim(name) )
        enddo
    end block
    
    ! If error encoutered exit test, as downstream tests may be unpredictable.
    if (allocated(error)) return
    
    !!! Check int8 interface
    block
        type(key_type), allocatable :: keys(:)
        integer(int8), allocatable :: key_array(:) 
        
        do i = 1, test_size
            key_array = transfer(i,key_array)
          
            ! Map entry
            call map%map_entry(key_array, i, conflict)
            call check(error, .not.conflict, "Failure on int8 interface map_entry for "//trim(name))
      
            ! Verify key exists
            call map % key_test( key_array, exists )
            call check(error, exists, "Key doesn't exist after mapping on int8 interface for "//trim(name))
       
            ! Get data and verify it is correct.
            call map % get_other_data( key_array, data, exists )
            call check(error, exists, "Failure on int8 interface for get_other_data for"//trim(name))
            select type(data)
              type is (integer)
                call check(error, data == i, "Failure on int8 interface data check for"//trim(name))
              class default
                call test_failed(error, "Int8 interface get_other_data didn't return an integer for "//trim(name))
            end select

            ! Set key to a new value                  
            call map % set_other_data( key_array, (i+test_size), exists )
            call check(error, exists, "Failure on int8 interface set_other_data for"//trim(name))
        
            ! Get updated value and verify it is correct.
            call map % get_other_data( key_array, data, exists )
            call check(error, exists, "Failure on int8 interface for get_other_data after set_other_data for"//trim(name))
            select type(data)
                type is (integer)
                    call check(error, data == (i+test_size), &
                    "Failure on int8 interface set_other_data data check for"//trim(name))
                class default
                    call test_failed(error, &
                    "Int8 interface set_other_data get_other_data didn't return an integer for "//trim(name))
                end select
                
            ! Check entry count and very it matches expected entry count
            call check( error, map % entries() == i, "Failure on int8 interface add entery count for "//trim(name) )
        end do
            
        ! Check get all keys routine
        call map%get_all_keys(keys)
        call check(error, size(keys) == test_size, "Failure on int8 interface for get_all_keys for "//trim(name))
        
        ! Check remove and get all keys function
        do i = 1, test_size
            key_array = transfer(i,key_array)
        
            call map % remove(key_array, existed)
            call check(error, existed,  "Failure on int8 interface for remove for "//trim(name))
      
            call map % key_test(key_array, exists )
            call check(error, .not.exists, "Key exists after removal on int8 interface for "//trim(name))
        
            call check( error, map % entries() == (test_size-i), &
            "Failure on int8 interface remove entery count for "//trim(name) )
        enddo
    end block
    
    ! If error encoutered exit test, as downstream tests may be unpredictable.
    if (allocated(error)) return
    
    !!! Check int32 interface 
    block
        type(key_type), allocatable :: keys(:)
        integer :: key_array(1)
        
        do i = 1, test_size
            key_array = i
            
            ! Map entry
            call map%map_entry(key_array, i, conflict)
            call check(error, .not.conflict, "Failure on int32 interface map_entry for "//trim(name))
      
            ! Verify key exists
            call map % key_test( key_array, exists )
            call check(error, exists, "Key doesn't exist after mapping on int32 interface for "//trim(name))
       
            ! Get data and verify it is correct.
            call map % get_other_data( key_array, data, exists )
            call check(error, exists, "Failure on int32 interface for get_other_data for"//trim(name))
            select type(data)
                type is (integer)
                    call check(error, data == i, "Failure on int32 interface data check for"//trim(name))
                class default
                    call test_failed(error, "Int32 interface get_other_data didn't return an integer for "//trim(name))
                end select
                
            ! Set key to a new value 
            call map % set_other_data( key_array, (i+test_size), exists )
            call check(error, exists, "Failure on int32 interface set_other_data for"//trim(name))
        
            ! Get updated value and verify it is correct.
            call map % get_other_data( key_array, data, exists )
            call check(error, exists, "Failure on int32 interface for get_other_data after set_other_data for"//trim(name))
            select type(data)
                type is (integer)
                    call check(error, data == (i+test_size), &
                    "Failure on int32 interface set_other_data data check for"//trim(name))
                class default
                    call test_failed(error, &
                    "Int32 interface set_other_data get_other_data didn't return an integer for "//trim(name))
            end select
                
            call check( error, map % entries() == i, "Failure on int32 interface add entery count for "//trim(name) )
        end do
        
        ! Check get all keys routine
        call map%get_all_keys(keys)
        call check(error, size(keys) == test_size, "Failure on int32 interface for get_all_keys for "//trim(name))
            
        ! Check remove and get all keys function
        do i = 1, test_size
            key_array = i
              
            call map % remove(key_array, existed)
            call check(error, existed,  "Failure on int32 interface for remove for "//trim(name))
      
            call map % key_test(key_array, exists )
            call check(error, .not.exists, "Key exists after removal on int32 interface for "//trim(name))
        
            call check( error, map % entries() == (test_size-i), "Failure on int32 interface remove entery count for "//trim(name) )
        enddo
    end block
    
    ! If error encoutered exit test, as downstream tests may be unpredictable.
    if (allocated(error)) return
    
    !!! Check character interface
    block
        type(key_type), allocatable :: keys(:)
        character(len=16) :: char_key
      
        do i = 1, test_size
            ! Generate a character string for the key
            write(char_key, '(I0)') i
        
            ! Map entry
            call map%map_entry(char_key, i, conflict)
            call check(error, .not.conflict, "Failure on char interface map_entry for "//trim(name))
      
            ! Verify key exists
            call map % key_test( char_key, exists )
            call check(error, exists, "Key doesn't exist after mapping on char interface for "//trim(name))
       
            ! Get data and verify it is correct.
            call map % get_other_data( char_key, data, exists )
            call check(error, exists, "Failure on char interface for get_other_data for"//trim(name))
        
            select type(data)
                type is (integer)
                    call check(error, data == i, "Failure on char interface data check for"//trim(name))
                class default
                    call test_failed(error, "Char interface get_other_data didn't return an integer for "//trim(name))
            end select
        
            ! Set key to a new value 
            call map % set_other_data( char_key, (i+test_size), exists )
            call check(error, exists, "Failure on char interface set_other_data for"//trim(name))
        
            ! Get updated value and verify it is correct.
            call map % get_other_data( char_key, data, exists )
            call check(error, exists, &
            "Failure on char interface for get_other_data after set_other_data for"//trim(name))
            select type(data)
                type is (integer)
                    call check(error, data == (i+test_size), &
                    "Failure on char interface set_other_data data check for"//trim(name))
                class default
                    call test_failed(error, &
                    "Char interface set_other_data get_other_data didn't return an integer for "//trim(name))
                end select
                
            call check( error, map % entries() == i, "Failure on char interface add entery count for "//trim(name) )
        end do
        
        ! Check get all keys routine
        call map%get_all_keys(keys)
        call check(error, size(keys) == test_size, "Failure on char interface for get_all_keys for "//trim(name))
      
        ! Check remove and get all keys function
        do i = 1, test_size
            write(char_key, '(I0)') i
              
            call map % remove(char_key, existed)
            call check(error, existed,  "Failure on char interface for remove for "//trim(name))
      
            call map % key_test(char_key, exists )
            call check(error, .not.exists, "Key exists after removal on char interface for "//trim(name))
        
            call check( error, map % entries() == (test_size-i), &
            "Failure on char interface remove entery count for "//trim(name) )
        enddo
    end block
    
    ! If error encoutered exit test, as downstream tests may be unpredictable.
    if (allocated(error)) return
    
    ! Final loading.  
    do i = 1, test_size
        call map%map_entry([i], i)
    enddo
    
    ! Test rehash function.
    call map%rehash(fnv_1a_hasher)
    
    ! Loop back through and verify data is correct.
    do i = 1, test_size
        call map % get_other_data( [i], data, exists )
        call check(error, exists, "Failure on get_other_data after rehash for"//trim(name))
        
        select type(data)
            type is (integer)
                call check(error, data == i, "Failure on data check after rehash for"//trim(name))
            class default
                call test_failed(error, "After rehash didn't return an integer for "//trim(name))
            end select
    enddo
    
    ! Check miscellaneous functions calls
    block
        real :: ratio
        integer :: num_slots, nprobes, depth, bits
 
        bits = 0
        bits = map%slots_bits()
        call check(error, bits > 0, "Slots_bits function failure for "//trim(name))
        
        ratio = -1
        ratio = map%loading()
        call check(error, ratio > 0, "Loading function failure for"//trim(name))
    
        num_slots = -1
        num_slots = map%num_slots()
        call check(error, num_slots > 0, "Num_slots function failure for"//trim(name))
        
        nprobes = -1
        nprobes = map%map_probes()
        call check(error, nprobes > 0, "Map_probes failure for"//trim(name))
        
        depth = -1
        depth = map%total_depth()
        call check(error, depth > 0, "Total_depth failure for"//trim(name))
    end block
    
    ! Leaving map with entries to test finalization routine on subroutine exit.
  end subroutine run_hashmap_tests

end module test_hashmaps

    
program main
  use, intrinsic :: iso_fortran_env, only : error_unit
  use testdrive, only: run_testsuite, new_testsuite, testsuite_type
  use test_hashmaps, only: collect_hashmap_tests
  
  implicit none
  
  integer :: stat
  type(testsuite_type), allocatable :: testsuites(:)
  
  testsuites = [ new_testsuite("hashmap_tests", collect_hashmap_tests) ]
  
  stat = 0
  call run_testsuite(testsuites(1)%collect, error_unit, stat)
  
  if (stat > 0) then
    write(error_unit, '(i0, 1x, a)') stat, "Hashmap tests failed!"
    error stop
  end if
  
end program main
