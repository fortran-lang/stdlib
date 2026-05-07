!! The module, STDLIB_HASHMAP_OPEN implements a simple open addressing hash
!! map using linear addressing. The implementation is loosely based on a
!! C implementation by David Chase, http://chasewoerner.org/src/hasht/, for
!! which he has given permission to use in the Fortran Standard Library.

! Note an error in the code caused attempts to deallocate already deallocated
! entries. This did not cause stat to be non-zero, but did cause system errors,
! on my Mac. I therefore decided to remove all deallocation error reporting.

submodule(stdlib_hashmaps) stdlib_hashmap_open

    use, intrinsic :: iso_fortran_env, only: &
        character_storage_size,              &
        error_unit

    use stdlib_hashmap_wrappers

    implicit none

! Error messages
    character(len=*), parameter ::                                             &
        alloc_inv_fault     = "OPEN_HASHMAP_TYPE % INVERSE allocation fault.", &
        alloc_key_fault     = "KEY allocation fault.",                         &
        alloc_slots_fault   = "OPEN_HASHMAP_TYPE % SLOTS allocation fault.",   &
        conflicting_key     = "KEY already exists in MAP.",                    &
        expand_slots_fail   = "OPEN_HASHMAP_TYPE % SLOTS allocation > " //     &
                              "MAX_BITS.",                                     &
        init_slots_pow_fail = "SLOTS_BITS is not between DEFAULT_BITS " //     &
                              "and MAX_BITS.",                                 &
        invalid_inmap       = "INMAP was not a valid INVERSE index.",          &
        map_consist_fault   = "The hash map found an inconsistency."

    character(*), parameter :: submodule_name = 'STDLIB_HASHMAP_OPEN'


    interface expand_slots
!! Version: Experimental
!!
!! Interface to internal procedure that expands an open map's slots.
        module procedure expand_open_slots
    end interface expand_slots

    interface extend_map_entry_pool
!! Version: Experimental
!!
!! Interface to internal procedure that expands an open map entry pool.
        module procedure extend_open_map_entry_pool
    end interface extend_map_entry_pool

    interface free_map
!! Version: Experimental
!!
!! Interface to procedure that finalizes an open hash map.
        module procedure free_open_map
    end interface free_map

    interface free_map_entry_pool
!! Version: Experimental
!!
!! Interface to internal procedure that finalizes an open hash map
!! entry pool.
        module procedure free_map_entry_pool
    end interface free_map_entry_pool

    interface get_other_data
!! Version: Experimental
!!
!! Interface to procedure that gets an entry's other data.
        module procedure get_other_open_data
    end interface get_other_data

    interface  init
!! Version: Experimental
!!
!! Interface to initialization procedure for an open hash map.
        module procedure init_open_map
    end interface init

    interface rehash
!! Version: Experimental
!!
!! Interface to a procedure that changes the hash function that
!! is used to map the keys into an open hash map.
        module procedure rehash_open_map
    end interface rehash

    interface remove
!! Version: Experimental
!!
!! Interface to a procedure that removees an entry from an open hash map.
        module procedure remove_open_entry
    end interface remove

    interface set_other_data
!! Version: Experimental
!!
!! Interface to a procedure that changes the other data associated with a key
        module procedure set_other_open_data
    end interface set_other_data

contains


    subroutine expand_open_slots( map )
!! Version: Experimental
!!
!! Internal routine to make a duplicate map with more hash slots.
!! Doubles the size of the map % slots array
!! Arguments:
!!     map - the hash table whose hash slots are to be expanded
!
        type(open_hashmap_type), intent(inout) :: map

        integer(int_hash)               :: base_slot
        integer(int_index), allocatable :: dummy_slots(:)
        integer(int_index)              :: inv_index,  &
                                           new_size,   &
                                           offset,     &
                                           old_size,   &
                                           test_slot
        integer(int32)                  :: bits,      &
                                           stat

        character(256) :: errmsg
        character(*), parameter :: procedure = 'EXPAND_SLOTS'

        if ( map % nbits == max_bits ) then
            error stop submodule_name // ' % ' // procedure // ': ' // &
                expand_slots_fail
        end if

        old_size = size(map % slots, kind=int_index)

        new_size = 2*old_size
        bits = map % nbits + 1

        allocate( dummy_slots(0:new_size-1), stat=stat, errmsg=errmsg )
        if (stat /= 0) then
            error stop submodule_name // ' % ' // procedure // ': ' // &
                alloc_slots_fault
        end if

        map % nbits = bits

        dummy_slots(:) = 0
        map % index_mask = new_size-1

        map % total_probes = map % total_probes + map % probe_count
        map % probe_count = 0

        REMAP_SLOTS: do inv_index=1_int_index, &
            map % num_entries + map % num_free
            associate( inverse => map % inverse(inv_index) )
              if ( associated(inverse % target) ) then
                  base_slot = fibonacci_hash( inverse % target % hash_val, &
                                              map % nbits )
                  offset = 0
                  FIND_EMPTY_SLOT: do
                      test_slot = iand( int( base_slot + offset, int_hash), &
                                        map % index_mask )
                      if ( dummy_slots(test_slot) == 0 ) then
                          dummy_slots(test_slot) = inv_index
                          exit FIND_EMPTY_SLOT
                      end if
                      offset = offset + 1
                  end do FIND_EMPTY_SLOT
              end if
            end associate
        end do REMAP_SLOTS

        call move_alloc( dummy_slots, map % slots )

    end subroutine expand_open_slots


    subroutine extend_open_map_entry_pool(pool) ! gent_pool_new
!! Version: Experimental
!!
!! Add more map_entrys to the pool head
!! Arguments:
!!     pool - an open map entry pool
        type(open_map_entry_pool), intent(inout), pointer :: pool

        type(open_map_entry_pool), pointer :: map_entry_pool_head

        allocate(map_entry_pool_head)
        allocate(map_entry_pool_head % more_map_entries(0:pool_size-1))
        map_entry_pool_head % lastpool => pool
        pool => map_entry_pool_head
        pool % next = 0

    end subroutine extend_open_map_entry_pool


    recursive subroutine free_map_entry_pool(pool) ! gent_pool_free
!! Version: Experimental
!! Note the freeing of allocated memory may be unnecessary
!!
!! Recursively descends map entry pool list freeing each element
!! Arguments:
!!     pool  The map entry pool whose elements are to be freed
!
        type(open_map_entry_pool), intent(inout), pointer :: pool

        type(open_map_entry_pool), pointer :: lastpool

        if ( associated(pool) ) then
            lastpool => pool % lastpool
            pool % lastpool => null()
            deallocate( pool )
!         Trace component pointers/lists
            call free_map_entry_pool( lastpool )
        end if

    end subroutine free_map_entry_pool


    module subroutine free_open_map( map )
!! Version: Experimental
!!
!! Frees internal memory of an open map
!! Arguments:
!!     map - the open hash map whose memory is to be freed
!
        type(open_hashmap_type), intent(inout) :: map

        type(open_map_entry_list), pointer :: free_list
        integer(int_index) :: i

        if ( allocated( map % slots ) ) then
            deallocate( map % slots )
        end if

        if ( allocated( map % inverse ) ) then
             remove_links: do i=1, size( map % inverse, kind=int_index )
                map % inverse(i) % target => null()
            end do remove_links
            deallocate( map % inverse )
        end if

        free_free_list: do while( map % num_free > 0 )
            free_list => map % free_list
            map % free_list => map % free_list % next
            free_list % next => null()
            free_list % target => null()
            map % num_free = map % num_free - 1
        end do free_free_list
        map % num_free = 0

        if ( associated( map % cache ) ) call free_map_entry_pool(map % cache)

        map % num_entries = 0

    end subroutine free_open_map


    module subroutine get_all_open_keys(map, all_keys)
!! Version: Experimental
!!
!! Returns all the keys contained in a hash map
!! Arguments:
!!     map - an open hash map
!!     all_keys - all the keys contained in a hash map
!
        class(open_hashmap_type), intent(in) :: map
        type(key_type), allocatable, intent(out) :: all_keys(:)
        
        integer(int32) :: num_keys
        integer(int_index) :: i, key_idx

        num_keys = map % entries()
        allocate( all_keys(num_keys) )
        if ( num_keys == 0 ) return

        if ( allocated( map % inverse) ) then
            key_idx = 1_int_index
            do i=1_int_index, size( map % inverse, kind=int_index )
                if ( associated( map % inverse(i) % target ) ) then
                    all_keys(key_idx) = map % inverse(i) % target % key
                    key_idx = key_idx + 1_int_index
                end if
            end do 
        end if

    end subroutine get_all_open_keys


    module subroutine get_other_open_data( map, key, other, exists )
!! Version: Experimental
!!
!! Returns the other data associated with the inverse table index
!! Arguments:
!!     map   - an open hash table
!!     key   - the key associated with a map entry
!!     other - the other data associated with the key
!!     exists - a logical flag indicating whether an entry with that key exists
!
        class(open_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)              :: key
        class(*), allocatable, intent(out)      :: other
        logical, intent(out), optional          :: exists

        integer(int_index) :: inmap
        character(*), parameter :: procedure = 'GET_OTHER_DATA'

        call in_open_map(map, inmap, key)
        if ( inmap <= 0 .or. &
            inmap > map % num_entries + map % num_free ) then
            if ( present(exists) ) then
                exists = .false.
                return
            else
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    invalid_inmap
            end if
        else if ( associated( map % inverse(inmap) % target ) ) then
            if ( present(exists) ) exists = .true.
            other = map % inverse(inmap) % target % other
        else
            if ( present(exists) ) then
                exists = .false.
                return
            else
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    map_consist_fault
            end if
        end if

    end subroutine get_other_open_data


    subroutine in_open_map(map, inmap, key) ! Chase's inmap
!! Version: Experimental
!!
!! Returns the index into the INVERSE array associated with the KEY
!! Arguments:
!!     map   - the hash map of interest
!!     inmap - the returned index into the INVERSE array of entry pointers
!!     key   - the key identifying the entry of interest
!
        class(open_hashmap_type), intent(inout) :: map
        integer(int_index), intent(out)         :: inmap
        type(key_type), intent(in)              :: key

        character(*), parameter :: procedure = 'IN_MAP'
        integer(int_hash) :: &
            base_slot,       &
            hash_val,        &
            test_slot
        integer(int_index) :: &
            offset

        hash_val = map % hasher( key )

        if ( map % probe_count > inmap_probe_factor * map % call_count .or. &
             map % num_entries >= load_factor *                             &
             size( map % slots, kind=int_index ) ) then
            if ( map % nbits < max_bits ) &
                 call expand_slots(map)
        end if

        map % call_count = map % call_count + 1
        base_slot = fibonacci_hash( hash_val, map % nbits )
        offset = 0_int_index
        PROBE_SLOTS: do
            test_slot = iand( base_slot + offset, map % index_mask )
            map % probe_count = map % probe_count + 1
            inmap = map % slots( test_slot )
            if ( inmap == 0 ) then
                return
            else if ( inmap < 0 .or. &
                 inmap > map % num_entries + map % num_free ) then
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    map_consist_fault
            else if ( .not. associated( map % inverse(inmap) % target ) ) then
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    map_consist_fault
            else
                associate( inverse => map % inverse(inmap) )
                  if ( hash_val == inverse % target % hash_val ) then
                      if ( key == inverse % target % key ) then
                          return
                      end if
                  end if
                end associate
            end if
            offset = offset + 1_int_index
        end do PROBE_SLOTS

    end subroutine in_open_map


    module subroutine init_open_map( map,         &
                                     hasher,      &
                                     slots_bits,  &
                                     status )
!! Version: Experimental
!!
!! Routine to allocate an empty map with HASHER as the hash function,
!! 2**SLOTS_BITS initial SIZE(map % slots), and SIZE(map % slots) limited to a
!! maximum of 2**MAX_BITS. All fields are initialized.
!! Arguments:
!!     map         - the open hash maap to be initialized
!!     hasher      - the hash function to be used to map keys to slots
!!     slots_bits  - the number of bits used to map to the slots
!!     status      - an integer error status flag with the allowed values:
!!         success - no problems were found
!!         alloc_fault - map % slots or map % inverse could not be allocated
!!         array_size_error - slots_bits is less than default_bitd or
!!             greater than max_bits

        class(open_hashmap_type), intent(out)      :: map
        procedure(hasher_fun), optional            :: hasher
        integer, intent(in), optional              :: slots_bits
        integer(int32), intent(out), optional      :: status

        character(256)          :: errmsg
        integer(int_index)      :: i
        character(*), parameter :: procedure = 'INIT'
        integer(int_index)      :: slots
        integer(int32)          :: stat
        type(open_map_entry_pool), pointer :: map_entry_pool_head

        map % call_count = 0
        map % probe_count = 0
        map % total_probes = 0
        
        ! Check if user has specified a hasher other than the default hasher.
        if (present(hasher)) map % hasher => hasher        

        if ( present(slots_bits) ) then
            if ( slots_bits < default_bits .OR. &
                 slots_bits > max_bits ) then
                if ( present(status) ) then
                    status = array_size_error
                    return
                else
                    error stop submodule_name // ' % ' // procedure // ': ' // &
                        init_slots_pow_fail
                end if
            end if
            map % nbits = slots_bits
        else
            map % nbits = min( default_bits, max_bits )
        end if

        slots = 2_int32**map % nbits
        map % index_mask = slots - 1

        allocate( map % slots(0:slots-1), stat=stat, errmsg=errmsg )
        if ( stat /= 0 ) then
            if ( present(status) ) then
                status = alloc_fault
                return
            else
                write(error_unit, '(a)') 'Allocation ERRMSG: ' // trim(errmsg)
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    alloc_slots_fault
            end if
        end if

        do i=0, size( map % slots, kind=int_index ) -  1
            map % slots(i) = 0 ! May be redundant
        end do

!! 5*s from Chase's g_new_map
        allocate( map % inverse(1:ceiling(load_factor*slots, &
                  kind=int_index)),                          &
                  stat=stat,                                 &
                  errmsg=errmsg )
        if ( stat /= 0 ) then
            if ( present( status ) ) then
                status = alloc_fault
                return
            else
                write(error_unit, '(a)') 'Allocation ERRMSG: ' // trim(errmsg)
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    alloc_inv_fault
            end if
        end if

        do i=1, size(map % inverse, kind=int_index)
            map % inverse(i) % target => null()
        end do

        do while(associated(map % cache))
            map_entry_pool_head => map % cache
            map % cache => map_entry_pool_head % lastpool
            map_entry_pool_head % lastpool => null()
            deallocate( map_entry_pool_head % more_map_entries )
            deallocate( map_entry_pool_head )
        end do

        call extend_map_entry_pool(map % cache)
        
        map % initialized = .true.

        if (present(status) ) status = success

    end subroutine init_open_map


    pure module function open_loading( map )
!! Version: Experimental
!!
!! Returns the number of entries relative to slots in a hash map
!! Arguments:
!!       map - an open hash map
        class(open_hashmap_type), intent(in) :: map
        real :: open_loading

        open_loading = real( map % num_entries ) / &
                       size( map % slots, kind=int_index )

    end function open_loading


    module subroutine map_open_entry(map, key, other, conflict)
!! Version: Experimental
!!
!! Inserts an entry into the hash table
!!  Arguments:
!!      map     the hash table of interest
!!      key      - the key identifying the entry
!!      other    - other data associated with the key
!!      conflict - logical flag indicating whether the entry key conflicts
!!                 with an existing key
!
        class(open_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)              :: key
        class(*), intent(in), optional          :: other
        logical, intent(out), optional          :: conflict

        type(open_map_entry_type), pointer :: new_ent
        integer(int_hash)  :: base_slot
        integer(int_hash)  :: hash_val
        integer(int_index) :: inmap, offset, test_slot
        character(*), parameter :: procedure = 'MAP_ENTRY'
        
        ! Check that map is initialized.  
        if (.not. map % initialized) call init_open_map( map )
        
        hash_val = map % hasher( key )

        if ( map % probe_count > map_probe_factor * map % call_count .or.   &
             map % num_entries >= load_factor * size( map % slots,          &
                                                      kind=int_index) ) then
            call expand_slots(map)
        end if
        map % call_count = map % call_count  + 1
        base_slot = fibonacci_hash( hash_val, map % nbits )

        offset = 0
        PROBE_SUCCESSIVE_SLOTS: do
            map % probe_count = map % probe_count + 1
            test_slot = iand( base_slot + offset, map % index_mask )
            inmap = map % slots(test_slot)
            if ( inmap == 0 ) then
                call allocate_open_map_entry(map, new_ent)
                new_ent % hash_val = hash_val
                call copy_key( key, new_ent % key )
                if ( present( other ) ) new_ent % other = other
                inmap = new_ent % inmap
                map % inverse( inmap ) % target => new_ent
                map % slots( test_slot ) = inmap
                if ( present(conflict) ) conflict = .false.
                return
            else if ( inmap < 0 .or. &
                inmap > map % num_entries + map % num_free ) then
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    invalid_inmap
            else if (.not. associated( map % inverse(inmap) % target ) ) then
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    invalid_inmap
            else
                associate( target => map % inverse(inmap) % target )
                  if ( hash_val == target % hash_val ) then
                      if ( key == target % key ) then
                          ! entry already exists
                          if ( present(conflict) ) then
                              conflict = .true.
                          else
                              error stop submodule_name // ' % ' // procedure &
                                  // ': ' // conflicting_key
                          end if
                          return
                      end if
                  end if
                end associate
            end if
            offset = offset + 1
        end do PROBE_SUCCESSIVE_SLOTS

    contains

        subroutine allocate_open_map_entry(map, bucket)
!         allocates a hash bucket
            type(open_hashmap_type), intent(inout) :: map
            type(open_map_entry_type), pointer, intent(out) :: bucket
            type(open_map_entry_list), pointer :: free_list
            type(open_map_entry_pool), pointer :: pool
            character(*), parameter :: procedure_name = "ALLOCATE_MAP_ENTRY"

            pool => map % cache
            map % num_entries = map % num_entries + 1
            if ( associated(map % free_list) ) then
!             Get hash bucket from free_list
                free_list => map % free_list
                bucket => free_list % target
                map % free_list => free_list % next
                free_list % target => null()
                free_list % next => null()
                if (bucket % inmap <= 0) &
                    error stop submodule_name // " % " // procedure_name // &
                    ": Failed consistency check: BUCKET % INMAP <= 0"
                map % num_free = map % num_free - 1
            else
!             Get hash bucket from pool
                if ( pool % next == pool_size ) then
!         Expand pool
                    call extend_map_entry_pool(map % cache)
                    pool => map % cache
                end if
                bucket      => pool % more_map_entries(pool % next)
                pool % next =  pool % next + 1 ! 0s based -> post-increment
                if ( map % num_entries >                     &
                     size( map % inverse, kind=int_index ) ) then
                    call expand_inverse( map )
                end if
                if ( map % num_entries <= 0 ) &
                    error stop submodule_name // " % " // procedure_name // &
                    ": Failed consistency check: MAP % NUM_ENTRIES <= 0."
                bucket % inmap = map % num_entries
            end if

        end subroutine allocate_open_map_entry

        subroutine expand_inverse(map)
!!     Increase size of map % inverse
            type(open_hashmap_type), intent(inout) :: map
            type(open_map_entry_ptr), allocatable   :: dummy_inverse(:)

            integer(int32) :: stat
            character(256) :: errmsg

            allocate( dummy_inverse(1:2*size(map % inverse, kind=int_index)), &
                      stat=stat, errmsg=errmsg )
            if ( stat /= 0 ) then
                write(error_unit, '(a)') 'Allocation ERRMSG: ' // trim(errmsg)
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    alloc_inv_fault
            end if
            dummy_inverse(1:size(map % inverse, kind=int_index)) = &
                map % inverse(:)

            call move_alloc( dummy_inverse, map % inverse )

        end subroutine expand_inverse

    end subroutine map_open_entry


    module subroutine rehash_open_map( map, hasher )
!! Version: Experimental
!!
!! Changes the hashing method of the table entries to that of HASHER.
!! Arguments:
!!     map      the table to be rehashed
!!     hasher the hasher function to be used for the table
!
        class(open_hashmap_type), intent(inout) :: map
        procedure(hasher_fun)                   :: hasher

        integer(int_hash)       :: base_slot
        integer(int_hash)       :: hash_val
        integer(int_index)      :: i, test_slot, offset

        map % hasher => hasher

        map % slots = 0

        do i=1, map % num_entries + map % num_free
            if ( .not. associated( map % inverse(i) % target ) ) cycle
            hash_val = map % hasher( map % inverse(i) % target % key )
            map % inverse(i) % target % hash_val = hash_val
            base_slot = fibonaccI_hash( hash_val, map % nbits )
            offset = 0
            FIND_EMPTY_SLOT: do
                test_slot = iand( int( base_slot + offset, int_hash ), &
                                  map % index_mask )
                if ( map % slots(test_slot) == 0 ) then
                    map % slots(test_slot) = i
                    exit FIND_EMPTY_SLOT
                end if
                offset = offset + 1
            end do FIND_EMPTY_SLOT
        end do

    end subroutine rehash_open_map


    module subroutine remove_open_entry(map, key, existed)
!! Remove the entry, if any, that has the key
!! Arguments:
!!    map     - the table from which the entry is to be removed
!!    key     - the key to an entry
!!    existed - a logical flag indicating whether an entry with the key
!!              was present in the original map
!
        class(open_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)              :: key
        logical, intent(out), optional          :: existed

        type(open_map_entry_list), pointer :: aentry
        type(open_map_entry_type), pointer :: bucket
        integer(int_index)                 :: base_slot
        integer(int_index)                 :: current_index
        integer(int_index)                 :: current_slot
        integer(int_index)                 :: empty_slot
        integer(int_index)                 :: inmap
        logical                            :: overlap
        integer(int_index)                 :: slot_index

        overlap = .false.
        call in_open_map( map, inmap, key )
        if ( inmap < 1 .or. inmap > size( map % inverse ) ) then
            if ( present( existed ) ) existed = .false.
            return
        end if

        bucket => map % inverse(inmap) % target
        if ( associated(bucket) ) then
            base_slot = fibonacci_hash( bucket % hash_val, map % nbits )
            if ( present(existed) ) existed = .true.
        else
            if ( present( existed ) ) existed = .false.
            return
        end if

! Find slot associated with inmap and nullify the pointer
        current_slot = base_slot
        search_for_inmap: do
            slot_index = map % slots(current_slot)
            if ( slot_index == inmap ) then
                allocate(aentry)
                aentry % target => map % inverse(inmap) % target
                aentry % next => map % free_list
                map % free_list => aentry
                map % num_free = map % num_free + 1
                map % slots( current_slot ) = 0
                map % inverse(inmap) % target => null()
                map % num_entries = map % num_entries - 1
                empty_slot = current_slot
                current_slot = iand( map % index_mask, current_slot + 1 )
                if ( map % slots(current_slot) == 0 ) return
                if ( current_slot == 0 ) overlap = .true.
                exit search_for_inmap
            else
                if ( map % slots(current_slot) == 0 ) return
                current_slot = iand( map % index_mask, current_slot + 1 )
                if ( current_slot == 0 ) overlap = .true.
                cycle search_for_inmap
            end if
        end do search_for_inmap

! Have found slot and stored it in free_list, now may need to iteratively
! swap to fill holes. First search backwards to find start of run.
        find_run_start: do
            base_slot = iand( map % index_mask, base_slot - 1 )
            if ( base_slot == map % index_mask ) then
                if ( map % slots(base_slot) == 0 ) then
                    base_slot = 0
                    exit find_run_start
                else
                    overlap = .true.
                    cycle find_run_start
                end if
            else if ( map % slots(base_slot) == 0 ) then
                base_slot = iand( map % index_mask, base_slot + 1 )
                exit find_run_start
            else
                cycle find_run_start
            end if
        end do find_run_start

! Search forward for entry to fill empty slot
        fill_empty_slots: do
            bucket => map % inverse(map % slots(current_slot) ) % target
            current_index = fibonacci_hash( bucket % hash_val, &
                                            map % nbits )
            if ( overlap .and. empty_slot < base_slot ) then
                if ( ( current_index >= base_slot .and. &
                       current_index <= map % index_mask ) .or. &
                     ( current_index >= 0 .and. &
                       current_index <= empty_slot ) ) then
                    map % slots( empty_slot ) = map % slots( current_slot )
                    map % slots( current_slot ) = 0
                    empty_slot = current_slot
                end if
                current_slot = iand( map % index_mask, current_slot + 1 )
            else
                if ( current_index >= base_slot .and. &
                     current_index <= empty_slot ) then
                    map % slots( empty_slot ) = map % slots( current_slot )
                    map % slots( current_slot ) = 0
                    empty_slot = current_slot
                end if
                current_slot = iand( map % index_mask, current_slot + 1 )
                if ( current_slot == 0 ) overlap = .true.
            end if
            if ( map % slots( current_slot ) == 0 ) exit fill_empty_slots
        end do fill_empty_slots

    end subroutine remove_open_entry


    module subroutine set_other_open_data( map, key, other, exists )
!! Version: Experimental
!!
!! Change the other data associated with the key
!! Arguments:
!!     map    - the map with the entry of interest
!!     key    - the key to the entry inthe map
!!     other  - the new data to be associated with the key
!!     exists - a logical flag indicating whether the key is already entered
!!              in the map
!
        class(open_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)              :: key
        class(*), intent(in)                    :: other
        logical, intent(out),optional           :: exists

        integer(int_index) :: inmap

        character(*), parameter :: procedure = 'SET_OTHER_DATA'

        call in_open_map( map, inmap, key )
        if ( inmap <= 0 .or. inmap > size( map % inverse, kind=int_index ) ) &
            then
            if ( present(exists) ) then
                exists = .false.
                return
            else
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    invalid_inmap
            end if
        else if ( associated( map % inverse(inmap) % target ) ) then
            map % inverse(inmap) % target % other = other
            if ( present(exists) ) exists = .true.
            return
        else
            error stop submodule_name // ' % ' // procedure // ': ' // &
                invalid_inmap
        end if

    end subroutine set_other_open_data


    module function total_open_depth( map ) result(total_depth)
!! Version: Experimental
!!
!! Returns the total number of ones based offsets of slot entries from
!! their slot index for a hash map
!! Arguments:
!!     map - an open hash map
        class(open_hashmap_type), intent(in) :: map
        integer(int64) :: total_depth

        integer(int_index) :: inv_index, slot, slots
        integer(int_hash)  :: index

        total_depth = 0_int64
        slots = size( map % slots, kind=int_index )
        do slot=0, slots-1
            if ( map % slots( slot ) == 0 ) cycle
            inv_index = map % slots( slot )
            if ( inv_index <= 0 ) cycle
            associate( inverse => map % inverse( inv_index ))
              index = fibonacci_hash( inverse % target % hash_val, &
                                      map % nbits )
            end associate
            total_depth = total_depth + &
                iand( slot - index, map % index_mask ) + 1_int64
        end do

    end function total_open_depth


    module subroutine open_key_test(map, key, present)
!! Version: Experimental
!!
!! Returns a logical flag indicating whether KEY exists in the hash map
!! Arguments:
!!     map - the hash map of interest
!!     key - the key of interest
!
        class(open_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)              :: key
        logical, intent(out)                    :: present

        integer(int_index) :: inmap

        call in_open_map( map, inmap, key )
        if ( inmap <= 0 .or. inmap > size( map % inverse, kind=int_index ) ) &
            then
            present = .false.
        else
            present = associated( map % inverse(inmap) % target )
        end if

    end subroutine open_key_test

end submodule stdlib_hashmap_open
