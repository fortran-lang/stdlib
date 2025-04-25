!! The module STDLIB_HASHMAP_CHAINING implements a simple separate
!! chaining hash map. The implementation is loosely based on a C
!! implementation by David Chase, http://chasewoerner.org/src/hasht/, for
!! which he has given permission to use in the Fortran Standard Library.

! Note an error in the code caused attempts to deallocate already deallocated
! entries. This did not cause stat to be non-zero, but did cause system errors,
! on my Mac. I therefore decided to remove all deallocation error reporting.

submodule(stdlib_hashmaps) stdlib_hashmap_chaining
!! Version: Experimental
!!
!! Implements a simple separate chaining hash map.

    implicit none

! Error messages
    character(len=*), parameter ::                                            &
        alloc_inv_fault    = "CHAINING_HASHMAP_TYPE % INVERSE allocation " // &
                             "fault.",                                        &
        alloc_slots_fault  = "CHAINING_HASHMAP_TYPE % SLOTS allocation " //   &
                             "fault.",                                        &
        conflicting_key    = "KEY already exists in MAP.",                    &
        expand_slots_fail  = "CHAINING_HASHMAP_TYPE % SLOTS allocation > " // &
                             "max bits.",                                     &
        init_slots_pow_fail = "SLOT_BITS is not between DEFAULT_BITS " //     &
                              "and MAX_BITS.",                                &
        invalid_inmap      = "INMAP was not a valid INVERSE index.",          &
        map_consist_fault  = "The hash map found a inconsistency."

    character(len=*), parameter :: submodule_name = "STDLIB_HASHMAP_CHAINING"

    interface expand_slots
!! Version: Experimental
!!
!! Interface to internal procedure that expands the number of map slots.
        module procedure expand_chaining_slots
    end interface expand_slots

    interface extend_map_entry_pool
!! Version: Experimental
!!
!! Interface to internal procedure that expands a chaining map entry pool.
        module procedure extend_chaining_map_entry_pool
    end interface extend_map_entry_pool

    interface free_map
!! Version: Experimental
!!
!! Interface to procedure that finalizes a chaining hash map.
        module procedure free_chaining_map
    end interface free_map

    interface free_map_entry_pool
!! Version: Experimental
!!
!! Interface to internal procedure that finalizes a chaining hash map
!! entry pool.
        module procedure free_map_entry_pool
    end interface free_map_entry_pool

    interface get_other_data
!! Version: Experimental
!!
!! Interface to procedure that gets an entry's other data.
        module procedure get_other_chaining_data
    end interface get_other_data

    interface init
!! Version: Experimental
!!
!! Interface to initialization procedure for a chaining hash map.
        module procedure init_chaining_map
    end interface init

    interface rehash
!! Version: Experimental
!!
!! Interface to a procedure that changes the hash function that
!! is used to map the keys into a chaining hash map.
        module procedure rehash_chaining_map
    end interface rehash

    interface remove
!! Version: Experimental
!!
!! Interface to a procedure that removes the entry associated with a key
        module procedure remove_chaining_entry ! Chase's delent
    end interface remove

    interface set_other_data
!! Version: Experimental
!!
!! Interface to a procedure that changes the other data associated with a key
        module procedure set_other_chaining_data
    end interface set_other_data

contains

!  Internal routine to make a duplicate map with more hash slots.
!  Note David Chase had pointer returning functions, but the logic did not
!  depend on the result value
    subroutine expand_chaining_slots( map )
!! Version: Experimental
!!
!! Internal routine to make a duplicate map with more hash slots.
!! Doubles the size of the map % slots array
!! Arguments:
!!     map - the hash map whose hash slots are to be expanded
!
        type(chaining_hashmap_type), intent(inout) :: map

        type(chaining_map_entry_type), pointer    :: current_entry
        type(chaining_map_entry_ptr), allocatable :: dummy_slots(:)
        integer(int_index)                        :: min_size, new_size
        integer(int_index)                        :: old_size, &
                                                     slot_index
        integer(int32)                            :: bits, &
                                                     stat
        character(256) :: errmsg
        character(*), parameter :: procedure = 'EXPAND_SLOTS'

        if ( map % nbits == max_bits ) then
            error stop submodule_name // ' % ' // procedure // ': ' // &
                expand_slots_fail
        end if

        old_size = size(map % slots, kind=int_index)

       determine_new_size: if ( map % num_entries <= old_size ) then
! Expand by factor of two to improve efficiency
            new_size = 2*old_size
            bits = map % nbits + 1
        else
! Expand so the number of slots is no more than 2**max_bits but otherwise
! at least the number of entries
            min_size = map % num_entries
            new_size = old_size
            bits = map % nbits
            do
                bits = bits + 1
                new_size = new_size * 2
                if ( bits >= max_bits .OR. new_size >= min_size ) exit
            end do
        end if determine_new_size

        allocate( dummy_slots(0:new_size-1), stat=stat, errmsg=errmsg )
        if (stat /= 0) then
            write(error_unit, '(a)') 'Allocation ERRMSG: ' // trim(errmsg)
            error stop submodule_name // ' % ' // procedure // ': ' // &
                alloc_slots_fault
        end if

        map % nbits = bits
        do slot_index=0, new_size-1
            dummy_slots(slot_index) % target => null() ! May be redundant
        end do

        map % total_probes = map % total_probes + map % probe_count
        map % probe_count = 0

! This maps old slots entries to new slots, but we could also map inverse
! entries to new_slots
        do slot_index=0, old_size-1
            do while( associated(map % slots(slot_index) % target) )
                current_entry => map % slots(slot_index) % target
                map % slots(slot_index) % target => current_entry % next
                call remap( dummy_slots, current_entry, map % nbits )
            end do
        end do

        call move_alloc( dummy_slots, map % slots )

    contains

        subroutine remap(slots, gentry, bits)
            type(chaining_map_entry_ptr), intent(inout)          :: slots(0:)
            type(chaining_map_entry_type), intent(inout), target :: gentry
            integer(int_hash), intent(in)                        :: bits

            integer(int_index)                     :: hash_index
            type(chaining_map_entry_type), pointer :: where_loc

            hash_index = fibonacci_hash( gentry % hash_val, bits )
            where_loc => slots(hash_index) % target
            gentry % next => null() ! May be redundant

            if ( associated( where_loc ) ) then
                do while ( associated(where_loc % next) )
                    where_loc => where_loc % next
                end do
                where_loc % next => gentry
            else
                slots(hash_index) % target => gentry
            end if

        end subroutine remap

    end subroutine expand_chaining_slots


    subroutine extend_chaining_map_entry_pool(map) ! gent_pool_new
!! Version: Experimental
!!
!! Add more map_entrys to the pool head
!! Arguments:
!!     pool - a chaining map entry pool
        type(chaining_hashmap_type), intent(inout) :: map

        type(chaining_map_entry_pool), pointer :: pool

        allocate(pool)
        allocate(pool % more_map_entries(0:pool_size-1))
        pool % next = 0 ! may be redundant
        pool % lastpool => map % cache
        map % cache => pool

    end subroutine extend_chaining_map_entry_pool


!  Internal final routine to free a map and its memory
    module subroutine free_chaining_map( map )
!! Version: Experimental
!!
!! Frees internal memory of an chaining map
!! Arguments:
!!     map - the chaining hash map whose memory is to be freed
!
        type(chaining_hashmap_type), intent(inout) :: map

        integer(int_index) :: i
        type(chaining_map_entry_type), pointer :: next

        if ( allocated( map % slots ) ) then
            remove_slot_links: do i=0, size( map % slots ) - 1
                if ( associated( map % slots(i) % target ) ) then
                    map % slots(i) % target => null()
                end if
            end do remove_slot_links
            deallocate( map % slots )
        end if

        if ( allocated( map % inverse) ) then
            remove_links: do i=1, size( map % inverse, kind=int_index )
                if ( associated( map % inverse(i) % target ) ) then
                    map % inverse(i) % target % next => null()
                end if
                map % inverse(i) % target => null()
            end do remove_links
            deallocate( map % inverse )
        end if

        free_free_list: do
            if ( associated( map % free_list) ) then
                next => map % free_list % next
                map % free_list => next
                cycle free_free_list
            else
                map % num_free = 0
                exit free_free_list
            end if
        end do free_free_list

        if ( associated( map % cache ) ) call free_map_entry_pool(map % cache)

        map % num_entries = 0

    end subroutine free_chaining_map


    recursive subroutine free_map_entry_pool(pool) ! gent_pool_free
!! Version: Experimental
!!
!! Recursively descends map entry pool list freeing each element
!! Arguments:
!!     pool  The map entry pool whose elements are to be freed
!
        type(chaining_map_entry_pool), intent(inout), pointer :: pool

        if ( .not.  associated(pool) ) return
        call free_map_entry_pool(pool % lastpool)
        deallocate( pool )

    end subroutine free_map_entry_pool


    module subroutine get_all_chaining_keys(map, all_keys)
!! Version: Experimental
!!
!! Returns all the keys contained in a hash map
!! Arguments:
!!     map - a chaining hash map
!!     all_keys - all the keys contained in a hash map
!
        class(chaining_hashmap_type), intent(in) :: map
        type(key_type), allocatable, intent(out) :: all_keys(:)
        
        integer(int32) :: num_keys
        integer(int_index) :: i, key_idx

        num_keys = map % entries()
        allocate( all_keys(num_keys) )
        if ( num_keys == 0 ) return

        if( allocated( map % inverse ) ) then
            key_idx = 1_int_index
            do i=1_int_index, size( map % inverse, kind=int_index )
                if ( associated( map % inverse(i) % target ) ) then
                    all_keys(key_idx) = map % inverse(i) % target % key
                    key_idx = key_idx + 1_int_index
                end if
            end do 
        end if

    end subroutine get_all_chaining_keys


    module subroutine get_other_chaining_data( map, key, other, exists )
!! Version: Experimental
!!
!! Returns the other data associated with the inverse table index
!! Arguments:
!!     map    - a chaining hash map
!!     key    - the key associated with a map entry
!!     other  - the other data associated with the key
!!     exists - a logical flag indicating whether an entry with that key exists
!
        class(chaining_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)                  :: key
        class(*), allocatable, intent(out)          :: other
        logical, intent(out), optional              :: exists

        integer(int_index) :: inmap
        character(*), parameter :: procedure = 'GET_OTHER_DATA'

        call in_chain_map(map, inmap, key)
        if ( inmap <= 0 .or. &
             inmap > size(map % inverse, kind=int_index ) ) then
            if ( present(exists) ) then
                exists = .false.
                return
            else
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    invalid_inmap
            end if
        else if ( associated( map % inverse(inmap) % target ) ) then
            if (present(exists) ) exists = .true.
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

    end subroutine get_other_chaining_data


    subroutine in_chain_map(map, inmap, key)
!! Version: Experimental
!!
!! Returns the index into the INVERSE array associated with the KEY
!! Arguments:
!!     map   - the hash map of interest
!!     inmap - the returned index into the INVERSE array of entry pointers.
!!             A value of zero indicates that an entry with that key was not
!!             found.
!!     key   - the key identifying the entry of interest
!
        class(chaining_hashmap_type), intent(inout) :: map
        integer(int_index), intent(out)             :: inmap
        type(key_type), intent(in)                  :: key

        integer(int_hash)                      :: hash_val, hash_index
        type(chaining_map_entry_type), pointer :: gentry, pentry, sentry

        if ( map % probe_count > inmap_probe_factor * map % call_count ) then
            if ( map % nbits < max_bits .AND. &
                 map % num_entries > size( map % slots, kind=int_index ) ) then
                call expand_slots(map)
            end if
        end if
        map % call_count = map % call_count + 1
        hash_val = map % hasher( key )
        hash_index = fibonacci_hash( hash_val, map % nbits )
        pentry => map % slots(hash_index) % target
        sentry => pentry

        climb_chain: do
            gentry => pentry
            map % probe_count = map % probe_count + 1
            if (.not. associated( gentry ) ) then
                inmap = 0
                return
            else if ( hash_val == gentry % hash_val ) then
                if ( key == gentry % key ) then
! The swap to front seems to confuse gfortran's pointers
!                    if ( .not. associated( pentry, sentry ) ) then
!                    ! swap to front
!                        pentry => gentry % next
!                        gentry % next => sentry
!                        sentry => gentry
!                    end if
                    inmap = gentry % inmap
                    return
                end if
            end if
            pentry => gentry % next
        end do climb_chain

    end subroutine in_chain_map


    module subroutine init_chaining_map( map,        &
                                         hasher,     &
                                         slots_bits, &
                                         status )
!! Version: Experimental
!!
!! Routine to allocate an empty map with HASHER as the hash function,
!! 2**SLOTS_BITS initial SIZE(map % slots), and SIZE(map % slots) limited
!! to a maximum of 2**MAX_BITS. All fields are initialized.
!! Arguments:
!!     map         - the chaining hash map to be initialized
!!     hasher      - the hash function to be used to map keys to slots
!!     slots_bits - the bits of two used to initialize the number of slots
!!     status      - an integer error status flag with the allowed values:
!!         success - no problems were found
!!         alloc_fault - map % slots or map % inverse could not be allocated
!!         array_size_error - slots_bits is less than default_bits or
!!             greater than max_bits
!
        class(chaining_hashmap_type), intent(out)  :: map
        procedure(hasher_fun), optional            :: hasher
        integer, intent(in), optional              :: slots_bits
        integer(int32), intent(out), optional      :: status

        character(256)          :: errmsg
        integer(int_index)      :: index
        character(*), parameter :: procedure = 'INIT'
        integer(int_index)      :: slots
        integer(int32)          :: stat

        map % call_count = 0
        map % probe_count = 0
        map % total_probes = 0

        ! Check if user has specified a hasher other than the default hasher.
        if (present(hasher)) map % hasher => hasher      
            
        call free_chaining_map( map )

        if ( present(slots_bits) ) then
            if ( slots_bits < 6 .OR. slots_bits > max_bits ) then
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

        slots = 2_int_index**map % nbits

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
        do index = 0, size( map % slots, kind=int_index )-1
            map % slots(index) % target => null() ! May be redundant
        end do

! 5*s from Chase's g_new_map
        allocate( map % inverse(1:slots), stat=stat, errmsg=errmsg )
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
        do index=1, size(map % inverse, kind=int_index)
            map % inverse(index) % target => null()
        end do

        call extend_map_entry_pool(map)

        map % initialized = .true.
        
        if (present(status) ) status = success

    end subroutine init_chaining_map


    pure module function chaining_loading( map )
!! Version: Experimental
!!
!! Returns the number of entries relative to slots in a hash map
!! Arguments:
!!      map - a chaining hash map
        class(chaining_hashmap_type), intent(in) :: map
        real :: chaining_loading

        chaining_loading = real( map % num_entries ) / &
                           real( size( map % slots, kind=int_index ) )

    end function chaining_loading


    module subroutine map_chain_entry(map, key, other, conflict)
!! Version: Experimental
!!
!! Inserts an entry into the hash table
!! Arguments:
!!     map      - the hash table of interest
!!     key      - the key identifying the entry
!!     other    - other data associated with the key
!!     conflict - logical flag indicating whether the entry key conflicts
!!                 with an existing key
!
        class(chaining_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)                  :: key
        class(*), intent(in), optional              :: other
        logical, intent(out), optional              :: conflict

        integer(int_hash)                      :: hash_index
        integer(int_hash)                      :: hash_val
        integer(int_index)                     :: inmap
        type(chaining_map_entry_type), pointer :: new_ent
        type(chaining_map_entry_type), pointer :: gentry, pentry, sentry
        character(*), parameter :: procedure = 'MAP_ENTRY'

        ! Check that map is initialized.  
        if (.not. map % initialized) call init_chaining_map( map )
        
        hash_val = map % hasher( key )

        if ( map % probe_count > map_probe_factor * map % call_count ) then
            call expand_slots(map)
        end if
        map % call_count = map % call_count + 1
        hash_index = fibonacci_hash( hash_val, map % nbits )
        pentry => map % slots(hash_index) % target
        sentry => pentry

        do
            gentry => pentry
            map % probe_count = map % probe_count + 1
            if ( .not. associated( gentry ) ) then
                call allocate_chaining_map_entry( map, new_ent )
                new_ent % hash_val = hash_val
! Adding to tail of chain doesn't work on gfortran
!                new_ent % next => sentry
!                sentry => new_ent
! Adding to head of chain works on gfortran
                new_ent % next => map % slots(hash_index) % target
                map % slots(hash_index) % target => new_ent
                call copy_key( key, new_ent % key )
                if ( present(other) ) new_ent % other = other
                if ( new_ent % inmap == 0 ) then
                    map % num_entries = map % num_entries + 1
                    inmap = map % num_entries
                else
                    inmap = new_ent % inmap
                end if

                if ( inmap == size( map % inverse, kind=int_index ) ) then
                    call expand_inverse( map )
                end if
                new_ent % inmap = inmap
                map % inverse(inmap) % target => new_ent
                if ( present(conflict) ) conflict = .false.

                return

            else if ( hash_val == gentry % hash_val ) then
                if ( key == gentry % key ) then
                    inmap = gentry % inmap
                    if ( .not. associated( pentry, sentry ) ) then
                        ! Swap to front
                        pentry => gentry % next
                        gentry % next => sentry
                        sentry => gentry
                    end if
                    if ( present(conflict) ) then
                        conflict = .true.
                    else
                        error stop submodule_name // ' % ' // procedure &
                                  // ': ' // conflicting_key
                    end if
                    return
                end if
            end if
            pentry => gentry % next

        end do

    contains

        subroutine allocate_chaining_map_entry(map, bucket) ! Chases gent_malloc
!         allocates a hash bucket
            type(chaining_hashmap_type), intent(inout)         :: map
            type(chaining_map_entry_type), pointer, intent(out) :: bucket

            type(chaining_map_entry_pool), pointer :: pool

            pool => map % cache
            map % num_entries = map % num_entries + 1
            if ( associated(map % free_list) ) then
!             Get hash bucket from free_list
                bucket         => map % free_list
                map % free_list => bucket % next
                map % num_free = map % num_free - 1
            else
!             Get hash bucket from pool
                if ( pool % next == pool_size ) then
!                 Expand pool
                    call extend_map_entry_pool(map)
                    pool => map % cache
                end if
                bucket      => pool % more_map_entries(pool % next)
                pool % next =  pool % next + 1 ! 0s based
                if ( map % num_entries > &
                     size( map % inverse, kind=int_index ) ) &
                    then
                    call expand_inverse( map )
                end if
                bucket % inmap = map % num_entries
            end if

        end subroutine allocate_chaining_map_entry


        subroutine expand_inverse(map)
!         Increase size of map % inverse
            type(chaining_hashmap_type), intent(inout) :: map
            type(chaining_map_entry_ptr), allocatable  :: dummy_inverse(:)
            integer(int32) :: stat
            character(256) :: errmsg
            character(*), parameter :: procedure = 'MAP_ENTRY'

            allocate( dummy_inverse( 1:2*size(map % inverse,     &
                                              kind=int_index) ), &
                      stat=stat,                                 &
                      errmsg=errmsg )
            if ( stat /= 0 ) then
                write(error_unit, '(a)') 'Allocation ERRMSG: ' // trim(errmsg)
                error stop submodule_name // ' % ' // procedure // ': ' // &
                    alloc_inv_fault
            end if

            dummy_inverse(1:size(map % inverse, kind=int_index)) = &
                map % inverse(:)

            call move_alloc( dummy_inverse, map % inverse )

        end subroutine expand_inverse

    end subroutine map_chain_entry


    module subroutine rehash_chaining_map( map, hasher )
!! Version: Experimental
!!
!! Changes the hashing method of the table entries to that of HASHER.
!! Arguments:
!!     map    the table to be rehashed
!!     hasher the hasher function to be used for the table
!
        class(chaining_hashmap_type), intent(inout) :: map
        procedure(hasher_fun)                       :: hasher

        integer(int_hash)  :: hash_val
        integer(int_index) :: i
        integer(int_index) :: index

        map % hasher => hasher

        do i=0, size( map % slots, kind=int_index ) - 1
            map % slots(i) % target => null()
        end do

        do i=1, map % num_entries + map % num_free
            if ( .not. associated( map % inverse(i) % target ) ) cycle
            hash_val = map % hasher ( map % inverse(i) % target % key )
            map % inverse(i) % target % hash_val = hash_val
            index = fibonacci_hash( hash_val, map % nbits )
            map % inverse(i) % target % inmap = i
            if ( associated( map % slots(index) % target ) ) then
                map % inverse(i) % target % next => map % slots(index) % target
                map % slots(index) % target => map % inverse(i) % target
            else
                map % slots(index) % target => map % inverse(i) % target
                map % slots(index) % target % next => null()
            end if
        end do

    end subroutine rehash_chaining_map


    module subroutine remove_chaining_entry(map, key, existed)
!! Remove the entry, if any, that has the key
!! Arguments:
!!    map     - the table from which the entry is to be removed
!!    key     - the key to an entry
!!    existed - a logical flag indicating whether an entry with the key
!!              was present in the original map
!
        class(chaining_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)                  :: key
        logical, intent(out), optional              :: existed

        type(chaining_map_entry_type), pointer :: bucket, aentry, bentry, centry
        integer(int_hash)                      :: hash_val
        integer(int_index)                     :: inmap, k, level

        call in_chain_map( map, inmap, key )
        if ( inmap < 1 .or. inmap > size( map % inverse ) ) then
            if ( present( existed ) ) existed = .false.
            return
        end if

        bucket => map % inverse(inmap) % target
        if ( .not. associated(bucket) ) then
            if ( present( existed ) ) existed = .false.
            return
        end if
        if ( present(existed) ) existed = .true.
        hash_val = bucket % hash_val
        k = fibonacci_hash( hash_val, map % nbits )
        allocate(aentry)
        aentry => map % slots(k) % target
        if ( associated(aentry) ) then
            if ( aentry % inmap == inmap ) then
                bentry => aentry % next
                map % slots(k) % target => bentry
                aentry % next => map % free_list
                map % free_list => aentry
                map % inverse(inmap) % target => null()
                map % num_free = map % num_free + 1
                map % num_entries = map % num_entries - 1
                return
            end if
        else
            return
        end if
        level = 1
        centry => map % slots(k) % target
        aentry => aentry % next

        FIND_SLOTS_ENTRY:do
            if ( .not. associated(aentry) ) return
            if ( aentry % inmap == inmap ) exit
            centry => aentry
            aentry => aentry % next
            level = level + 1
        end do FIND_SLOTS_ENTRY

        bentry => aentry % next
        aentry % next => map % free_list
        map % free_list => aentry
        centry % next => bentry
        map % inverse(inmap) % target => null()
        map % num_free = map % num_free + 1
        map % num_entries = map % num_entries - 1

    end subroutine remove_chaining_entry


    module subroutine set_other_chaining_data( map, key, other, exists )
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
        class(chaining_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)                  :: key
        class(*), intent(in)                        :: other
        logical, intent(out), optional              :: exists

        integer(int_index) :: inmap
        character(*), parameter :: procedure = 'SET_OTHER_DATA'

        call in_chain_map( map, inmap, key )
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

    end subroutine set_other_chaining_data


    module function total_chaining_depth( map ) result(total_depth)
!! Version: Experimental
!!
!! Returns the total number of ones based offsets of slot entries from
!! their slot index for a hash map
!! Arguments:
!!     map - an chaining hash map
        class(chaining_hashmap_type), intent(in) :: map
        integer(int_depth)                       :: total_depth

        type(chaining_map_entry_type), pointer :: current_key
        integer(int_index) :: slot, slots
        integer(int_depth) :: index

        total_depth = 0_int_depth
        slots = size( map % slots, kind=int_index )
        do slot=0, slots-1
            current_key => map % slots(slot) % target
            index = 0_int_depth
            do while( associated(current_key) )
                index = index + 1_int_depth
                total_depth = total_depth + index
                current_key => current_key % next
            end do
        end do

    end function total_chaining_depth


    module subroutine chaining_key_test(map, key, present)
!! Version: Experimental
!!
!! Returns a logical flag indicating whether KEY is present in the hash map
!! Arguments:
!!     map     - the hash map of interest
!!     key     - the key of interest
!!     present - a logical flag indicating whether key is present in map
!
        class(chaining_hashmap_type), intent(inout) :: map
        type(key_type), intent(in)                  :: key
        logical, intent(out)                        :: present

        integer(int_index) :: inmap

        call in_chain_map( map, inmap, key )
        if ( inmap <= 0 .or. inmap > size( map % inverse, kind=int_index ) ) &
            then
            present = .false.
        else
            present = associated( map % inverse(inmap) % target )
        end if

    end subroutine chaining_key_test


end submodule stdlib_hashmap_chaining
