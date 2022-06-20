!! The module, STDLIB_HASH_MAPS, implements two hash maps:
!! CHAINING_HASH_MAP_TYPE, a separate chaining hash map; and OPEN_HASH_MAP_TYPE,
!! an open addressing hash map using linear addressing. The two hash maps are
!! implementations of the abstract type, HASH_MAP_TYPE.

module stdlib_hashmaps

    use, intrinsic :: iso_fortran_env, only: &
        character_storage_size,              &
        error_unit

    use stdlib_kinds, only: &
        dp,                 &
        int8,               &
        int16,              &
        int32,              &
        int64

    use stdlib_hashmap_wrappers

    implicit none

    private

!! Public data_types
    public ::                  &
        chaining_hashmap_type, &
        hashmap_type,          &
        open_hashmap_type

!! Values that parameterize David Chase's empirical SLOT expansion code
    integer, parameter ::        &
        inmap_probe_factor = 10, &
        map_probe_factor   =  5

!! Values that parameterize the SLOTS table size
    integer, parameter, public :: &
        default_bits =  6,        &
        max_bits     = 30

!! KIND values used to parameterixe the hash map and its procedures
    integer, parameter, public :: &
        int_calls  = int64,       &
        int_depth  = int64,       &
        int_index  = int32,       &
        int_probes = int64

!! Error codes returned by the hash map procedures
    integer, parameter, public ::  &
        success = 0,               &
        alloc_fault = 1,           &
        array_size_error = 2

! The number of bits used by various types
    integer, parameter ::             &
! Should be 8
        int8_bits = bit_size(0_int8), &
        char_bits = character_storage_size

!! The hash map load factor
    real, parameter, public ::      &
        load_factor = 0.5625

!! The size of the pools of allocated map entries
    integer(int32), parameter :: pool_size = 64

    character(*), parameter, private :: module_name = 'STDLIB_HASHMAPS'

    type, abstract :: hashmap_type
!! Version: Experimental
!!
!! Type implementing an abstract hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-hashmap_type-abstract-type))
        private
        integer(int_calls) :: call_count = 0
!! Number of calls
        integer(int_calls) :: probe_count = 0
!! Number of probes since last expansion
        integer(int_calls) :: total_probes = 0
!! Cumulative number of probes
        integer(int_index) :: num_entries = 0
!! Number of entries
        integer(int_index) :: num_free = 0
!! Number of elements in the free_list
        integer(int32)     :: nbits = default_bits
!! Number of bits used to address the slots
        procedure(hasher_fun), pointer, nopass :: hasher => fnv_1_hasher
!! Hash function

    contains

        procedure, non_overridable, pass(map) :: calls
        procedure, non_overridable, pass(map) :: entries
        procedure, non_overridable, pass(map) :: map_probes
        procedure, non_overridable, pass(map) :: num_slots
        procedure, non_overridable, pass(map) :: slots_bits
        procedure(get_other), deferred, pass(map)    :: get_other_data
        procedure(init_map), deferred, pass(map)     :: init
        procedure(key_test), deferred, pass(map)     :: key_test
        procedure(loading), deferred, pass(map)      :: loading
        procedure(map_entry), deferred, pass(map)    :: map_entry
        procedure(rehash_map), deferred, pass(map)   :: rehash
        procedure(remove_entry), deferred, pass(map) :: remove
        procedure(set_other), deferred, pass(map)    :: set_other_data
        procedure(total_depth), deferred, pass(map)  :: total_depth

    end type hashmap_type


    abstract interface

        subroutine get_other( map, key, other, exists )
!! Version: Experimental
!!
!! Returns the other data associated with the inverse table index
!! Arguments:
!!     map    - a hash map
!!     key    - the key associated with a map entry
!!     other  - the other data associated with the key
!!     exists - a logical flag indicating whether an entry with that key exists
!
            import hashmap_type, key_type, other_type
            class(hashmap_type), intent(inout) :: map
            type(key_type), intent(in)         :: key
            type(other_type), intent(out)      :: other
            logical, intent(out), optional     :: exists
        end subroutine get_other

        subroutine init_map( map,         &
                             hasher,      &
                             slots_bits,  &
                             status )
!! Version: Experimental
!!
!! Routine to allocate an empty map with HASHER as the hash function,
!! 2**SLOTS_BITS initial SIZE(map % slots), SIZE(map % slots) limited to a
!! maximum of 2**MAX_BITS, and with up to LOAD_FACTOR * SIZE(map % slots),
!! map % inverse elements. All fields are initialized.
!! Arguments:
!!     map         - the hash maap to be initialized
!!     hasher      - the hash function to be used to map keys to slots
!!     slots_bits   - the number of bits initially used to map to the slots
!!     status      - an integer error status flag with the allowed values:
!!         success - no problems were found
!!         alloc_fault - map % slots or map % inverse could not be allocated
!!         array_size_error - slots_bits or max_bits is less than
!!             default_bits or greater than strict_max_bits
!!         real_value_error - load_factor is less than 0.375 or greater than
!!             0.875
!
            import hashmap_type, hasher_fun, int32
            class(hashmap_type), intent(out)     :: map
            procedure(hasher_fun)                 :: hasher
            integer, intent(in), optional         :: slots_bits
            integer(int32), intent(out), optional :: status
        end subroutine init_map

        subroutine key_test(map, key, present)
!! Version: Experimental
!!
!! Returns a logical flag indicating whether KEY exists in the hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#key_test-indicates-whether-key-is-present))
!!
!! Arguments:
!!     map     - the hash map of interest
!!     key     - the key of interest
!!     present - a flag indicating whether key is present in the map
!
            import hashmap_type, key_type
            class(hashmap_type), intent(inout) :: map
            type(key_type), intent(in)         :: key
            logical, intent(out)               :: present
        end subroutine key_test

        pure function loading( map )
!! Version: Experimental
!!
!! Returns the number of entries relative to slots in a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#loading-returns-the-ratio-of-entries-to-slots))
!!
!! Arguments:
!!       map - a hash map
            import hashmap_type
            class(hashmap_type), intent(in) :: map
            real :: loading
        end function loading

        subroutine map_entry(map, key, other, conflict)
!! Version: Experimental
!!
!! Inserts an entry into the hash table
!! ([Specifications](../page/specs/stdlib_hashmaps.html#map_entry-inserts-an-entry-into-the-hash-map))
!!
            import hashmap_type, key_type, other_type
            class(hashmap_type), intent(inout)     :: map
            type(key_type), intent(in)             :: key
            type(other_type), intent(in), optional :: other
            logical, intent(out), optional         :: conflict
        end subroutine map_entry

        subroutine rehash_map( map, hasher )
!! Version: Experimental
!!
!! Changes the hashing method of the table entries to that of HASHER.
!! Arguments:
!!     map      the table to be rehashed
!!     hasher the hasher function to be used for the table
!
            import hashmap_type, hasher_fun
            class(hashmap_type), intent(inout) :: map
            procedure(hasher_fun)              :: hasher
        end subroutine rehash_map

        subroutine remove_entry(map, key, existed) ! Chase's delent
!! Version: Experimental
!!
!! Remove the entry, if any, that has the key
!! Arguments:
!!    map     - the table from which the entry is to be removed
!!    key     - the key to an entry
!!    existed - a logical flag indicating whether an entry with the key
!!              was present in the original map
!
            import hashmap_type, key_type
            class(hashmap_type), intent(inout) :: map
            type(key_type), intent(in)         :: key
            logical, intent(out), optional     :: existed
        end subroutine remove_entry

        subroutine set_other( map, key, other, exists )
!! Version: Experimental
!!
!! Change the other data associated with the key
!! Arguments:
!!     map    - the map with the entry of interest
!!     key    - the key to the entry inthe map
!!     other  - the new data to be associated with the key
!!     exists - a logical flag indicating whether the key is already entered
!!              in the map
!!
!
            import hashmap_type, key_type, other_type
            class(hashmap_type), intent(inout) :: map
            type(key_type), intent(in)         :: key
            type(other_type), intent(in)       :: other
            logical, intent(out), optional     :: exists
        end subroutine set_other

        function total_depth( map )
!! Version: Experimental
!!
!! Returns the total number of ones based offsets of slot entriesyy from
!! their slot index for a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#total_depth-returns-the-total-depth-of-the-hash-map-entries))
!! Arguments:
!!     map - a hash map
            import hashmap_type, int64
            class(hashmap_type), intent(in) :: map
            integer(int64)                   :: total_depth
        end function total_depth

    end interface

!! API for the chaining_hashmap_type

    type :: chaining_map_entry_type  ! Hash entry
!! Version: Experimental
!!
!! Chaining hash map entry type
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_map_entry_type-derived-type))
        private
        integer(int_hash)  :: hash_val
!! Full hash value
        type(key_type)     :: key
!! The entry's key
        type(other_type)   :: other
!! Other entry data
        integer(int_index) :: inmap
!! Index into inverse table
        type(chaining_map_entry_type), pointer :: next => null()
!! Next bucket
    end type chaining_map_entry_type


    type chaining_map_entry_ptr
!! Version: Experimental
!!
!! Wrapper for a pointer to a chaining map entry type object
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_map_entry_type_ptr-derived-type))
        type(chaining_map_entry_type), pointer :: target => null()
    end type chaining_map_entry_ptr


    type :: chaining_map_entry_pool
!! Version: Experimental
!!
!! Type implementing a pool of allocated `chaining_map_entry_type`
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_map_entry_pool-derived-type))
        private
! Index of next bucket
        integer(int_index)                         :: next = 0
        type(chaining_map_entry_type), allocatable :: more_map_entries(:)
        type(chaining_map_entry_pool), pointer     :: lastpool => null()
    end type chaining_map_entry_pool


    type, extends(hashmap_type) :: chaining_hashmap_type
!! Version: Experimental
!!
!! Type implementing the `chaining_hashmap_type` types
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-chaining_hashmap_type-derived-type))
        private
        type(chaining_map_entry_pool), pointer    :: cache => null()
!! Pool of allocated chaining_map_entry_type objects
        type(chaining_map_entry_type), pointer    :: free_list => null()
!! free list of map entries
        type(chaining_map_entry_ptr), allocatable :: inverse(:)
!! Array of bucket lists (inverses) Note max_elts=size(inverse)
        type(chaining_map_entry_ptr), allocatable :: slots(:)
!! Array of bucket lists Note # slots=size(slots)
    contains
        procedure :: get_other_data => get_other_chaining_data
        procedure :: init => init_chaining_map
        procedure :: loading => chaining_loading
        procedure :: map_entry => map_chain_entry
        procedure :: rehash => rehash_chaining_map
        procedure :: remove => remove_chaining_entry
        procedure :: set_other_data => set_other_chaining_data
        procedure :: total_depth => total_chaining_depth
        procedure :: key_test => chaining_key_test
        final     :: free_chaining_map
    end type chaining_hashmap_type


    interface

        module subroutine free_chaining_map( map )
!! Version: Experimental
!!
!! Frees internal memory of an chaining map
!! Arguments:
!!     map - the chaining hash map whose memory is to be freed
!
            type(chaining_hashmap_type), intent(inout) :: map
        end subroutine free_chaining_map


        module subroutine get_other_chaining_data( map, key, other, exists )
!! Version: Experimental
!!
!! Returns the other data associated with the inverse table index
!! Arguments:
!!     map   - a chaining hash table
!!     key   - the key associated with a map entry
!!     other - the other data associated with the key
!!     exists - a logical flag indicating whether an entry with that key exists
!
            class(chaining_hashmap_type), intent(inout) :: map
            type(key_type), intent(in)                  :: key
            type(other_type), intent(out)               :: other
            logical, intent(out), optional              :: exists
        end subroutine get_other_chaining_data


        module subroutine init_chaining_map( map,       &
                                             hasher,    &
                                             slots_bits, &
                                             status )
!! Version: Experimental
!!
!! Routine to allocate an empty map with HASHER as the hash function,
!! 2**SLOTS_BITS initial SIZE(map % slots), and SIZE(map % slots) limited
!! to a maximum of 2**MAX_BITS. All fields are initialized.
!! Arguments:
!!     map       - the chaining hash map to be initialized
!!     hasher    - the hash function to be used to map keys to slots
!!     slots_bits - the bits of two used to initialize the number of slots
!!     status    - an integer error status flag with the allowed values:
!!         success - no problems were found
!!         alloc_fault - map % slots or map % inverse could not be allocated
!!         array_size_error - slots_bits is less than default_bits or
!!             greater than max_bits
!
            class(chaining_hashmap_type), intent(out)  :: map
            procedure(hasher_fun)                      :: hasher
            integer, intent(in), optional              :: slots_bits
            integer(int32), intent(out), optional      :: status
        end subroutine init_chaining_map


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
        end subroutine chaining_key_test


        pure module function chaining_loading( map )
!! Version: Experimental
!!
!! Returns the number of entries relative to slots in a hash map
!! Arguments:
!!      map - a chaining hash map
            class(chaining_hashmap_type), intent(in) :: map
            real :: chaining_loading
        end function chaining_loading


        module subroutine map_chain_entry(map, key, other, conflict)
!
!     Inserts an entry innto the hash map
!     Arguments:
!!      map      - the hash table of interest
!!      key      - the key identifying the entry
!!      other    - other data associated with the key
!!      conflict - logical flag indicating whether the entry key conflicts
!!                 with an existing key
!
            class(chaining_hashmap_type), intent(inout) :: map
            type(key_type), intent(in)             :: key
            type(other_type), intent(in), optional :: other
            logical, intent(out), optional         :: conflict
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
        end subroutine rehash_chaining_map


        module subroutine remove_chaining_entry(map, key, existed)
!! Version: Experimental
!!
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
            type(other_type), intent(in)                :: other
            logical, intent(out), optional              :: exists
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
        end function total_chaining_depth

    end interface

!! API for the open_hashmap_type

    type :: open_map_entry_type
!! Version: Experimental
!!
!! Open hash map entry type
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-open_map_entry_type-derived-type))
        private
        integer(int_hash) :: hash_val
!! Full hash value
        type(key_type)    :: key
!! Hash entry key
        type(other_type)  :: other
!! Other entry data
        integer(int_index) :: inmap
!! Index into inverse table
    end type open_map_entry_type

    type :: open_map_entry_list
!! Version: Experimental
!!
!! Open hash map entry type
        private
        type(open_map_entry_type), pointer :: target => null()
        type(open_map_entry_list), pointer :: next => null()
    end type open_map_entry_list


    type open_map_entry_ptr
!! Version: Experimental
!!
!! Wrapper for a pointer to an open hash map entry type object
!! ([Specifications](../page/specs/stdlib_hashmaps.html#the-open_map_entry_ptr-derived-type))
        type(open_map_entry_type), pointer :: target => null()
    end type open_map_entry_ptr


    type :: open_map_entry_pool
!! Version: Experimental
!!
!! Type implementing a pool of allocated `open_map_entry_type`
        private
        integer(int_index)                     :: next = 0
!! Index of next bucket
        type(open_map_entry_type), allocatable :: more_map_entries(:)
        type(open_map_entry_pool), pointer     :: lastpool => null()
    end type open_map_entry_pool


    type, extends(hashmap_type) :: open_hashmap_type
!! Version: Experimental
!!
!! Type implementing an "open" hash map
        private
        integer(int_index) :: index_mask = 2_int_index**default_bits-1
!! Mask used in linear addressing
        type(open_map_entry_pool), pointer    :: cache => null()
!! Pool of allocated open_map_entry_type objects
        type(open_map_entry_list), pointer    :: free_list => null()
!! free list of map entries
        type(open_map_entry_ptr), allocatable  :: inverse(:)
!! Array of bucket lists (inverses) Note max_elts=size(inverse)
        integer(int_index), allocatable        :: slots(:)
!! Array of indices to the inverse Note # slots=size(slots)
    contains
        procedure :: get_other_data => get_other_open_data
        procedure :: init => init_open_map
        procedure :: loading => open_loading
        procedure :: map_entry => map_open_entry
        procedure :: rehash => rehash_open_map
        procedure :: remove => remove_open_entry
        procedure :: set_other_data => set_other_open_data
        procedure :: total_depth => total_open_depth
        procedure :: key_test => open_key_test
        final     :: free_open_map
    end type open_hashmap_type

    interface

        module subroutine free_open_map( map )
!! Version: Experimental
!!
!! Frees internal memory of an open map
!! Arguments:
!!     map - the open hash map whose memory is to be freed
!
            type(open_hashmap_type), intent(inout) :: map
        end subroutine free_open_map


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
            type(other_type), intent(out)           :: other
            logical, intent(out), optional          :: exists
        end subroutine get_other_open_data


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
            procedure(hasher_fun)                      :: hasher
            integer, intent(in), optional              :: slots_bits
            integer(int32), intent(out), optional      :: status
        end subroutine init_open_map


        module subroutine open_key_test(map, key, present)
!! Version: Experimental
!!
!! Returns a logical flag indicating whether KEY exists in the hash map
!! Arguments:
!!     map     - the hash map of interest
!!     key     - the key of interest
!!     present - a logical flag indicating whether KEY exists in the hash map
!
            class(open_hashmap_type), intent(inout) :: map
            type(key_type), intent(in)              :: key
            logical, intent(out)                    :: present
        end subroutine open_key_test


        pure module function open_loading( map )
!! Version: Experimental
!!
!! Returns the number of entries relative to slots in a hash map
!! Arguments:
!!       map - an open hash map
            class(open_hashmap_type), intent(in) :: map
            real :: open_loading
        end function open_loading


        module subroutine map_open_entry(map, key, other, conflict)
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
            class(open_hashmap_type), intent(inout) :: map
            type(key_type), intent(in)              :: key
            type(other_type), intent(in), optional  :: other
            logical, intent(out), optional          :: conflict
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
            type(other_type), intent(in)            :: other
            logical, intent(out), optional          :: exists
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
        end function total_open_depth

    end interface

contains

    pure function calls( map )
!! Version: Experimental
!!
!! Returns the number of subroutine calls on an open hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#calls-returns-the-number-of-calls-on-the-hash-map))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_calls)              :: calls

        calls = map % call_count

    end function calls

    pure function entries( map )
!! Version: Experimental
!!
!! Returns the number of entries in a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#entries-returns-the-number-of-entries-in-the-hash-map))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_index) :: entries

        entries = map % num_entries

    end function entries


    pure function map_probes( map )
!! Version: Experimental
!!
!! Returns the total number of table probes on a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#map_probes-returns-the-number-of-hash-map-probes))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_calls) :: map_probes

        map_probes = map % total_probes + map % probe_count

    end function map_probes


    pure function num_slots( map )
!! Version: Experimental
!!
!! Returns the number of allocated slots in a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#num_slots-returns-the-number-of-hash-map-slots))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer(int_index)              :: num_slots

        num_slots = 2**map % nbits

    end function num_slots


    pure function slots_bits( map )
!! Version: Experimental
!!
!! Returns the number of bits used to specify the number of allocated
!! slots in a hash map
!! ([Specifications](../page/specs/stdlib_hashmaps.html#slots_bits-returns-the-number-of-bits-used-to-address-the-hash-map-slots))
!!
!! Arguments:
!!     map - an open hash map
        class(hashmap_type), intent(in) :: map
        integer                              :: slots_bits

        slots_bits = map % nbits

    end function slots_bits


end module stdlib_hashmaps
