!! The module STDLIB_HASHMAP_WRAPPERS provides wrappers for various
!! entities used by the hash map procedures. These include wrappers for the
!! `key` and `other` data, and hashing procedures to operate on entities of
!! the `key_type`.

module stdlib_hashmap_wrappers

    use, intrinsic :: iso_fortran_env, only : &
        character_storage_size

    use stdlib_hash_32bit

    use stdlib_kinds, only : &
        int8,                &
        int16,               &
        int32,               &
        int64,               &
        dp

    implicit none

    private

!! Public procedures
    public ::                    &
        copy_key,                &
        fibonacci_hash,          &
        fnv_1_hasher,            &
        fnv_1a_hasher,           &
        free_key,                &
        get,                     &
        hasher_fun,              &
        operator(==),            &
        seeded_nmhash32_hasher,  &
        seeded_nmhash32x_hasher, &
        seeded_water_hasher,     &
        set

!! Public types
    public ::      &
        key_type

!! Public integers
    public ::   &
        int_hash

    integer, parameter ::               &
! Should be 8
        bits_int8  = bit_size(0_int8)

    integer, parameter ::                   &
        bits_char = character_storage_size, &
        bytes_char = bits_char/bits_int8

    character(*), parameter :: module_name = "STDLIB_HASHMAP_WRAPPERS"

    type :: key_type
!! Version: Experimental
!!
!! A wrapper type for the key's true type
!        private
        integer(int8), allocatable :: value(:)
    end type key_type

    abstract interface
!! Version: Experimental
!!
!! Abstract interface to a 64 bit hash function operating on a KEY_TYPE
        pure function hasher_fun( key )  result(hash_value)
            import key_type, int_hash
            type(key_type), intent(in)    :: key
            integer(int_hash)             :: hash_value
        end function hasher_fun
    end interface

    
    interface get

        module procedure get_char_key,   &
                         get_int8_key,   &
                         get_int32_key

    end interface get


    interface operator(==)
        module procedure equal_keys
    end interface operator(==)

    interface set

        module procedure set_char_key,   &
                         set_int8_key,   &
                         set_int32_key

    end interface set

contains


    pure subroutine copy_key( old_key, new_key )
!! Version: Experimental
!!
!! Copies the contents of the key, old_key, to the key, new_key
!! ([Specifications](../page/specs/stdlib_hashmaps.html#copy_key-returns-a-copy-of-the-key))
!!
!! Arguments:
!!     old_key - the input key
!!     new_key - the output copy of old_key
        type(key_type), intent(in)  :: old_key
        type(key_type), intent(out) :: new_key

        new_key % value = old_key % value

    end subroutine copy_key


    function equal_keys( key1, key2 ) result(test) ! Chase's tester
!! Version: Experimental
!!
!! Compares two keys for equality
!! ([Specifications](../page/specs/stdlib_hashmaps.html#operator(==)-compares-two-keys-for-equality))
!!
!! Arguments:
!!     key1 - the first key
!!     key2 - the second key
        logical                    :: test
        type(key_type), intent(in) :: key1
        type(key_type), intent(in) :: key2

        if ( size(key1 % value, kind=int64) /= &
             size(key2 % value, kind=int64) ) then
            test = .false.
            return
        end if

        if ( all( key1 % value == key2 % value ) ) then
            test = .true.
        else
            test = .false.
        end if

    end function equal_keys


    subroutine free_key( key )
!! Version: Experimental
!!
!! Frees the memory in a key
!! ([Specifications](../page/specs/stdlib_hashmaps.html#free_key-frees-the-memory-associated-with-a-key))
!!
!! Arguments:
!!     key  - the key
        type(key_type), intent(inout) :: key

        if ( allocated( key % value ) ) deallocate( key % value )

    end subroutine free_key


    subroutine get_char_key( key, value )
!! Version: Experimental
!!
!! Gets the contents of the key as a CHARACTER string
!! Arguments:
!!     key   - the input key
!!     value - the contents of key mapped to a CHARACTER string
        type(key_type), intent(in)             :: key
        character(:), allocatable, intent(out) :: value
        character(*), parameter :: procedure = "GET"

        integer(int64) :: key_as_char
        integer(int64) :: key_size

        key_size = size( key % value, kind=int64 )
        select case( bytes_char )
        case(1)
            key_as_char = key_size
        case(2)
            if ( iand( key_size, 1_int64 ) > 0 ) then
                error stop module_name // " % " // procedure // &
                          ": Internal Error at stdlib_hashmaps: " // &
                           "System uses 2 bytes per character, so " // &
                           "key_size can't be an odd number."
            end if
            key_as_char = ishft( key_size, -1 )
        case(4)
            if ( iand( key_size, 3_int64) > 0 ) then
                error stop module_name // " % " // procedure // &
                          ": Internal Error at stdlib_hashmaps: " // &
                           "System uses 4 bytes per character, and " // &
                           "key_size is not a multiple of four."
            end if
            key_as_char = ishft( key_size, -2 )
        case default
            error stop module_name // " % " // procedure // &
                       ": Internal Error: " // &
                       "System doesn't use a power of two for its " // &
                       "character size as expected by stdlib_hashmaps."
        end select

        allocate( character( len=key_as_char ) :: value )

        value(1:key_as_char) = transfer( key % value, value )

    end subroutine get_char_key


    subroutine get_int8_key( key, value )
!! Version: Experimental
!!
!! Gets the contents of the key as an INTEGER(INT8) vector
!! Arguments:
!!     key   - the input key
!!     value - the contents of key mapped to an INTEGER(INT8) vector
        type(key_type), intent(in)              :: key
        integer(int8), allocatable, intent(out) :: value(:)

        value = key % value

    end subroutine get_int8_key


    pure subroutine get_int32_key( key, value )
!! Version: Experimental
!!
!! Gets the contents of the key as an INTEGER(INT32) vector
!! Arguments:
!!     key   - the input key
!!     value - the contents of key mapped to an INTEGER(INT32) vector
        type(key_type), intent(in)              :: key
        integer(int32), allocatable, intent(out) :: value(:)
        
        value = transfer( key % value, value )
        
    end subroutine get_int32_key


    subroutine set_char_key( key, value )
!! Version: Experimental
!!
!! Sets the contents of the key from a CHARACTER string
!! Arguments:
!!     key   - the output key
!!     value - the input CHARACTER string
        type(key_type), intent(out) :: key
        character(*), intent(in)    :: value

        key % value = transfer( value, key % value, &
                                bytes_char * len( value ) )

    end subroutine set_char_key


    subroutine set_int8_key( key, value )
!! Version: Experimental
!!
!! Sets the contents of the key from an INTEGER(INT8) vector
!! Arguments:
!!     key   - the output key
!!     value - the input INTEGER(INT8) vector
        type(key_type), intent(out) :: key
        integer(int8), intent(in)   :: value(:)

        key % value = value

    end subroutine set_int8_key


    pure subroutine set_int32_key( key, value )
!! Version: Experimental
!!
!! Sets the contents of the key from an INTEGER(INT32) vector
!! Arguments:
!!     key   - the output key
!!     value - the input INTEGER(INT32) vector
        type(key_type), intent(out) :: key
        integer(int32), intent(in)   :: value(:)
                
        key % value = transfer(value, key % value)
                
    end subroutine set_int32_key


    pure function fnv_1_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the FNV_1 algorithm
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: fnv_1_hasher

        fnv_1_hasher = fnv_1_hash( key % value )

    end function fnv_1_hasher


    pure function fnv_1a_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the FNV_1a algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#fnv_1a_hasher-calculates-a-hash-code-from-a-key))
!!
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: fnv_1a_hasher

        fnv_1a_hasher = fnv_1a_hash( key % value )

    end function fnv_1a_hasher


    pure function seeded_nmhash32_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the NMHASH32 hash algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#seeded_nmhash32_hasher-calculates-a-hash-code-from-a-key))
!!
!! Arguments:
!!     key  - the key to be hashed
!!     seed - the seed (unused) for the hashing algorithm
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: seeded_nmhash32_hasher

        seeded_nmhash32_hasher = nmhash32( key % value, &
            int( z'DEADBEEF', int32 ) )

    end function seeded_nmhash32_hasher


    pure function seeded_nmhash32x_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the NMHASH32X hash algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#seeded_nmhash32x_hasher-calculates-a-hash-code-from-a-key))
!! Arguments:
!!     key  - the key to be hashed
!!     seed - the seed (unused) for the hashing algorithm
        type(key_type), intent(in)    :: key
        integer(int_hash)             :: seeded_nmhash32x_hasher

        seeded_nmhash32x_hasher = nmhash32x( key % value, &
            int( z'DEADBEEF', int32 ) )

    end function seeded_nmhash32x_hasher


    pure function seeded_water_hasher( key )
!! Version: Experimental
!!
!! Hashes a key with the waterhash algorithm
!! ([Specifications](../page/specs/stdlib_hashmaps.html#seeded_water_hasher-calculates-a-hash-code-from-a-key))
!!
!! Arguments:
!!     key  - the key to be hashed
        type(key_type), intent(in)  :: key
        integer(int_hash)           :: seeded_water_hasher

        seeded_water_hasher = water_hash( key % value, &
            int( z'DEADBEEF1EADBEEF', int64 ) )

    end function seeded_water_hasher


end module stdlib_hashmap_wrappers
