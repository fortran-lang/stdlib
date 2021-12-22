
module stdlib_hash_32bit

    use, intrinsic :: iso_fortran_env, only : &
        character_storage_size

    use stdlib_kinds, only: &
        dp,                 &
        int8,               &
        int16,              &
        int32,              &
        int64

    implicit none

    private

    integer, parameter, public :: &
        int_hash     = int32
!! The number of bits in the output hash

! pow32_over_phi is the odd integer that most closely approximates 2**32/phi,
! where phi is the golden ratio 1.618...
    integer(int32), parameter ::                 &
        pow32_over_phi = int( z'9E3779B9', int32 )

! The number of bits used by each integer type
    integer, parameter ::               &
! Should be 8
        bits_int8  = bit_size(0_int8),  &
! Should be 16
        bits_int16 = bit_size(0_int16), &
! Should be 32
        bits_int32 = bit_size(0_int32), &
! Should be 64
        bits_int64 = bit_size(0_int64)

    integer, parameter ::                   &
! Should be 1
        bytes_int8  = bits_int8/bits_int8,  &
! Should be 2
        bytes_int16 = bits_int16/bits_int8, &
! Should be 4
        bytes_int32 = bits_int32/bits_int8, &
! Should be 8
        bytes_int64 = bits_int64/bits_int8

    integer, parameter ::                   &
        bits_char = character_storage_size, &
        bytes_char = bits_char/bits_int8

! Dealing with different endians
    logical, parameter, public ::                                    &
        little_endian = ( 1 == transfer([1_int8, 0_int8], 0_int16) )

    public ::               &
        fibonacci_hash,     &
        fnv_1_hash,         &
        fnv_1a_hash,        &
        new_nmhash32_seed,  &
        new_nmhash32x_seed, &
        new_water_hash_seed,&
        nmhash32,           &
        nmhash32x,          &
        odd_random_integer, &
        universal_mult_hash,&
        water_hash


    interface fnv_1_hash
!! Version: experimental
!!
!! FNV_1 interfaces
!! ([Specification](../page/specs/stdlib_hash_procedures.html#fnv_1_hash-calculates-a-hash-code-from-a-key))
          pure module function int8_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 array keys of kind int8
              integer(int8), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int8_fnv_1

          pure module function int16_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 array keys of kind int16
              integer(int16), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int16_fnv_1

          pure module function int32_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 array keys of kind int32
              integer(int32), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int32_fnv_1

          pure module function int64_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 array keys of kind int64
              integer(int64), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int64_fnv_1


        elemental module function character_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for default character string keys
            character(*), intent(in)      :: key
            integer(int_hash)             :: hash_code
        end function character_fnv_1

    end interface fnv_1_hash

    interface fnv_1a_hash
!! Version: experimental
!!
!! FNV_1A interfaces
!! ([Specification](../page/specs/stdlib_hash_procedures.html#fnv_1a_hash-calculates-a-hash-code-from-a-key))
          pure module function int8_fnv_1a( key ) result(hash_value)
!! FNV_1A hash function for rank 1 array keys of kind int8
              integer(int8), intent(in)   :: key(:)
              integer(int_hash)             :: hash_value
          end function int8_fnv_1a

          pure module function int16_fnv_1a( key ) result(hash_value)
!! FNV_1A hash function for rank 1 array keys of kind int16
              integer(int16), intent(in)   :: key(:)
              integer(int_hash)             :: hash_value
          end function int16_fnv_1a

          pure module function int32_fnv_1a( key ) result(hash_value)
!! FNV_1A hash function for rank 1 array keys of kind int32
              integer(int32), intent(in)   :: key(:)
              integer(int_hash)             :: hash_value
          end function int32_fnv_1a

          pure module function int64_fnv_1a( key ) result(hash_value)
!! FNV_1A hash function for rank 1 array keys of kind int64
              integer(int64), intent(in)   :: key(:)
              integer(int_hash)             :: hash_value
          end function int64_fnv_1a


        elemental module function character_fnv_1a( key ) result(hash_value)
!! FNV_1A hash function for default character string keys
            character(*), intent(in)      :: key
            integer(int_hash)             :: hash_value
        end function character_fnv_1a

    end interface fnv_1a_hash

    interface nmhash32
!! Version: experimental
!!
!! NMHASH32 interfaces
!! ([Specification](../page/specs/stdlib_hash_procedures.html#nmhash32-calculates-a-hash-code-from-a-key-and-a-seed))
          pure module function int8_nmhash32( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int8
              integer(int8), intent(in)   :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int8_nmhash32

          pure module function int16_nmhash32( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int16
              integer(int16), intent(in)   :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int16_nmhash32

          pure module function int32_nmhash32( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int32
              integer(int32), intent(in)   :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int32_nmhash32

          pure module function int64_nmhash32( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int64
              integer(int64), intent(in)   :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int64_nmhash32


        elemental module function character_nmhash32( key, seed ) &
            result(hash_value)
!! NMHASH32 hash function for default character string keys
            character(*), intent(in)      :: key
            integer(int32), intent(in) :: seed
            integer(int32) :: hash_value
        end function character_nmhash32

    end interface nmhash32

    interface nmhash32x
!! Version: experimental
!!
!! NMHASH32X interfaces
!! ([Specification](file:///home/jvandenp/stdlib/API-doc/page/specs/stdlib_hash_procedures.html#nmhash32x-calculates-a-hash-code-from-a-key-and-a-seed))
          pure module function int8_nmhash32x( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int8
              integer(int8), intent(in) :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int8_nmhash32x

          pure module function int16_nmhash32x( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int16
              integer(int16), intent(in) :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int16_nmhash32x

          pure module function int32_nmhash32x( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int32
              integer(int32), intent(in) :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int32_nmhash32x

          pure module function int64_nmhash32x( key, seed ) &
              result(hash_value)
!! NMHASH32 hash function for rank 1 array keys of kind int64
              integer(int64), intent(in) :: key(0:)
              integer(int32), intent(in) :: seed
              integer(int32)             :: hash_value
          end function int64_nmhash32x


        elemental module function character_nmhash32x( key, seed ) &
            result(hash_value)
!! NMHASH32 hash function for default character string keys
            character(*), intent(in)      :: key
            integer(int32), intent(in) :: seed
            integer(int32) :: hash_value
        end function character_nmhash32x

    end interface nmhash32x

    interface water_hash
!! Version: experimental
!!
!! WATER_HASH interfaces
!! ([Specification](../page/specs/stdlib_hash_procedures.html#water_hash-calculates-a-hash-code-from-a-key-and-a-seed))
          pure module function int8_water_hash( key, seed ) &
              result(hash_code)
!! WATER HASH function for rank 1 array keys of kind int8
              integer(int8), intent(in) :: key(0:)
              integer(int64), intent(in)  :: seed
              integer(int_hash)           :: hash_code
          end function int8_water_hash
          pure module function int16_water_hash( key, seed ) &
              result(hash_code)
!! WATER HASH function for rank 1 array keys of kind int16
              integer(int16), intent(in) :: key(0:)
              integer(int64), intent(in)  :: seed
              integer(int_hash)           :: hash_code
          end function int16_water_hash
          pure module function int32_water_hash( key, seed ) &
              result(hash_code)
!! WATER HASH function for rank 1 array keys of kind int32
              integer(int32), intent(in) :: key(0:)
              integer(int64), intent(in)  :: seed
              integer(int_hash)           :: hash_code
          end function int32_water_hash
          pure module function int64_water_hash( key, seed ) &
              result(hash_code)
!! WATER HASH function for rank 1 array keys of kind int64
              integer(int64), intent(in) :: key(0:)
              integer(int64), intent(in)  :: seed
              integer(int_hash)           :: hash_code
          end function int64_water_hash

        elemental module function character_water_hash( key, seed ) &
            result(hash_code)
!! WATER hash function for default character string keys
            character(*), intent(in)   :: key
            integer(int64), intent(in) :: seed
            integer(int_hash)          :: hash_code
        end function character_water_hash

    end interface water_hash

    interface new_water_hash_seed
!! Version: experimental
!!
!! ([Specification](file:///home/jvandenp/stdlib/API-doc/page/specs/stdlib_hash_procedures.html#new_water_hash_seed-returns-a-valid-input-seed-for-water_hash))
        module subroutine new_water_hash_seed( seed )
            integer(int64), intent(inout) :: seed
        end subroutine new_water_hash_seed

    end interface new_water_hash_seed

    interface new_nmhash32_seed
!! Version: experimental
!!
!! ([Specification](../page/specs/stdlib_hash_procedures.html#new_nmhash32_seed-returns-a-valid-input-seed-for-nmhash32)

        module subroutine new_nmhash32_seed( seed )
            integer(int32), intent(inout) :: seed
        end subroutine new_nmhash32_seed

    end interface new_nmhash32_seed

    interface new_nmhash32x_seed
!! Version: experimental
!!
!! ([Specification](../page/specs/stdlib_hash_procedures.html#new_nmhash32x_seed-returns-a-valid-input-seed-for-nmhash32x))

        module subroutine new_nmhash32x_seed( seed )
            integer(int32), intent(inout) :: seed
        end subroutine new_nmhash32x_seed

    end interface new_nmhash32x_seed

contains

    elemental function fibonacci_hash( key, nbits ) result( sample )
!! Version: experimental
!!
!! Maps the 32 bit integer `key` to an unsigned integer value with only `nbits`
!! bits where `nbits` is less than 32
!! ([Specification](../page/specs/stdlib_hash_procedures.html#fibonacci_hash-maps-an-integer-to-a-smaller-number-of-bits))

        integer(int32), intent(in) :: key
        integer, intent(in)        :: nbits
        integer(int32)             :: sample

        sample = ishft( key*pow32_over_phi, -32 + nbits )

    end function fibonacci_hash

    elemental function universal_mult_hash( key, seed, nbits ) result( sample )
!! Version: experimental
!!
!! Uses the "random" odd 32 bit integer `seed` to map the 32 bit integer `key` to
!! an unsigned integer value with only `nbits` bits where `nbits` is less than 32
!! ([Specification](../page/specs/stdlib_hash_procedures.html#universal_mult_hash-maps-an-integer-to-a-smaller-number-of-bits))
        integer(int32), intent(in) :: key
        integer(int32), intent(in) :: seed
        integer, intent(in)        :: nbits
        integer(int32)             :: sample

        sample = ishft( key*seed, -32 + nbits )

    end function universal_mult_hash

    subroutine odd_random_integer( harvest )
!! Version: experimental
!!
!! Returns a 32 bit pseudo random integer, `harvest`, distributed uniformly over
!! the odd integers of the `int32` kind.
!! ([Specification](../page/specs/stdlib_hash_procedures.html#odd_random_integer-returns-an-odd-integer))
        integer(int32), intent(out) :: harvest
        real(dp) :: sample

        call random_number( sample )
        harvest = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, &
            int32 )
        harvest = ishft( harvest, 1 ) + 1_int32

    end subroutine odd_random_integer

end module stdlib_hash_32bit
