
module stdlib_hash_64bit

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
        int_hash     = int64
!! The number of bits in the output hash

! The number of bits used by each integer type
    integer, parameter, public ::       &
! Should be 8
        bits_int8  = bit_size(0_int8),  &
! Should be 16
        bits_int16 = bit_size(0_int16), &
! Should be 32
        bits_int32 = bit_size(0_int32), &
! Should be 64
        bits_int64 = bit_size(0_int64)

    integer, parameter, public ::       &
! Should be 1
        bytes_int8  = bits_int8/bits_int8,  &
! Should be 2
        bytes_int16 = bits_int16/bits_int8, &
! Should be 4
        bytes_int32 = bits_int32/bits_int8, &
! Should be 8
        bytes_int64 = bits_int64/bits_int8

    integer, parameter, public :: &
        bits_char = character_storage_size, &
        bytes_char = bits_char/bits_int8

! Dealing with different endians
    logical, parameter, public ::                                    &
        little_endian = ( 1 == transfer( [1_int8, 0_int8], 0_int16) )

    public ::                     &
        fibonacci_hash,           &
        fnv_1_hash,               &
        fnv_1a_hash,              &
        new_pengy_hash_seed,      &
        new_spooky_hash_seed,     &
        odd_random_integer,       &
        pengy_hash,               &
        spooky_hash,              &
        spookyhash_128,           &
        universal_mult_hash

! pow64_over_phi is the odd number that most closely approximates 2**64/phi,
! where phi is the golden ratio 1.618...
    integer(int64), parameter ::                        &
        pow64_over_phi = int(z'9E3779B97F4A7C15', int64)

    integer(int_hash), parameter :: &
        two_32 = 2_int_hash**32

! constants used by Bob Jenkins' SpookyHash
    integer(int32), parameter ::                            &
        sc_numvars = 12,                                    &
        sc_blocksize = sc_numvars*8,                        &
        sc_buffsize = 2*sc_blocksize,                       &
        sc_constsub = int(z'deadbeef', int32)
        ! twos complement "deadbeef"

    integer(int64), parameter ::                                  &
        sc_const = transfer( [sc_constsub, sc_constsub], 0_int64 )

    type :: spooky_subhash
        integer(int8)  :: data(0:2*sc_blocksize-1)
        integer(int64) :: state(0:sc_numvars-1)
        integer(int64) :: length
        integer(int16) :: remainder
    end type spooky_subhash

    interface fnv_1_hash
!! Version: experimental
!!
!! FNV_1 interfaces
!! ([Specification](../page/specs/stdlib_hash_procedures.html#fnv_1-calculates-a-hash-code-from-a-key))
          pure module function int8_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 arrays of kind int8
              integer(int8), intent(in) :: key(:)
              integer(int_hash)              :: hash_code
          end function int8_fnv_1
          pure module function int16_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 arrays of kind int16
              integer(int16), intent(in) :: key(:)
              integer(int_hash)              :: hash_code
          end function int16_fnv_1
          pure module function int32_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 arrays of kind int32
              integer(int32), intent(in) :: key(:)
              integer(int_hash)              :: hash_code
          end function int32_fnv_1
          pure module function int64_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for rank 1 arrays of kind int64
              integer(int64), intent(in) :: key(:)
              integer(int_hash)              :: hash_code
          end function int64_fnv_1

        elemental module function character_fnv_1( key ) result(hash_code)
!! FNV_1 hash function for character strings
            character(*), intent(in)   :: key
            integer(int_hash)             :: hash_code
        end function character_fnv_1

    end interface fnv_1_hash


    interface fnv_1a_hash
!! Version: experimental
!!
!! FNV_1A interfaces
!! ([Specification](../page/specs/stdlib_hash_procedures.html#fnv_1a-calculates-a-hash-code-from-a-key))
          pure module function int8_fnv_1a( key ) result(hash_code)
!! FNV_1A hash function for rank 1 arrays of kind int8
              integer(int8), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int8_fnv_1a
          pure module function int16_fnv_1a( key ) result(hash_code)
!! FNV_1A hash function for rank 1 arrays of kind int16
              integer(int16), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int16_fnv_1a
          pure module function int32_fnv_1a( key ) result(hash_code)
!! FNV_1A hash function for rank 1 arrays of kind int32
              integer(int32), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int32_fnv_1a
          pure module function int64_fnv_1a( key ) result(hash_code)
!! FNV_1A hash function for rank 1 arrays of kind int64
              integer(int64), intent(in) :: key(:)
              integer(int_hash)           :: hash_code
          end function int64_fnv_1a

        elemental module function character_fnv_1a( key ) result(hash_code)
!! FNV_1A hash function for character strings
            character(*), intent(in)   :: key
             integer(int_hash)         :: hash_code
        end function character_fnv_1a

    end interface fnv_1a_hash

    interface spooky_hash
!! Version: experimental
!!
!! SPOOKY_HASH interfaces
!!([Specification](../page/specs/stdlib_hash_procedures.html#spooky_hash-maps-a-character-string-or-integer-vector-to-an-integer))
           module function int8_spooky_hash( key, seed ) &
              result(hash_code)
!! SPOOKY HASH function for rank 1 arrays of kind int8
              integer(int8), intent(in) :: key(0:)
              integer(int_hash), intent(in)  :: seed(2)
              integer(int_hash) :: hash_code(2)
          end function int8_spooky_hash
           module function int16_spooky_hash( key, seed ) &
              result(hash_code)
!! SPOOKY HASH function for rank 1 arrays of kind int16
              integer(int16), intent(in) :: key(0:)
              integer(int_hash), intent(in)  :: seed(2)
              integer(int_hash) :: hash_code(2)
          end function int16_spooky_hash
           module function int32_spooky_hash( key, seed ) &
              result(hash_code)
!! SPOOKY HASH function for rank 1 arrays of kind int32
              integer(int32), intent(in) :: key(0:)
              integer(int_hash), intent(in)  :: seed(2)
              integer(int_hash) :: hash_code(2)
          end function int32_spooky_hash
           module function int64_spooky_hash( key, seed ) &
              result(hash_code)
!! SPOOKY HASH function for rank 1 arrays of kind int64
              integer(int64), intent(in) :: key(0:)
              integer(int_hash), intent(in)  :: seed(2)
              integer(int_hash) :: hash_code(2)
          end function int64_spooky_hash

         module function character_spooky_hash( key, seed ) &
            result(hash_code)
!! SPOOKY hash function for character strings
            character(*), intent(in)    :: key
            integer(int_hash), intent(in)  :: seed(2)
            integer(int_hash) :: hash_code(2)
        end function character_spooky_hash

    end interface spooky_hash

    interface

         module subroutine spookyHash_128( key, hash_inout )
!! Version: experimental
!!
            integer(int8), intent(in), target :: key(0:)
            integer(int_hash), intent(inout)  :: hash_inout(2)
        end subroutine spookyHash_128

    end interface


    interface spooky_init
!! Version: experimental
!!

         pure module subroutine spookysubhash_init( self, seed )
            type(spooky_subhash), intent(out) :: self
            integer(int_hash), intent(in)     :: seed(2)
        end subroutine spookysubhash_init

    end interface spooky_init


    interface spooky_update

         module subroutine spookyhash_update( spooky, key )
!! Version: experimental
!!
            type(spooky_subhash), intent(inout) :: spooky
            integer(int8), intent(in)         :: key(0:)
        end subroutine spookyhash_update

    end interface spooky_update


    interface spooky_final

         module subroutine spookyhash_final(spooky, hash_code)
!! Version: experimental
!!
            type(spooky_subhash), intent(inout) :: spooky
            integer(int_hash), intent(inout)    :: hash_code(2)
        end subroutine spookyhash_final

    end interface spooky_final

interface

        module subroutine new_spooky_hash_seed( seed )
!! Version: experimental
!!
!! Random seed generator for SPOOKY_HASH
            integer(int64), intent(inout) :: seed(2)
        end subroutine new_spooky_hash_seed

    end interface

    interface pengy_hash
!! Version: experimental
!!
!! PENGY_HASH interfaces
!! ([Specification](../page/specs/stdlib_hash_procedures.html#pengy_hash-maps-a-character-string-or-integer-vector-to-an-integer))
    pure module function int8_pengy_hash( key, seed ) result(hash_code)
!! PENGY_HASH hash function for rank 1 array keys of kind int8
        integer(int8), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int64)           :: hash_code
    end function int8_pengy_hash
    pure module function int16_pengy_hash( key, seed ) result(hash_code)
!! PENGY_HASH hash function for rank 1 array keys of kind int16
        integer(int16), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int64)           :: hash_code
    end function int16_pengy_hash
    pure module function int32_pengy_hash( key, seed ) result(hash_code)
!! PENGY_HASH hash function for rank 1 array keys of kind int32
        integer(int32), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int64)           :: hash_code
    end function int32_pengy_hash
    pure module function int64_pengy_hash( key, seed ) result(hash_code)
!! PENGY_HASH hash function for rank 1 array keys of kind int64
        integer(int64), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int64)           :: hash_code
    end function int64_pengy_hash

        elemental module function character_pengy_hash( key, seed ) &
            result(hash_code)
!! MIR HASH STRICT function for character strings
            character(*), intent(in)      :: key
            integer(int32), intent(in) :: seed
            integer(int64)             :: hash_code
        end function character_pengy_hash

    end interface pengy_hash

    interface

        module subroutine new_pengy_hash_seed( seed )
!! Version: experimental
!!
!! Random seed generator for MIR_HASH_STRICT
            integer(int32), intent(inout) :: seed
        end subroutine new_pengy_hash_seed

    end interface

contains

    elemental function fibonacci_hash( key, nbits ) result( sample )
!! Version: experimental
!!
!! Maps the 64 bit integer `key` to an unsigned integer value with only `nbits`
!! bits where `nbits` is less than 64
!! ([Specification](../page/specs/stdlib_hash_procedures.html#fibonacci_hash-maps-an-integer-to-a-smaller-number-of-bits_1))

        integer(int64), intent(in) :: key
        integer, intent(in)        :: nbits
        integer(int64)             :: sample

        sample = ishft( key*pow64_over_phi, -64 + nbits )

    end function fibonacci_hash

    elemental function universal_mult_hash( key, seed, nbits ) result( sample )
!! Version: experimental
!!
!! Uses the "random" odd 64 bit integer `seed` to map the 64 bit integer `key` to
!! an unsigned integer value with only `nbits` bits where `nbits` is less than 64.
!! ([Specification](../page/specs/stdlib_hash_procedures.html#universal_mult_hash-maps-an-integer-to-a-smaller-number-of-bits_1))

        integer(int64), intent(in) :: key
        integer(int64), intent(in) :: seed
        integer, intent(in)        :: nbits
        integer(int64)             :: sample

        sample = ishft( key*seed, -64 + nbits )

    end function universal_mult_hash

    subroutine odd_random_integer( harvest )
!! Version: experimental
!!
!! Returns a 64 bit pseudo random integer, `harvest`, distributed uniformly over
!! the odd integers of the 64 bit kind.
!! ([Specification](../page/specs/stdlib_hash_procedures.html#odd_random_integer-returns-odd-integer))

        integer(int64), intent(out) :: harvest
        real(dp) :: sample(2)
        integer(int32) :: part(2)

        call random_number( sample )
        part = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, int32 )
        harvest = transfer( part, harvest )
        harvest = ishft( harvest, 1 ) + 1_int64

    end subroutine odd_random_integer

    subroutine random_integer( harvest )
!! Version: experimental
!!
!! Returns a 64 bit pseudo random integer, `harvest`, distributed uniformly over
!! the values of the 64 bit kind.
        integer(int64), intent(out) :: harvest
        real(dp) :: sample(2)
        integer(int32) :: part(2)

        call random_number( sample )
        part = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, int32 )
        harvest = transfer( part, harvest )

    end subroutine random_integer

end module stdlib_hash_64bit
