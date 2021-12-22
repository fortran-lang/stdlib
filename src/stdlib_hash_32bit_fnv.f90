!!------------------------------------------------------------------------------
!! `FNV_1_HASH` and  `FNV_1A_Hash` are translations to Fortran 2008 of the
!! `FNV-1` and `FNV-1a` hash functions of Glenn Fowler, Landon Curt Noll,
!! and Phong Vo, that has been released into the public domain. Permission
!! has been granted, by Landon Curt Noll, for the use of these algorithms
!! in the Fortran Standard Library. A description of these functions is
!! available at https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function.
!!------------------------------------------------------------------------------

!#! Integer kinds to be considered during templating

submodule(stdlib_hash_32bit) stdlib_hash_32bit_fnv
!! An implementation of the FNV hashes 1 and 1a of Glenn Fowler, Landon Curt
!! Noll, and Kiem-Phong-Vo,
!! https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
    implicit none

    integer(int_hash), parameter ::                  &
        offset_basis = int( z'811C9DC5', int_hash ), &
        prime        = int( z'01000193', int_hash )

contains

    pure module function int8_fnv_1( key ) result(hash_code)
!! The original FNV-1 8-bit key algorithm.
        integer(int8), intent(in)     :: key(:)
        integer(int_hash)             :: hash_code

        integer(int64) :: i

        hash_code = offset_basis
        do i=1_int64, size(key, kind=int64)
            hash_code = hash_code * prime
            if ( little_endian ) then
                hash_code = ieor( hash_code, &
                                  transfer( [key(i), 0_int8, 0_int8, 0_int8], &
                                            0_int_hash ) )
            else
                hash_code = ieor( hash_code, &
                                  transfer( [0_int8, 0_int8, 0_int8, key(i)], &
                                            0_int_hash ) )
            end if
        end do

    end function int8_fnv_1


    pure module function int16_fnv_1( key ) result(hash_code)
! A int16 array key wrapper for the FNV-1 algorithm.
        integer(int16), intent(in) :: key(:)
        integer(int_hash)           :: hash_code

        hash_code = int8_fnv_1( transfer( key, 0_int8,                      &
                                          bytes_int16*                     &
                                          size( key, kind=int64 ) ) )

    end function int16_fnv_1

    pure module function int32_fnv_1( key ) result(hash_code)
! A int32 array key wrapper for the FNV-1 algorithm.
        integer(int32), intent(in) :: key(:)
        integer(int_hash)           :: hash_code

        hash_code = int8_fnv_1( transfer( key, 0_int8,                      &
                                          bytes_int32*                     &
                                          size( key, kind=int64 ) ) )

    end function int32_fnv_1

    pure module function int64_fnv_1( key ) result(hash_code)
! A int64 array key wrapper for the FNV-1 algorithm.
        integer(int64), intent(in) :: key(:)
        integer(int_hash)           :: hash_code

        hash_code = int8_fnv_1( transfer( key, 0_int8,                      &
                                          bytes_int64*                     &
                                          size( key, kind=int64 ) ) )

    end function int64_fnv_1



    elemental module function character_fnv_1( key ) result(hash_code)
! A default character key wrapper for the FNV-1 algorithm.
        character(*), intent(in)      :: key
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1( transfer( key,                           &
                                          0_int8,                        &
                                          bytes_char*                    &
                                          len(key, kind=int64) ) )

    end function character_fnv_1


    pure module function int8_fnv_1a( key ) result(hash_code)
!! The original FNV-1a 8-bit key algorithm.
        integer(int8), intent(in)     :: key(:)
        integer(int_hash)             :: hash_code

        integer(int64) :: i

        hash_code = offset_basis
        do i=1_int64, size(key, kind=int64)
            if ( little_endian ) then
                hash_code = ieor( hash_code, &
                                  transfer( [key(i), 0_int8, 0_int8, 0_int8],  &
                                            0_int_hash ) )
            else
                hash_code = ieor( hash_code, &
                                  transfer( [0_int8, 0_int8, 0_int8, key(i)], &
                                            0_int_hash ) )
            end if
            hash_code = hash_code * prime
        end do

    end function int8_fnv_1a


    pure module function int16_fnv_1a( key ) result(hash_code)
! A int16 array key wrapper for the FNV-1a algorithm.
        integer(int16), intent(in)   :: key(:)
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a( transfer( key, 0_int8,                   &
                                           bytes_int16*                  &
                                           size(key, kind=int64)) )

    end function int16_fnv_1a

    pure module function int32_fnv_1a( key ) result(hash_code)
! A int32 array key wrapper for the FNV-1a algorithm.
        integer(int32), intent(in)   :: key(:)
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a( transfer( key, 0_int8,                   &
                                           bytes_int32*                  &
                                           size(key, kind=int64)) )

    end function int32_fnv_1a

    pure module function int64_fnv_1a( key ) result(hash_code)
! A int64 array key wrapper for the FNV-1a algorithm.
        integer(int64), intent(in)   :: key(:)
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a( transfer( key, 0_int8,                   &
                                           bytes_int64*                  &
                                           size(key, kind=int64)) )

    end function int64_fnv_1a


    elemental module function character_fnv_1a( key ) result(hash_code)
! A default character key wrapper for the FNV-1 algorithm.
        character(*), intent(in)      :: key
        integer(int_hash)             :: hash_code

        hash_code = int8_fnv_1a( transfer( key, 0_int8,                   &
                                           (bits_char/bits_int8)*         &
                                           len(key, kind=int64) ) )

    end function character_fnv_1a

end submodule stdlib_hash_32bit_fnv
