!!------------------------------------------------------------------------------
!! `WATER_HASH` is a translation to Fortran 2008 of the `waterhash` algorithm
!! of Tommy Ettinger. Tommy Ettinger's original C++ code, `waterhash.h`, is
!! available at the URL: https://github.com/tommyettinger/waterhash under the
!! `unlicense`, https://github.com/tommyettinger/waterhash/blob/master/LICENSE.
!! "`waterhash` is a variant on Wang Yi's `wyhash`, with 32 bit output,
!! using at most 64 bit arithmetic. `wyhash` is available at the URL:
!! `https://github.com/wangyi-fudan/wyhash` also under the unlicense:
!! `https://github.com/wangyi-fudan/wyhash/blob/master/LICENSE`.
!! Original Author: Wang Yi <godspeed_china@yeah.net>
!! Waterhash Variant Author: Tommy Ettinger <tommy.ettinger@gmail.com>
!!
!! The `unlicense` reads as follows:
!!   This is free and unencumbered software released into the public domain.
!!
!!   Anyone is free to copy, modify, publish, use, compile, sell, or
!!   distribute this software, either in source code form or as a compiled
!!   binary, for any purpose, commercial or non-commercial, and by any
!!   means.
!!
!!   In jurisdictions that recognize copyright laws, the author or authors
!!   of this software dedicate any and all copyright interest in the
!!   software to the public domain. We make this dedication for the benefit
!!   of the public at large and to the detriment of our heirs and
!!   successors. We intend this dedication to be an overt act of
!!   relinquishment in perpetuity of all present and future rights to this
!!   software under copyright law.
!!
!!   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!!   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!!   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
!!   IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
!!   OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
!!   ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
!!   OTHER DEALINGS IN THE SOFTWARE.
!!
!!   For more information, please refer to <http://unlicense.org>
!!
!! `WATER_HASH` is distributed as part of the `stdlib_32_bit_hash_functions.f90`
!! module and its `stdlib_hash_32bit_water.f90` submodule with the Fortran
!! Standard Library at URL: https://github.com/fortran-lang/stdlib.
!! The Fortran Standard Library, including this code, is distributed under the
!! MIT License as described in the `LICENSE` file distributed with the library.
!! `WATER_HASH` differs from `waterhash.h` not only in its use of Fortran,
!! but also in its use of signed two's complement arithmetic in contrast to
!! the unsigned arithmetic of Ettinger and Wang Yi, and in making some of the
!! uses of `TRANSFER` endian dependent, in an attempt to make the quality of
!! the hash endian independent.
!!
!! To be useful this code must be processed by a processor that implements two
!! Fortran 2008 extensions to Fortran 2003: submodules, and 64 bit (`INT64`)
!! integers. The processor must also use two's complement integers
!! (all Fortran 95+ processors use two's complement arithmetic) with
!! wrap around overflow at runtime and for BOZ constants. The latest releases
!! of the following processors are known to implement the required Fortran
!! 2008 extensions and default to runtime wrap around overflow: FLANG,
!! gfortran, ifort, and NAG Fortran. Older versions of gfortran will require
!! the compiler flag, `-fno-range-check`, to ensure wrap around semantics
!! for BOZ constants, and only versions of the NAG compiler starting with
!! version 17, have implemented submodules. The latest releases of Cray
!! Fortran and IBM Fortran are known to implement the Fortran 2008 extensions,
!! but whether they also implement wrap around overflow is unknown.
!!
!! This implementation has only been tested on little endian processors. It
!! will generate different hashes on big endian processors, but they are
!! believed to be of comparable quality to those generated for little endian
!! processors.
!!
!! No version of this hash is suitable as a cryptographic hash.
!!------------------------------------------------------------------------------


submodule(stdlib_hash_32bit) stdlib_hash_32bit_water
    implicit none

contains

    pure module function int8_water_hash( key, seed ) result(hash_code)
        integer(int32)             :: hash_code
        integer(int8), intent(in)  :: key(0:)
        integer(int64), intent(in) :: seed

        integer(int32) :: dummy(2)
        integer(int64) :: h
        integer(int64) :: i
        integer(int64) :: len
        integer(int64), parameter ::                &
            waterp0 = int(z'a0761d65', kind=int64), &
            waterp1 = int(z'e7037ed1', kind=int64), &
            waterp2 = int(z'8ebc6af1', kind=int64), &
            waterp3 = int(z'589965cd', kind=int64), &
            waterp4 = int(z'1d8e4e27', kind=int64), &
            waterp5 = int(z'eb44accb', kind=int64)

        len = size(key, kind=int64)
        h = seed
        do i = 0_int64, len-16, 16
            h = watermum(watermum(ieor(waterr32(key(i:)),waterp1),        &
                                  ieor(waterr32(key(i+4:)),waterp2)) + h, &
                         watermum(ieor(waterr32(key(i+8:)),waterp3),      &
                                  ieor(waterr32(key(i+12:)),waterp4)))
        end do
        h = h + waterp5

        select case( iand(len, 15_int64) )
        case(1)
            h = watermum(ieor(waterp2, h),               &
                         ieor(waterr08(key(i:)), waterp1))
        case(2)
            h = watermum(ieor(waterp3, h),               &
                         ieor(waterr16(key(i:)), waterp4))
        case(3)
            h = watermum(ieor(waterr16(key(i:)), h),        &
                         ieor(waterr08(key(i+2:)), waterp2))
        case(4)
            h = watermum(ieor(waterr16(key(i:)), h),        &
                         ieor(waterr16(key(i+2:)), waterp3))
        case(5)
            h = watermum(ieor(waterr32(key(i:)), h),        &
                         ieor(waterr08(key(i+4:)), waterp1))
        case(6)
            h = watermum(ieor(waterr32(key(i:)), h),        &
                         ieor(waterr16(key(i+4:)), waterp1))
        case(7)
            h = watermum(ieor(waterr32(key(i:)), h),             &
                         ieor(ior(ishft(waterr16(key(i+4:)), 8), &
                                  waterr08(key(i+6:))), waterp1))
        case(8)
            h = watermum(ieor(waterr32(key(i:)), h),        &
                         ieor(waterr32(key(i+4:)), waterp0))
        case(9)
            h = ieor(watermum(ieor(waterr32(key(i:)), h),          &
                              ieor(waterr32(key(i+4:)), waterp2)), &
                     watermum(ieor(h, waterp4),                    &
                              ieor(waterr08(key(i+8:)), waterp3)))
        case(10)
            h = ieor(watermum(ieor(waterr32(key(i:)), h),            &
                              ieor(waterr32(key(i+4:)), waterp2)),   &
                     watermum(h, ieor(waterr16(key(i+8:)), waterp3)))
        case(11)
            h = ieor(watermum(ieor(waterr32(key(i:)), h),            &
                              ieor(waterr32(key(i+4:)), waterp2)),   &
                     watermum(h,                                     &
                              ieor(ior(ishft(waterr16(key(i+8:)),8), &
                                       waterr08(key(i+10:))),        &
                                   waterp3)))
        case(12)
            h = ieor(watermum(ieor(waterr32(key(i:)), h),          &
                              ieor(waterr32(key(i+4:)), waterp2)), &
                     watermum(ieor(h, waterr32(key(i+8:))),        &
                                      waterp4))
        case(13)
            h = ieor(watermum(ieor(waterr32(key(i:)), h),            &
                              ieor(waterr32(key(i+4:)), waterp2)),   &
                     watermum(ieor(h, waterr32(key(i+8:))),          &
                              ieor(waterr08(key(i+12:)), waterp4)))
        case(14)
            h = ieor(watermum(ieor(waterr32(key(i:)), h),            &
                              ieor(waterr32(key(i+4:)), waterp2)),   &
                     watermum(ieor(h, waterr32(key(i+8:))),          &
                              ieor(waterr16(key(i+12:)), waterp4)))
        case(15)
            h = ieor(watermum(ieor(waterr32(key(i:)), h),             &
                              ieor(waterr32(key(i+4:)), waterp2)),    &
                     watermum(ieor(h, waterr32(key(i+8:))),           &
                              ieor(ior(ishft(waterr16(key(i+12:)),8), &
                                       waterr08(key(i+14:))),         &
                                   waterp4)))
        end select

        h = ieor( h, ishft(h,16) ) * ieor( len, waterp0 )
        h = h - ishft( h, -32 )
        dummy(1:2) = transfer(h, dummy, 2)
        if (little_endian) then
            hash_code = dummy(1)
        else
            hash_code = dummy(2)
        end if

    contains

        pure function watermum( a, b ) result(r)
            integer(int64)             :: r
            integer(int64), intent(in) :: a, b

            r = a * b
            r = r - ishft(r, -32)

        end function watermum

        pure function waterr08( p ) result(v)
            integer(int64)            :: v
            integer(int8), intent(in) :: p(:)

            if (little_endian) then
                v = transfer( [ p(1), 0_int8, 0_int8, 0_int8,       &
                                0_int8, 0_int8, 0_int8, 0_int8 ], v )
            else
                v = transfer( [ 0_int8, 0_int8, 0_int8, 0_int8,   &
                                0_int8, 0_int8, 0_int8, p(1) ], v )
            end if

        end function waterr08

        pure function waterr16( p ) result(v)
            integer(int64)            :: v
            integer(int8), intent(in) :: p(:)

            if (little_endian) then
                v = transfer( [ p(1), p(2), 0_int8, 0_int8,         &
                                0_int8, 0_int8, 0_int8, 0_int8 ], v )
            else
                v = transfer( [ 0_int8, 0_int8, 0_int8, 0_int8,  &
                                0_int8, 0_int8, p(2), p(1) ], v )
            end if

        end function waterr16

        pure function waterr32( p ) result(v)
            integer(int64)            :: v
            integer(int8), intent(in) :: p(:)

            if (little_endian) then
                v = transfer( [ p(1), p(2), p(3), p(4),             &
                                0_int8, 0_int8, 0_int8, 0_int8 ], v )
            else
                v = transfer( [ 0_int8, 0_int8, 0_int8, 0_int8, &
                                p(4), p(3), p(2), p(1) ], v )
            end if

        end function waterr32

    end function int8_water_hash


    pure module function int16_water_hash( key, seed ) result(hash_code)
        integer(int16), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed
        integer(int_hash)           :: hash_code

        hash_code = int8_water_hash( transfer( key, 0_int8, &
                     bytes_int16*size(key, kind=int64) ), seed)

    end function int16_water_hash

    pure module function int32_water_hash( key, seed ) result(hash_code)
        integer(int32), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed
        integer(int_hash)           :: hash_code

        hash_code = int8_water_hash( transfer( key, 0_int8, &
                     bytes_int32*size(key, kind=int64) ), seed)

    end function int32_water_hash

    pure module function int64_water_hash( key, seed ) result(hash_code)
        integer(int64), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed
        integer(int_hash)           :: hash_code

        hash_code = int8_water_hash( transfer( key, 0_int8, &
                     bytes_int64*size(key, kind=int64) ), seed)

    end function int64_water_hash


    elemental module function character_water_hash( key, seed ) &
        result(hash_code)
        character(*), intent(in)   :: key
        integer(int64), intent(in) :: seed
        integer(int_hash)          :: hash_code

        hash_code = int8_water_hash( transfer( key, 0_int8, &
                     bytes_char*len(key, kind=int64) ), seed)

    end function character_water_hash

    module subroutine new_water_hash_seed( seed )
        integer(int64), intent(inout) :: seed

        integer(int64) :: old_seed

        real(dp) :: sample(2)
        integer(int32) :: part(2)

        old_seed = seed
        find_seed:do
            call random_number( sample )
            part = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, &
                int32 )
            seed = transfer( part, seed )
            if ( seed /= old_seed ) return
        end do find_seed

    end subroutine new_water_hash_seed

end submodule stdlib_hash_32bit_water
