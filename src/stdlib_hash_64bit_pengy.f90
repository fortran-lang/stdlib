!!------------------------------------------------------------------------------
!! `PENGY_HASH` is a translation to Fortran 2008 and signed two's complement
!! arithmetic of the `pengyhash` algorithm of Alberto Fajardo, copyright 2020.
!! Alberto Fajardo's original C code, `pengyhash.c`, is available at the URL:
!! https://github.com/tinypeng/pengyhash/blob/master/pengyhash.c
!! under the BSD 2-Clause License:
!! https://github.com/tinypeng/pengyhash/blob/master/LICENSE
!!
!! The BSD 2-Clause license is as follows:
!!
!! BSD 2-Clause License
!!
!! pengyhash
!! Copyright (c) 2020 Alberto Fajardo
!! All rights reserved.
!!
!! Redistribution and use in source and binary forms, with or without
!! modification, are permitted provided that the following conditions are met:
!!
!! 1. Redistributions of source code must retain the above copyright notice,
!!    this list of conditions and the following disclaimer.
!!
!! 2. Redistributions in binary form must reproduce the above copyright notice,
!!    this list of conditions and the following disclaimer in the documentation
!!    and/or other materials provided with the distribution.
!!
!! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
!! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
!! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
!! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
!! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
!! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
!! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
!! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
!! POSSIBILITY OF SUCH DAMAGE.
!!------------------------------------------------------------------------------


submodule(stdlib_hash_64bit) stdlib_hash_64bit_pengy

    implicit none

contains

    pure module function int8_pengy_hash( key, seed ) result(hash_code)
        integer(int64) :: hash_code
        integer(int8), intent(in) :: key(0:)
        integer(int32), intent(in) :: seed

        integer(int64) :: b(0:3)
        integer(int64) :: i
        integer(int64) :: index
        integer(int64) :: len
        integer(int64) :: s(0:3)
        integer(int64) :: seed2
        integer(int8)  :: dummy(0:31)

        b(0:3) = 0_int64
        len = size( key, kind=int64 )
        s(0:3) = [ 0_int64, 0_int64, 0_int64, len ]

        index = 0_int64
        do while ( len >= 32 )
            b(0:3) = transfer( key( index:index+31 ), 0_int64, 4 )

            s(0) = s(0) + s(1) + b(3)
            s(1) = s(0) + ishftc( s(1), 14 )
            s(2) = s(2) + s(3) + b(2)
            s(3) = s(2) + ishftc( s(3), 23 )
            s(0) = s(0) + s(3) + b(1)
            s(3) = ieor( s(0), ishftc( s(3), 16 ) )
            s(2) = s(2) + s(1) + b(0)
            s(1) = ieor( s(2), ishftc( s(1), 40 ) )

            len = len - 32
            index = index + 32
        end do

        dummy(0:31) = transfer( b, 0_int8, 32 )
        dummy(0:len-1) = key(index:index+len-1)
        b(0:3) = transfer( dummy, 0_int64, 4)
        if ( little_endian ) then
            seed2 = transfer( [ seed, 0_int32 ], 0_int64 )
        else
            seed2 = transfer( [ 0_int32, seed ], 0_int64 )
        end if

        do i = 0, 5
            s(0) = s(0) + s(1) + b(3)
            s(1) = s(0) + ishftc( s(1), 14 ) + seed2
            s(2) = s(2) + s(3) + b(2)
            s(3) = s(2) + ishftc( s(3), 23 )
            s(0) = s(0) + s(3) + b(1)
            s(3) = ieor( s(0), ishftc( s(3), 16 ) )
            s(2) = s(2) + s(1) + b(0)
            s(1) = ieor( s(2), ishftc( s(1), 40 ) )
        end do

        hash_code = s(0) + s(1) + s(2) + s(3)

    end function int8_pengy_hash

    pure module function int16_pengy_hash( key, seed ) result(hash_code)
!! PENGY_HASH hash function for rank 1 array keys of kind int16
        integer(int16), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int64)              :: hash_code

        hash_code = int8_pengy_hash( transfer( key, 0_int8, &
                     bytes_int16*size(key, kind=int64) ), seed)

    end function int16_pengy_hash

    pure module function int32_pengy_hash( key, seed ) result(hash_code)
!! PENGY_HASH hash function for rank 1 array keys of kind int32
        integer(int32), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int64)              :: hash_code

        hash_code = int8_pengy_hash( transfer( key, 0_int8, &
                     bytes_int32*size(key, kind=int64) ), seed)

    end function int32_pengy_hash

    pure module function int64_pengy_hash( key, seed ) result(hash_code)
!! PENGY_HASH hash function for rank 1 array keys of kind int64
        integer(int64), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int64)              :: hash_code

        hash_code = int8_pengy_hash( transfer( key, 0_int8, &
                     bytes_int64*size(key, kind=int64) ), seed)

    end function int64_pengy_hash


    elemental module function character_pengy_hash( key, seed ) &
        result(hash_code)
!! PENGY_HASH hash function for default character keys
        character(*), intent(in)   :: key
        integer(int32), intent(in) :: seed
        integer(int64)             :: hash_code

        hash_code = int8_pengy_hash( transfer( key, 0_int8, &
                     bytes_char*len(key, kind=int64) ), seed)

    end function character_pengy_hash

    module subroutine new_pengy_hash_seed( seed )
! Random SEED generator for PENGY_HASH
        integer(int32), intent(inout) :: seed
        real(dp) :: sample
        integer(int32) :: old_seed

        old_seed = seed
        find_seed: do
            call random_number( sample )
            seed = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, &
                int32 )
            if ( seed /= old_seed ) return
        end do find_seed

    end subroutine new_pengy_hash_seed

end submodule stdlib_hash_64bit_pengy
