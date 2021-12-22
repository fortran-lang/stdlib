!!------------------------------------------------------------------------------
!! `NM_HASH32` and `NM_HASH32X` are translations to Fortran 2008 and signed
!! two's complement arithmetic of the `nmhash32` and `nmhash32x` scalar
!! algorithms of James Z. M. Gao, copyright 2021. James Z. M. Gao's original
!! C++ code, `nmhash.h`, is available at the URL:
!! https://github.com/gzm55/hash-garage/blob/a8913138bdb3b7539c202edee30a7f0794bbd835/nmhash.h
!! under the BSD 2-Clause License:
!! https://github.com/gzm55/hash-garage/blob/a8913138bdb3b7539c202edee30a7f0794bbd835/LICENSE
!! The algorithms come in multiple versions, depending on whether the
!! vectorized instructions SSE2 or AVX2 are available. As neither instruction
!! is available in portable Fortran 2008, the algorithms that do not use these
!! instructions are used.
!!
!! The BSD 2-Clause license is as follows:
!!
!! BSD 2-Clause License
!!
!! Copyright (c) 2021, water hash algorithm. James Z.M. Gao
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


submodule(stdlib_hash_32bit) stdlib_hash_32bit_nm

    implicit none

! Primes from XXH
    integer(int32), parameter :: nmh_prime32_1 = int( Z'9E3779B1', int32 )
    integer(int32), parameter :: nmh_prime32_2 = int( Z'85EBCA77', int32 )
    integer(int32), parameter :: nmh_prime32_3 = int( Z'C2B2AE3D', int32 )
    integer(int32), parameter :: nmh_prime32_4 = int( Z'27D4EB2F', int32 )

    integer(int32), parameter :: nmh_m1 = int(z'F0D9649B', int32 )
    integer(int32), parameter :: nmh_m2 = int(z'29A7935D', int32 )
    integer(int32), parameter :: nmh_m3 = int(z'55D35831', int32 )

    integer(int32), parameter :: nmh_m1_v(0:31) = nmh_m1
    integer(int32), parameter :: nmh_m2_v(0:31) = nmh_m2
    integer(int32), parameter :: nmh_m3_v(0:31) = nmh_m3

    logical, parameter :: nmh_short32_without_seed2=.false.
    logical, parameter :: nmh_short32_with_seed2=.true.

    integer, parameter :: init_size = 32

! Pseudorandom secrets taken directly from FARSH.
    integer(int32), parameter :: nmh_acc_init(0:init_size-1) = [ &
        int( z'B8FE6C39', int32 ), int( z'23A44BBE', int32 ), &
        int( z'7C01812C', int32 ), int( z'F721AD1C', int32 ), &
        int( z'DED46DE9', int32 ), int( z'839097DB', int32 ), &
        int( z'7240A4A4', int32 ), int( z'B7B3671F', int32 ), &
        int( z'CB79E64E', int32 ), int( z'CCC0E578', int32 ), &
        int( z'825AD07D', int32 ), int( z'CCFF7221', int32 ), &
        int( z'B8084674', int32 ), int( z'F743248E', int32 ), &
        int( z'E03590E6', int32 ), int( z'813A264C', int32 ), &

        int( z'3C2852BB', int32 ), int( z'91C300CB', int32 ), &
        int( z'88D0658B', int32 ), int( z'1B532EA3', int32 ), &
        int( z'71644897', int32 ), int( z'A20DF94E', int32 ), &
        int( z'3819EF46', int32 ), int( z'A9DEACD8', int32 ), &
        int( z'A8FA763F', int32 ), int( z'E39C343F', int32 ), &
        int( z'F9DCBBC7', int32 ), int( z'C70B4F1D', int32 ), &
        int( z'8A51E04B', int32 ), int( z'CDB45931', int32 ), &
        int( z'C89F7EC9', int32 ), int( z'D9787364', int32 ) ]

contains

    pure function nmh_readle32( p ) result( v )
        integer(int32) :: v
        integer(int8), intent(in) :: p(:)

        if ( little_endian ) then
            v = transfer( p(1:4), 0_int32 )
        else
            v = transfer( [ p(4), p(3), p(2), p(1) ], 0_int32 )
        end if

    end function nmh_readle32

    pure function nmh_readle16( p ) result( v )
        integer(int16) :: v
        integer(int8), intent(in) :: p(:)

        if ( little_endian ) then
            v = transfer( p(1:2), 0_int16 )
        else
            v = transfer( [ p(2), p(1) ], 0_int16 )
        end if

    end function nmh_readle16

    pure function nmhash32_0to8( x, seed ) result( vx32 )
        integer(int32), intent(in) :: x
        integer(int32), intent(in) :: seed
        integer(int32) :: vx32
        ! base mixer: [-6 -12 776bf593 -19 11 3fb39c65 -15 -9 e9139917 -11 16]
        ! = 0.027071104091278835
        integer(int32), parameter :: m1 = int(z'776BF593', int32)
        integer(int32), parameter :: m2 = int(z'3FB39C65', int32)
        integer(int32), parameter :: m3 = int(z'E9139917', int32)

        integer(int16) :: vx16(2)

        vx32 = x
        vx32 = ieor( vx32, ieor( ishft( vx32, -12 ), ishft( vx32, -6 ) ) )
        vx16 = transfer( vx32, 0_int16, 2 )
        vx16 = vx16 * transfer( m1, 0_int16, 2 )
        vx32 = transfer( vx16, 0_int32 )
        vx32 = ieor( vx32, ieor( ishft( vx32, 11 ), ishft( vx32, -19 ) ) )
        vx16 = transfer( vx32, 0_int16, 2 )
        vx16 = vx16 * transfer( m2, 0_int16, 2 )
        vx32 = transfer( vx16, 0_int32 )
        vx32 = ieor( vx32, seed )
        vx32 = ieor( vx32, ieor( ishft( vx32, -15 ), ishft( vx32, -9 ) ) )
        vx16 = transfer( vx32, 0_int16, 2 )
        vx16 = vx16 * transfer( m3, 0_int16, 2 )
        vx32 = transfer( vx16, 0_int32 )
        vx32 = ieor( vx32, ieor( ishft(vx32, 16), ishft(vx32, -11) ) )

    end function nmhash32_0to8

    pure function nmhash32_9to255( p, seed, full_avalanche ) result( result )
        integer(int8), intent(in)  :: p(0:)
        integer(int32), intent(in) :: seed
        logical, intent(in)        :: full_avalanche
        integer(int32) :: result

        integer(int32) :: xu32(0:3), yu32(0:3)
        integer(int16) :: xu16(0:1)
!       Due to an issue with Intel OneAPI ifort 2021 (see  
!       https://community.intel.com/t5/Intel-Fortran-Compiler/Intrinsic-transfer-with-a-provided-size-un-expected-behavior/m-p/1343313#M158733
!       ), it is not possible to define the following variables as a parameter.
!        integer(int16), parameter :: &
!            nmh_m1_16(0:1) = transfer( nmh_m1, 0_int16, 2 ),  &
!            nmh_m2_16(0:1) = transfer( nmh_m2, 0_int16, 2 ),  &
!            nmh_m3_16(0:1) = transfer( nmh_m3, 0_int16, 2 )
        integer(int16) :: nmh_m1_16(0:1), nmh_m2_16(0:1), nmh_m3_16(0:1)
        integer(int32) :: s1
        integer(int64) :: length
        integer(int32) :: length32(0:1)
        integer(int64) :: i, j, r

        nmh_m1_16(0:1) = transfer( nmh_m1, 0_int16, 2 )
        nmh_m2_16(0:1) = transfer( nmh_m2, 0_int16, 2 )
        nmh_m3_16(0:1) = transfer( nmh_m3, 0_int16, 2 )

        ! base mixer: [f0d9649b  5 -13 29a7935d -9 11 55d35831 -20 -10 ] =
        ! 0.93495901789135362

        result = 0
        length = size( p, kind=int64 )
        length32 = transfer(length, 0_int32, 2)
        if (little_endian) then
            s1 = seed + length32(0)
        else
            s1 = seed + length32(1)
        end if
        xu32(0) = nmh_prime32_1
        xu32(1) = nmh_prime32_2
        xu32(2) = nmh_prime32_3
        xu32(3) = nmh_prime32_4
        yu32(:) = s1

        if (full_avalanche) then
            ! 33 to 255 bytes
            r = (length - 1 ) /32
            do i=0, r-1
                do j=0, 3
                    xu32(j) = ieor( xu32(j), nmh_readle32( p(i*32 + j*4: ) ) )
                    yu32(j) = ieor( yu32(j), &
                                    nmh_readle32( p(i*32 + j*4 + 16: ) ) )
                    xu32(j) = xu32(j) + yu32(j)
                    xu16 = transfer( xu32(j), 0_int16, 2 )
                    xu16 = xu16 * nmh_m1_16
                    xu32(j) = transfer( xu16, 0_int32 )
                    xu32(j) = ieor( xu32(j), &
                                    ieor( ishft(xu32(j), 5), &
                                          ishft(xu32(j), -13)) )
                    xu16 = transfer( xu32(j), 0_int16, 2 )
                    xu16 = xu16 * nmh_m2_16
                    xu32(j) = transfer( xu16, 0_int32 )
                    xu32(j) = ieor( xu32(j), yu32(j) )
                    xu32(j) = ieor( xu32(j), &
                                    ieor( ishft(xu32(j), 11), &
                                          ishft(xu32(j), -9) ) )
                    xu16 = transfer( xu32(j), 0_int16, 2 )
                    xu16 = xu16 * nmh_m3_16
                    xu32(j) = transfer( xu16, 0_int32 )
                    xu32(j) = ieor( xu32(j), &
                                    ieor( ishft(xu32(j),-10), &
                                          ishft(xu32(j), -20) ) )
                end do
            end do
            do j=0, 3
                xu32(j) = ieor( xu32(j), &
                                nmh_readle32( p(length - 32 + j*4: ) ) )
                yu32(j) = ieor( yu32(j), &
                                nmh_readle32( p(length - 16 + j*4: ) ) )
            end do
        else
            ! 9 to 32 bytes
            xu32(0) = ieor(xu32(0), nmh_readle32(p(0:)))
            xu32(1) = ieor(xu32(1), nmh_readle32(p(ishft(ishft(length,-4),3):)))
            xu32(2) = ieor(xu32(2), nmh_readle32(p(length-8:)))
            xu32(3) = ieor(xu32(3), &
                           nmh_readle32(p(length-8-ishft(ishft(length,-4),3):)))
            yu32(0) = ieor(yu32(0), nmh_readle32(p(4:)))
            yu32(1) = ieor(yu32(1), &
                      nmh_readle32(p(ishft(ishft(length,-4),3)+4:)))
            yu32(2) = ieor(yu32(2), nmh_readle32(p(length-8+4:)))
            yu32(3) = ieor(yu32(3), &
                           nmh_readle32(p(length - 8 - &
                                        ishft(ishft(length,-4),3)+4:)))
        end if
        do j=0, 3
            xu32(j) = xu32(j) + yu32(j)
            yu32(j) = ieor( yu32(j), ieor(ishft(yu32(j), 17), &
                                          ishft(yu32(j), -6) ) )
            xu16 = transfer( xu32(j), 0_int16, 2 )
            xu16 = xu16 * nmh_m1_16
            xu32(j) = transfer( xu16, 0_int32 )
            xu32(j) = ieor( xu32(j), ieor(ishft(xu32(j), 5), &
                                          ishft(xu32(j), -13) ) )
            xu16 = transfer( xu32(j), 0_int16, 2 )
            xu16 = xu16 * nmh_m2_16
            xu32(j) = transfer( xu16, 0_int32 )
            xu32(j) = ieor( xu32(j), yu32(j) )
            xu32(j) = ieor( xu32(j), ieor(ishft(xu32(j), 11), &
                                          ishft(xu32(j), -9) ) )
            xu16 = transfer( xu32(j), 0_int16, 2 )
            xu16 = xu16 * nmh_m3_16
            xu32(j) = transfer( xu16, 0_int32 )
            xu32(j) = ieor( xu32(j), ieor(ishft(xu32(j), -10), &
                                          ishft(xu32(j), -20) ) )
        end do
        xu32(0) = ieor( xu32(0), nmh_prime32_1 )
        xu32(1) = ieor( xu32(1), nmh_prime32_2 )
        xu32(2) = ieor( xu32(2), nmh_prime32_3 )
        xu32(3) = ieor( xu32(3), nmh_prime32_4 )
        do j=1, 3
            xu32(0) = xu32(0) + xu32(j)
        end do
        xu32(0) = ieor(xu32(0), s1 + ishft(s1, -5) )
        xu16 = transfer( xu32(0), 0_int16, 2 )
        xu16 = xu16 * nmh_m3_16
        xu32(0) = transfer( xu16, 0_int32 )
        xu32(0) = ieor(xu32(0), &
                       ieor(ishft(xu32(0), -10), ishft(xu32(0), -20) ) )
        result = xu32(0)

    end function nmhash32_9to255

    pure function nmhash32_9to32( p, seed ) result( result )
        integer(int8), intent(in)  :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: result

        result = nmhash32_9to255( p, seed, .false. )

    end function nmhash32_9to32

    pure function nmhash32_33to255( p, seed ) result( result )
        integer(int8), intent(in)  :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: result

        result = nmhash32_9to255( p, seed, .true. )

    end function nmhash32_33to255

    pure subroutine nmhash32_long_round( accx, accy, p )
        integer(int32), intent(inout) :: accx(0:)
        integer(int32), intent(inout) :: accy(0:)
        integer(int8), intent(in)     :: p(0:)

        integer(int64), parameter :: nbgroups = init_size
        integer(int64) :: i
        integer(int16) :: dummy1(0:1)
        integer(int16) :: dummy2(0:1)

        do i = 0, nbgroups-1
            accx(i) = ieor( accx(i), nmh_readle32( p(i*4:) ) )
            accy(i) = ieor( accy(i), nmh_readle32( p(i*4+nbgroups*4:) ) )
            accx(i) = accx(i) + accy(i)
            accy(i) = ieor( accy(i), ishft(accx(i),  -1) )
            dummy1 = transfer( accx(i), 0_int16, 2 )
            dummy2 = transfer( nmh_m1_v(i), 0_int16, 2 )
            dummy1 = dummy1 * dummy2
            accx(i) = transfer( dummy1, 0_int32 )
            accx(i) = ieor( accx(i), ieor( ishft(accx(i), 5), &
                                           ishft(accx(i),-13) ) )
            dummy1 = transfer( accx(i), 0_int16, 2 )
            dummy2 = transfer( nmh_m2_v(i), 0_int16, 2 )
            dummy1 = dummy1 * dummy2
            accx(i) = transfer( dummy1, 0_int32 )
            accx(i) = ieor( accx(i), accy(i) )
            accx(i) = ieor( accx(i), ieor( ishft(accx(i), 11), &
                                           ishft(accx(i),-9) ) )
            dummy1 = transfer( accx(i), 0_int16, 2 )
            dummy2 = transfer( nmh_m3_v(i), 0_int16, 2 )
            dummy1 = dummy1 * dummy2
            accx(i) = transfer( dummy1, 0_int32 )
            accx(i) = ieor( accx(i), ieor( ishft(accx(i),-10), &
                                           ishft(accx(i),-20) ) )
        end do

    end subroutine nmhash32_long_round

    pure function nmhash32_long( p, seed ) result( sum )
        integer(int32) :: sum
        integer(int8), intent(in) :: p(0:)
        integer(int32), intent(in) :: seed

        integer(int32) :: accx(0:size(nmh_acc_init)-1)
        integer(int32) :: accy(0:size(nmh_acc_init)-1)
        integer(int64) :: nbrounds
        integer(int64) :: len
        integer(int32) :: len32(0:1)
        integer(int64) :: i

        len  = size( p, kind=int64 )
        nbrounds = (len-1) / ( 4*size(accx, kind=int64) * 2 )
        sum = 0

!  Init
        do i=0_int64, size(nmh_acc_init, kind=int64)-1
            accx(i) = nmh_acc_init(i)
            accy(i) = seed
        end do

        ! init
        do i=0_int64, nbrounds-1
            call nmhash32_long_round( accx, accy, &
                                      p(i*8*size(accx, kind=int64):) )
        end do
        call nmhash32_long_round( accx, accy, &
                                  p(len-8*size(accx, kind=int64):) )

        ! merge acc
        do i=0, size( accx, kind=int64 )-1
            accx(i) = ieor( accx(i), nmh_acc_init(i) )
            sum = sum + accx(i)
        end do

        len32 = transfer(len, 0_int32, 2)
        if ( little_endian ) then
            sum = sum + len32(1)
            sum = ieor(sum, len32(0))
        else
            sum = sum + len32(0)
            sum = ieor(sum, len32(1))
        end if

    end function nmhash32_long

    pure function nmhash32_avalanche32( x ) result( u32 )
        integer(int32) :: u32
        integer(int32), intent(in) :: x

        integer(int16) :: u16(0:1)
        integer(int32), parameter:: m1 = int(z'CCE5196D', int32)
        integer(int32), parameter:: m2 = int(z'464BE229', int32)
!       Due to an issue with Intel OneAPI ifort 2021 (see  
!       https://community.intel.com/t5/Intel-Fortran-Compiler/Intrinsic-transfer-with-a-provided-size-un-expected-behavior/m-p/1343313#M158733
!       ), it is not possible to define the following variables as a parameter.
        !integer(int16), parameter:: m1_16(0:1) = transfer(m1, 0_int16, 2)
        !integer(int16), parameter:: m2_16(0:1) = transfer(m2, 0_int16, 2)
        integer(int16) :: m1_16(0:1), m2_16(0:1)
        ! [-21 -8 cce5196d 12 -7 464be229 -21 -8] = 3.2267098842182733

        m1_16(0:1) = transfer(m1, 0_int16, 2)
        m2_16(0:1) = transfer(m2, 0_int16, 2)

        u32 = x
        u32 = ieor( u32, ieor( ishft( u32, -8 ), ishft( u32, -21 ) ) )
        u16 = transfer( u32, 0_int16, 2 )
        u16(0) = u16(0) * m1_16(0)
        u16(1) = u16(1) * m1_16(1)
        u32 = transfer( u16, 0_int32 )
        u32 = ieor( u32, ieor( ishft( u32, 12 ), ishft( u32, -7 ) ) )
        u16 = transfer( u32, 0_int16, 2 )
        u16(0) = u16(0) * m2_16(0)
        u16(1) = u16(1) * m2_16(1)
        u32 = transfer( u16, 0_int32 )
        u32 = ieor( u32, ieor( ishft( u32, -8 ), ishft( u32, -21 ) ) )

    end function nmhash32_avalanche32

    pure module function int8_nmhash32( key, seed ) result( hash )
!! NMHASH32 hash function for rank 1 array keys of kind INT8
        integer(int32) :: hash
        integer(int8), intent(in) :: key(0:)
        integer(int32), intent(in) :: seed
        integer(int64) :: len
        integer(int32) :: u32
        integer(int16) :: u16(0:1)
        integer(int32) :: x, y
        integer(int32) :: new_seed

        len = size( key, kind=int64 )
        if ( len <= 32 ) then
            if ( len > 8 ) then
                hash = nmhash32_9to32( key, seed )
                return
            else if ( len > 4 ) then
                x = nmh_readle32(key)
                y = ieor( nmh_readle32(key(len-4:)), nmh_prime32_4 + 2 + seed )
                x = x + y
                x = ieor( x, ishft(x, len + 7 ) )
                hash = nmhash32_0to8( x, ishftc(y, 5) )
                return
            else
                select case(len)
                case(0)
                    new_seed = seed + nmh_prime32_2
                    u32 = 0
                case(1)
                    new_seed = seed + nmh_prime32_2 + ishft(1_int32, 24) + &
                               2_int32
                    if ( little_endian ) then
                        u32 = transfer( [key(0), 0_int8, 0_int8, 0_int8], &
                                        0_int32 )
                    else
                        u32 = transfer( [0_int8, 0_int8, 0_int8, key(0)], &
                                        0_int32 )
                    end if
                case(2)
                    new_seed = seed + nmh_prime32_2 + ishft(2_int32, 24) + &
                               4_int32
                    if (little_endian) then
                        u32 = transfer( [nmh_readle16(key), 0_int16], 0_int32 )
                    else
                        u32 = transfer( [0_int16, nmh_readle16(key)], 0_int32 )
                    end if
                case(3)
                    new_seed = seed + nmh_prime32_2 + ishft(3_int32, 24) + &
                               6_int32
                    if ( little_endian ) then
                        u16(1) = transfer( [key(2), 0_int8], 0_int16 )
                        u16(0) = nmh_readle16( key )
                    else
                        u16(0) = transfer( [0_int8, key(2)], 0_int16 )
                        u16(1) = nmh_readle16( key )
                    end if
                    u32 = transfer( u16, 0_int32 )
                case(4)
                    new_seed = seed + nmh_prime32_3
                    u32 = nmh_readle32(key)
                case default
                    hash = 0
                    return
                end select
                hash = nmhash32_0to8(u32+new_seed, ishftc(new_seed, 5) )
                return
            end if
        else if ( len < 256_int64 ) then
            hash = nmhash32_33to255( key, seed )
            return
        else
            hash = nmhash32_avalanche32( nmhash32_long(key, seed ))
            return
        end if

    end function int8_nmhash32

    pure function nmhash32x_0to4( x, seed ) result( hash )
        integer(int32), intent(in) :: x
        integer(int32), intent(in) :: seed
        integer(int32) :: hash

        ! [bdab1ea9 18 a7896a1b 12 83796a2d 16] = 0.092922873297662509

        hash = x
        hash = ieor( hash, seed )
        hash = hash * int(z'BDAB1EA9', int32)
        hash = hash + ishftc(seed, 31)
        hash = ieor( hash, ishft(hash, -18) )
        hash = hash * int(z'A7896A1B', int32)
        hash = ieor( hash, ishft(hash, -12) )
        hash = hash * int(z'83796A2D', int32)
        hash = ieor( hash, ishft(hash, -16) )

    end function nmhash32x_0to4

    pure function nmhash32x_5to8( p, seed ) result( x )
        integer(int8), intent(in) :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: x

        integer(int64) :: len
        integer(int32) :: y

        ! 5 to 9 bytes
        ! mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246

        len = size(p, kind=int64)
        x = ieor( nmh_readle32(p), nmh_prime32_3 )
        y = ieor( nmh_readle32(p(len-4:)), seed )
        x  = x + y
        x = ieor( x, ishft(x, -len) )
        x = x * int(z'11049A7D', int32)
        x = ieor( x, ishft(x, -23) )
        x = x * int(z'BCCCDC7B', int32)
        x = ieor( x, ishftc(y, 3) )
        x = ieor( x, ishft(x, -12) )
        x = x * int(z'065E9DAD', int32)
        x = ieor( x, ishft(x, -12) )

    end function nmhash32x_5to8

    pure function nmhash32x_9to255( p, seed ) result( x )
        integer(int8), intent(in) :: p(0:)
        integer(int32), intent(in) :: seed
        integer(int32) :: x

        integer(int64) :: len
        integer(int32) :: len32(0:1), len_base
        integer(int32) :: y
        integer(int32) :: a, b
        integer(int64) :: i, r

        ! - at least 9 bytes
        ! - base mixer: [11049a7d 23 bcccdc7b 12 065e9dad 12] = 0.16577596555667246
        ! - tail mixer: [16 a52fb2cd 15 551e4d49 16] = 0.17162579707098322

        len = size(p, kind=int64)
        len32 = transfer(len, 0_int32, 2)
        if (little_endian) then
            len_base = len32(0)
        else
            len_base = len32(1)
        end if
        x = nmh_prime32_3
        y = seed
        a = nmh_prime32_4
        b = seed
        r = (len - 1)/16

        do i=0, r-1
            x = ieor(x, nmh_readle32( p(i*16 + 0:) ) )
            y = ieor(y, nmh_readle32( p(i*16 + 4:) ) )
            x = ieor(x, y)
            x = x * int(z'11049A7D', int32)
            x = ieor(x, ishft(x, -23) )
            x = x * int(z'BCCCDC7B', int32)
            y = ishftc(y, 4)
            x = ieor(x, y)
            x = ieor(x, ishft(x, -12) )
            x = x * int(z'065E9DAD', int32)
            x = ieor(x, ishft(x, -12) )

            a = ieor(a, nmh_readle32(p(i*16 + 8:)))
            b = ieor(b, nmh_readle32(p(i*16 + 12:)))
            a = ieor(a, b)
            a = a * int(z'11049A7D', int32)
            a = ieor(a, ishft(a, -23) )
            a = a * int(z'BCCCDC7B', int32)
            b = ishftc(b, 3)
            a = ieor(a, b)
            a = ieor(a, ishft(a, -12) )
            a = a * int(z'065E9DAD', int32)
            a = ieor(a, ishft(a, -12) )
        end do

        if ( iand(len_base-1_int32, 8_int32) /= 0 ) then
            if ( iand(len_base-1_int32, 4_int32) /= 0 ) then
                a = ieor( a, nmh_readle32( p(r*16 + 0:) ) )
                b = ieor( b, nmh_readle32( p(r*16 + 4:) ) )
                a = ieor(a, b)
                a = a * int(z'11049A7D', int32)
                a = ieor(a, ishft(a, -23) )
                a = a * int(z'BCCCDC7B', int32)
                a = ieor(a, ishftc(b, 4))
                a = ieor(a, ishft(a, -12))
                a = a * int(z'065E9DAD', int32)
            else
                a = ieor( a, nmh_readle32( p(r*16:) ) + b )
                a = ieor( a, ishft(a, -16) )
                a = a * int(z'A52FB2CD', int32)
                a = ieor( a, ishft(a, -15) )
                a = a * int(z'551E4D49', int32)
            end if
            x = ieor( x, nmh_readle32( p(len - 8:) ) )
            y = ieor( y, nmh_readle32( p(len - 4:) ) )
            x = ieor( x, y )
            x = x * int(z'11049A7D', int32)
            x = ieor( x, ishft(x, -23) )
            x = x * int(z'BCCCDC7B', int32);
            x = ieor( x, ishftc(y, 3) )
            x = ieor( x, ishft(x, -12) )
            x = x * int(z'065E9DAD', int32)
        else
            if ( iand(len_base-1_int32, 4_int32) /= 0) then
                a = ieor(a, nmh_readle32(p( r * 16:) ) + b )
                a = ieor( a, ishft(a,-16) )
                a = a * int(z'A52FB2CD', int32)
                a = ieor( a, ishft(a,-15) )
                a = a * int(z'551E4D49', int32)
            end if
            x = ieor( x, nmh_readle32(p( len - 4:) ) + y )
            x = ieor( x, ishft(x,-16) )
            x = x * int(z'A52FB2CD', int32)
            x = ieor( x, ishft(x,-15) )
            x = x * int(z'551E4D49', int32)
        end if

        x = ieor(x, len_base )
        x = ieor(x, ishftc(a, 27)) ! rotate one lane to pass Diff test
        x = ieor(x, ishft(x,-14))
        x = x * int(z'141CC535', int32 )

    end function nmhash32x_9to255

    pure function nmhash32x_avalanche32( x ) result(hash)
        integer(int32) :: hash
        integer(int32), intent(in) :: x
! Mixer with 2 mul from skeeto/hash-prospector:
! [15 d168aaad 15 af723597 15] = 0.15983776156606694

        hash = x
        hash = ieor( hash, ishft( hash, -15 ) )
        hash = hash * int( z'D168AAAD', int32 )
        hash = ieor( hash, ishft( hash, -15 ) )
        hash = hash * int( z'AF723597', int32 )
        hash = ieor( hash, ishft( hash, -15 ) )

    end function nmhash32x_avalanche32

    pure module function int8_nmhash32x( key, seed ) result(hash)
!! NMHASH32x hash function for rank 1 array keys of kind INT8
        integer(int32) :: hash
        integer(int8), intent(in) :: key(0:)
        integer(int32), intent(in) :: seed

        integer(int64) :: len
        integer(int32) :: seed2
        integer(int32) :: u32
        integer(int16) :: u16(0:1)

        len = size( key, kind=int64 )
        if ( len <= 8 ) then
            if ( len > 4 ) then
                hash = nmhash32x_5to8( key, seed )
                return
            else ! 0 to 4 bytes
                select case (len)
                case(0)
                    seed2 = seed + nmh_prime32_2
                    u32 = 0
                case(1)
                    seed2 = seed + nmh_prime32_2 + ishft(1_int32, 24) + &
                        ishft(1_int32, 1)
                    if (little_endian) then
                        u32 = transfer( [key(0), 0_int8, 0_int8, 0_int8], &
                                        0_int32 )
                    else
                        u32 = transfer( [0_int8, 0_int8, 0_int8, key(0)], &
                                        0_int32 )
                    end if
                case(2)
                    seed2 = seed + nmh_prime32_2 + ishft(2_int32, 24) + &
                        ishft(2_int32, 1)
                    if (little_endian) then
                        u32 = transfer( [nmh_readle16(key), 0_int16], 0_int32 )
                    else
                        u32 = transfer( [0_int16, nmh_readle16(key)], 0_int32 )
                    end if
                case(3)
                    seed2 = seed + nmh_prime32_2 + ishft(3_int32, 24) + &
                        ishft(3_int32, 1)
                    if (little_endian ) then
                        u16(1) = transfer( [ key(2), 0_int8 ], 0_int16 )
                        u16(0) = nmh_readle16(key)
                    else
                        u16(0) = transfer( [ 0_int8, key(2) ], 0_int16 )
                        u16(1) = nmh_readle16(key)
                    end if
                    u32 = transfer( u16, 0_int32 )
                case(4)
                    seed2 = seed + nmh_prime32_1
                    u32 = nmh_readle32(key)
                case default
                    hash = 0
                    return
                end select
                hash = nmhash32x_0to4(u32, seed2)
                return
            end if
        end if
        if (len < 256) then
            hash = nmhash32x_9to255(key, seed)
            return
        end if
        hash = nmhash32x_avalanche32(nmhash32_long(key, seed))

    end function int8_nmhash32x

    pure module function int16_nmhash32( key, seed ) result(hash_code)
!! NMHASH32 hash function for rank 1 array keys of kind int16
        integer(int16), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32( transfer( key, 0_int8, &
                     bytes_int16*size(key, kind=int64) ), seed)

    end function int16_nmhash32

    pure module function int32_nmhash32( key, seed ) result(hash_code)
!! NMHASH32 hash function for rank 1 array keys of kind int32
        integer(int32), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32( transfer( key, 0_int8, &
                     bytes_int32*size(key, kind=int64) ), seed)

    end function int32_nmhash32

    pure module function int64_nmhash32( key, seed ) result(hash_code)
!! NMHASH32 hash function for rank 1 array keys of kind int64
        integer(int64), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32( transfer( key, 0_int8, &
                     bytes_int64*size(key, kind=int64) ), seed)

    end function int64_nmhash32


    elemental module function character_nmhash32( key, seed ) result(hash_code)
!! NMHASH32 hash function for default character keys
        character(*), intent(in)   :: key
        integer(int32), intent(in) :: seed
        integer(int32)             :: hash_code

        hash_code = int8_nmhash32( transfer( key, 0_int8, &
                     bytes_char*len(key, kind=int64) ), seed)

    end function character_nmhash32

    pure module function int16_nmhash32x( key, seed ) result(hash_code)
!! NMHASH32X hash function for rank 1 array keys of kind int16
        integer(int16), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32x( transfer( key, 0_int8, &
                     bytes_int16*size(key, kind=int64) ), seed)

    end function int16_nmhash32x

    pure module function int32_nmhash32x( key, seed ) result(hash_code)
!! NMHASH32X hash function for rank 1 array keys of kind int32
        integer(int32), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32x( transfer( key, 0_int8, &
                     bytes_int32*size(key, kind=int64) ), seed)

    end function int32_nmhash32x

    pure module function int64_nmhash32x( key, seed ) result(hash_code)
!! NMHASH32X hash function for rank 1 array keys of kind int64
        integer(int64), intent(in) :: key(:)
        integer(int32), intent(in)  :: seed
        integer(int32)           :: hash_code

        hash_code = int8_nmhash32x( transfer( key, 0_int8, &
                     bytes_int64*size(key, kind=int64) ), seed)

    end function int64_nmhash32x


    elemental module function character_nmhash32x( key, seed ) result(hash_code)
!! NMHASH32X hash function for default character keys
        character(*), intent(in)   :: key
        integer(int32), intent(in) :: seed
        integer(int32)             :: hash_code

        hash_code = int8_nmhash32x( transfer( key, 0_int8, &
                     bytes_char*len(key, kind=int64) ), seed)

    end function character_nmhash32x

    module subroutine new_nmhash32_seed( seed )
! Random SEED generator for NMHASH32
        integer(int32), intent(inout) :: seed

        integer(int32) :: old_seed
        real(dp) :: sample

        old_seed = seed
        find_seed:do
            call random_number( sample )
            seed = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, &
                int32 )
            if ( seed /= old_seed ) return
        end do find_seed

    end subroutine new_nmhash32_seed

    module subroutine new_nmhash32x_seed( seed )
! Random SEED generator for NMHASH32X
         integer(int32), intent(inout) :: seed

        integer(int32) :: old_seed
        real(dp) :: sample

        old_seed = seed
        find_seed:do
            call random_number( sample )
            seed = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, &
                int32 )
            if ( seed /= old_seed ) return
        end do find_seed

    end subroutine new_nmhash32x_seed

end submodule stdlib_hash_32bit_nm
