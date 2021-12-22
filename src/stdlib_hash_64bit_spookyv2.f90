!!------------------------------------------------------------------------------
!! `SPOOKY_HASH` is a translation to Fortran 2008 of the unsigned 64 bit
!! `SpookyHash` V2 function of Bob Jenkins
!! <https://burtleburtle.net/bob/hash/spooky.html> to signed 64 bit
!! operations. Bob Jenkins has put his code in the public domain and has
!! given permission to treat this code as public domain in the USA,
!! provided the code can be used under other licenses and he is given
!! appropriate credit.
!! The code was designed for Little-Endian processors. The output is
!! different on Big Endian processors, but still probably as good quality.
!!------------------------------------------------------------------------------


submodule(stdlib_hash_64bit) stdlib_hash_64bit_spookyv2

! I have tried to make this portable while retaining efficiency. I assume
! processors with two's complement integers from 8, 16, 32, and 64 bits.
! The code is a transliteration of the 64 bit SpookyHash V2 of Bob Jenkins
!     <https://burtleburtle.net/bob/hash/spooky.html>
! The code was designed for Little-Endian processors. The output is
! different on Big Endian processors, but still probably as good quality.

    implicit none

contains


     module function int8_spooky_hash( key, seed ) result(hash_code)
        integer(int8), intent(in)  :: key(:)
        integer(int64), intent(in) :: seed(2)
        integer(int64)             :: hash_code(2)

        hash_code(:) = seed
        call spookyhash_128( key, hash_code )

    end function int8_spooky_hash


     module function int16_spooky_hash( key, seed ) result(hash_code)
        integer(int16), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed(2)
        integer(int64)              :: hash_code(2)

        integer(int64) :: hash2(2)

        hash2(:) = seed
        call spookyhash_128( transfer( key, 0_int8, &
                     bytes_int16*size(key, kind=int64) ), hash2 )
        hash_code = hash2

    end function int16_spooky_hash

     module function int32_spooky_hash( key, seed ) result(hash_code)
        integer(int32), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed(2)
        integer(int64)              :: hash_code(2)

        integer(int64) :: hash2(2)

        hash2(:) = seed
        call spookyhash_128( transfer( key, 0_int8, &
                     bytes_int32*size(key, kind=int64) ), hash2 )
        hash_code = hash2

    end function int32_spooky_hash

     module function int64_spooky_hash( key, seed ) result(hash_code)
        integer(int64), intent(in) :: key(:)
        integer(int64), intent(in)  :: seed(2)
        integer(int64)              :: hash_code(2)

        integer(int64) :: hash2(2)

        hash2(:) = seed
        call spookyhash_128( transfer( key, 0_int8, &
                     bytes_int64*size(key, kind=int64) ), hash2 )
        hash_code = hash2

    end function int64_spooky_hash


     module function character_spooky_hash( key, seed ) result(hash_code)
        character(*), intent(in)   :: key
        integer(int64), intent(in) :: seed(2)
        integer(int64)             :: hash_code(2)

        integer(int64)    :: hash2(2)

        hash2(:) = seed
        call spookyhash_128( transfer( key, 0_int8,                    &
                             bytes_char*len(key, kind=int64) ), hash2 )
        hash_code = hash2

    end function character_spooky_hash

!
! short hash ... it could be used on any message,
! but it's used by Spooky just for short messages.
!
     subroutine spookyhash_short( key, hash_inout )
        integer(int8), intent(in), target :: key(0:)
        integer(int64), intent(inout)     :: hash_inout(2)

        integer(int64) :: a, b, c, d
        integer(int64) :: length, p8, remainder

        p8 = 0
        length = size( key, kind=int64 )

        ! The number of bytes after all the INT256s
        remainder = iand( length, 31_int64 )
        a = hash_inout(1)
        b = hash_inout(2)
        c = sc_const
        d = sc_const

        if ( length > 15 ) then
            block
                integer(int64) :: bend, step
                integer(int64) :: buf(0:2*sc_numVars-1)
                bend = ishft(length, -4) ! The number of complete INT128s
                buf(0:2*bend-1) = transfer( key(0:16*bend-1), 0_int64, 2*bend )
                ! Number of Int64's in number of complete INT256s
                bend = ishft(ishft(length, -5), 2)

                ! handle all complete sets of 32 bytes
                do step = 0_int64, bend-1, 4
                    c = c + buf(step)
                    d = d + buf(step+1)
                    call shortmix( a, b, c, d )
                    a = a + buf(step+2)
                    b = b + buf(step+3)
                end do
                ! Completed all INT64s in complete INT256s
                p8 = p8 + 8*bend ! Number of INT8s in complete INT256s

                ! Handle the case of 16+ remaining bytes.
                if (remainder >= 16) then
                    c = c + buf(step)
                    d = d + buf(step+1)
                    call shortmix( a, b, c, d )
                    p8 = p8 + 16
                    remainder = remainder - 16
                end if

            end block
        end if

    ! Handle the last 0..15 bytes, and its length V2
        d = d + shiftl( length, 56_int64 )

        select case(remainder)
        case(15)
            go to 115
        case(14)
            go to 114
        case(13)
            go to 113
        case(12)
            go to 112
        case(11)
            go to 111
        case(10)
            go to 110
        case(9)
            go to 109
        case(8)
            go to 108
        case(7)
            go to 107
        case(6)
            go to 106
        case(5)
            go to 105
        case(4)
            go to 104
        case(3)
            go to 103
        case(2)
            go to 102
        case(1)
            go to 101
        case(0)
            go to 100
        end select

115     d = d + shiftl( map_to_64( key(p8+14) ), 48_int64 )
114     d = d + shiftl( map_to_64( key(p8+13) ), 40_int64 )
113     d = d + shiftl( map_to_64( key(p8+12) ), 32_int64 )
112     if ( little_endian) then
            d = d + transfer( [ transfer(key(p8+8:p8+11), 0_int32), &
                                0_int32 ], 0_int64)
        else
            d = d + transfer( [ 0_int32, &
                                transfer(key(p8+8:p8+11), 0_int32) ], &
                                0_int64)
        end if
        c = c + transfer( key(p8+0:p8+7), 0_int64 )
        go to 888

111     d = d + shiftl( map_to_64( key(p8+10) ), 16_int32 )
110     d = d + shiftl( map_to_64( key(p8+9) ), 8_int32 )
109     d = d + map_to_64( key(p8+8) )
108     c = c + transfer( key(p8+0:p8+7), 0_int64 )
        go to 888

107     c = c + shiftl( map_to_64( key(p8+6) ), 48_int64 )
106     c = c + shiftl( map_to_64( key(p8+5) ), 40_int64 )
105     c = c + shiftl( map_to_64( key(p8+4) ), 32_int64 )
104     if ( little_endian) then
            c = c + transfer( [ transfer( key(p8+0:p8+3), 0_int32 ), &
                                0_int32 ], 0_int64 )
        else
            c = c + transfer( [ 0_int32, &
                                transfer( key(p8+0:p8+3), 0_int32 ) ], 0_int64 )
        end if

        go to 888

103     c = c + shiftl( map_to_64( key(p8+2) ), 16_int64 )
102     c = c + shiftl( map_to_64( key(p8+1) ), 8_int64 )
101     c = c + map_to_64( key(p8+0) )
        go to 888

100     c = c + sc_const
        d = d + sc_const

888     call short_end( a, b, c, d )

        hash_inout(1) = a
        hash_inout(2) = b
        close(40)

    contains

        pure function map_to_64( key )
            integer(int8), intent(in) :: key
            integer(int64)         :: map_to_64

            if ( little_endian ) then
                map_to_64 = transfer( [ key, 0_int8, 0_int8, 0_int8, &
                                        0_int8, 0_int8, 0_int8, 0_int8 ], &
                                      0_int64 )
            else
                map_to_64 = transfer( [ 0_int8, 0_int8, 0_int8, 0_int8, &
                                        0_int8, 0_int8, 0_int8, key ], &
                                      0_int64 )
            end if

        end function map_to_64

        pure subroutine shortmix( h0, h1, h2, h3 )
    !
    ! The goal is for each bit of the input to expand into 128 bits of
    ! apparent entropy before it is fully overwritten.
    ! n trials both set and cleared at least m bits of h0 h1 h2 h3
    !   n: 2   m: 29
    !   n: 3   m: 46
    !   n: 4   m: 57
    !   n: 5   m: 107
    !   n: 6   m: 146
    !   n: 7   m: 152
    ! when run forwards or backwards
    ! for all 1-bit and 2-bit diffs
    ! with diffs defined by either xor or subtraction
    ! with a base of all zeros plus a counter, or plus another bit, or random
    !
            integer(int64), intent(inout) :: h0, h1, h2, h3

            h2 = ishftc( h2, 50 )
            h2 = h2 + h3
            h0 = ieor( h0, h2 )
            h3 = ishftc( h3, 52 )
            h3 = h3 + h0
            h1 = ieor( h1, h3 )
            h0 = ishftc( h0, 30 )
            h0 = h0 + h1
            h2 = ieor( h2, h0 )
            h1 = ishftc( h1, 41 )
            h1 = h1 + h2
            h3 = ieor( h3, h1 )
            h2 = ishftc( h2, 54 )
            h2 = h2 + h3
            h0 = ieor( h0, h2 )
            h3 = ishftc( h3, 48 )
            h3 = h3 + h0
            h1 = ieor( h1, h3 )
            h0 = ishftc( h0, 38 )
            h0 = h0 + h1
            h2 = ieor( h2, h0 )
            h1 = ishftc( h1, 37 )
            h1 = h1 + h2
            h3 = ieor( h3, h1 )
            h2 = ishftc( h2, 62 )
            h2 = h2 + h3
            h0 = ieor( h0, h2 )
            h3 = ishftc( h3, 34 )
            h3 = h3 + h0
            h1 = ieor( h1, h3 )
            h0 = ishftc( h0, 5 )
            h0 = h0 + h1
            h2 = ieor( h2, h0 )
            h1 = ishftc( h1, 36 )
            h1 = h1 + h2
            h3 = ieor( h3, h1 )

        end subroutine shortmix

        pure subroutine short_end( h0, h1, h2, h3 )
    !
    ! Mix all 4 inputs together so that h0, h1 are a hash of them all.
    !
    ! For two inputs differing in just the input bits
    ! Where "differ" means xor or subtraction
    ! And the base value is random, or a counting value starting at that bit
    ! The final result will have each bit of h0, h1 flip
    ! For every input bit,
    ! with probability 50 +- .3% (it is probably better than that)
    ! For every pair of input bits,
    ! with probability 50 +- .75% (the worst case is approximately that)
    !
            integer(int64), intent(inout) :: h0, h1, h2, h3

            h3 = ieor( h3, h2 )
            h2 = ishftc( h2, 15 )
            h3 = h3 + h2
            h0 = ieor( h0, h3 )
            h3 = ishftc( h3, 52 )
            h0 = h0 + h3
            h1 = ieor( h1, h0 )
            h0 = ishftc( h0, 26 )
            h1 = h1 + h0
            h2 = ieor( h2, h1 )
            h1 = ishftc( h1, 51 )
            h2 = h2 + h1
            h3 = ieor( h3, h2 )
            h2 = ishftc( h2, 28 )
            h3 = h3 + h2
            h0 = ieor( h0, h3 )
            h3 = ishftc( h3, 9 )
            h0 = h0 + h3
            h1 = ieor( h1, h0 )
            h0 = ishftc( h0, 47 )
            h1 = h1 + h0
            h2 = ieor( h2, h1 )
            h1 = ishftc( h1, 54 )
            h2 = h2 + h1
            h3 = ieor( h3, h2 )
            h2 = ishftc( h2, 32 )
            h3 = h3 + h2
            h0 = ieor( h0, h3 )
            h3 = ishftc( h3, 25 )
            h0 = h0 + h3
            h1 = ieor( h1, h0 )
            h0 = ishftc( h0, 63 )
            h1 = h1 + h0

        end subroutine short_end

    end subroutine spookyhash_short


! do the whole hash in one call
     module subroutine spookyHash_128( key, hash_inout )
        integer(int8), intent(in), target :: key(0:)
        integer(int64), intent(inout)     :: hash_inout(2)

        integer(int64) :: buf(sc_numvars)
        integer(int64) :: h(0:11)
        integer(int64) :: bend, i, length, p8, remain, remainder, tail
        integer(int8)  :: buf8(8)

        length = size(key, kind=int64)

        if ( length < sc_buffsize ) then
            call spookyhash_short( key, hash_inout )
            return
        end if

        h( [ 0, 3, 6,  9 ] ) = hash_inout(1)
        h( [ 1, 4, 7, 10 ] ) = hash_inout(2)
        h( [ 2, 5, 8, 11 ] ) = sc_const

        ! Number of bytes in number of complete internal states
        bend = (length/sc_blocksize)*sc_blocksize

        ! Handle all SC_BLOCKSIZE blocks of bytes
        do i=0, bend-1, sc_blocksize
            buf(:) = transfer( key(i:i+sc_blocksize-1), 0_int64, sc_numVars )
            call spookyhash_mix( buf, h )
        end do ! all complete internal states processed

    ! handle the last partial block of sc_blocksize bytes
        remainder = ( length - bend ) ! 0 <= remainder < sc_blocksize == 96
        remain    = remainder / 8 ! Number of INT64's in partial block
        buf(1:remain) = transfer( key(bend:bend+remain*8-1), 0_int64, remain )
        buf(remain+1:sc_numvars) = 0_int64
        tail = remainder - 8 * remain ! Number of INT8s after INT64s
        p8 = bend + remain * 8 ! # of bytes until tail start
        buf8(1:tail) = key(p8:p8+tail-1)
        buf8(tail+1:8) = 0_int8
        buf(remain+1) = transfer( buf8, 0_int64 )
        buf8(1:7) = 0_int8
        buf8(8) = int( remainder, kind=int8 ) ! 0 <= remainder < 96
        buf(sc_numvars) = ieor( buf(sc_numvars), transfer( buf8, 0_int64 ) )

    ! do some final mixing
        call spookyhash_end( buf, h )
        hash_inout(1:2) = h(0:1)

    end subroutine spookyHash_128

    !
    ! This is used if the input is 96 bytes long or longer.
    !
    ! The internal state is fully overwritten every 96 bytes.
    ! Every input bit appears to cause at least 128 bits of entropy
    ! before 96 other bytes are combined, when run forward or backward
    !   For every input bit,
    !   Two inputs differing in just that input bit
    !   Where "differ" means xor or subtraction
    !   And the base value is random
    !   When run forward or backwards one Mix
    ! I tried 3 pairs of each; they all differed by at least 212 bits.
    !
    pure subroutine spookyhash_mix( data, s )
        integer(int64), intent(in)    :: data(0:)
        integer(int64), intent(inout) :: s(0:11)

        s(0)  = s(0) + data(0)
        s(2)  = ieor( s(2), s(10) )
        s(11) = ieor( s(11), s(0) )
        s(0)  = ishftc( s(0), 11 )
        s(11) = s(11) + s(1)
        s(1)  = s(1) + data(1)
        s(3)  = ieor( s(3), s(11) )
        s(0)  = ieor( s(0), s(1) )
        s(1)  = ishftc( s(1), 32 )
        s(0)  = s(0) + s(2)
        s(2)  = s(2) + data(2)
        s(4)  = ieor( s(4), s(0) )
        s(1)  = ieor( s(1), s(2) )
        s(2)  = ishftc( s(2), 43 )
        s(1)  = s(1) + s(3)
        s(3)  = s(3) + data(3)
        s(5)  = ieor( s(5), s(1) )
        s(2)  = ieor( s(2), s(3) )
        s(3)  = ishftc( s(3), 31 )
        s(2)  = s(2) + s(4)
        s(4)  = s(4) + data(4)
        s(6)  = ieor( s(6), s(2) )
        s(3)  = ieor( s(3), s(4) )
        s(4)  = ishftc( s(4), 17 )
        s(3)  = s(3) + s(5)
        s(5)  = s(5) + data(5)
        s(7)  = ieor( s(7), s(3) )
        s(4)  = ieor( s(4), s(5) )
        s(5)  = ishftc( s(5), 28 )
        s(4)  = s(4) + s(6)
        s(6)  = s(6) + data(6)
        s(8)  = ieor( s(8), s(4) )
        s(5)  = ieor( s(5), s(6) )
        s(6)  = ishftc( s(6), 39 )
        s(5)  = s(5) + s(7)
        s(7)  = s(7) + data(7)
        s(9)  = ieor( s(9), s(5) )
        s(6)  = ieor( s(6), s(7) )
        s(7)  = ishftc( s(7), 57 )
        s(6)  = s(6) + s(8)
        s(8)  = s(8) + data(8)
        s(10) = ieor( s(10), s(6) )
        s(7)  = ieor( s(7), s(8) )
        s(8)  = ishftc( s(8), 55 )
        s(7)  = s(7) + s(9)
        s(9)  = s(9) + data(9)
        s(11) = ieor( s(11), s(7) )
        s(8)  = ieor( s(8), s(9) )
        s(9)  = ishftc( s(9), 54 )
        s(8)  = s(8) + s(10)
        s(10) = s(10) + data(10)
        s(0)  = ieor( s(0), s(8) )
        s(9)  = ieor( s(9), s(10) )
        s(10) = ishftc( s(10), 22 )
        s(9)  = s(9) + s(11)
        s(11) = s(11) + data(11)
        s(1)  = ieor( s(1), s(9) )
        s(10) = ieor( s(10), s(11) )
        s(11) = ishftc( s(11), 46 )
        s(10) = s(10) + s(0)

    end subroutine spookyhash_mix


    pure subroutine spookyhash_end( data, h)
        integer(int64), intent(in)    :: data(0:)
        integer(int64), intent(inout) :: h(0:11)

        h  = h + data(0:11)
        call endpartial( h )
        call endpartial( h )
        call endpartial( h )

    contains
    !
    ! Mix all 12 inputs together so that h0, h1 are a hash of them all.
    !
    ! For two inputs differing in just the input bits
    ! Where "differ" means xor or subtraction
    ! And the base value is random, or a counting value starting at that bit
    ! The final result will have each bit of h0, h1 flip
    ! For every input bit,
    ! with probability 50 +- .3%
    ! For every pair of input bits,
    ! with probability 50 +- 3%
    !
    ! This does not rely on the last Mix() call having already mixed some.
    ! Two iterations was almost good enough for a 64-bit result, but a
    ! 128-bit result is reported, so End() does three iterations.
    !
        pure subroutine endpartial( h )
            integer(int64), intent(inout) :: h(0:11)

            h(11) = h(11) + h(1)
            h(2)  =   ieor( h(2), h(11) )
            h(1)  = ishftc( h(1), 44 )
            h(0)  = h(0) + h(2)
            h(3)  =   ieor( h(3), h(0) )
            h(2)  = ishftc( h(2), 15 )
            h(1)  = h(1) + h(3)
            h(4)  =   ieor( h(4), h(1) )
            h(3)  = ishftc( h(3), 34 )
            h(2)  = h(2) + h(4)
            h(5)  =   ieor( h(5), h(2) )
            h(4)  = ishftc( h(4), 21 )
            h(3)  = h(3) + h(5)
            h(6)  =   ieor( h(6), h(3) )
            h(5)  = ishftc( h(5), 38 )
            h(4)  = h(4) + h(6)
            h(7)  = ieor( h(7), h(4) )
            h(6)  = ishftc( h(6), 33 )
            h(5)  = h(5) + h(7)
            h(8)  = ieor( h(8), h(5) )
            h(7)  = ishftc( h(7), 10 )
            h(6)  = h(6) + h(8)
            h(9)  = ieor( h(9), h(6) )
            h(8)  = ishftc( h(8), 13 )
            h(7)  = h(7) + h(9)
            h(10) = ieor( h(10), h(7) )
            h(9)  = ishftc( h(9), 38 )
            h(8)  = h(8) + h(10)
            h(11) = ieor( h(11), h(8) )
            h(10) = ishftc( h(10), 53 )
            h(9)  = h(9) + h(11)
            h(0)  = ieor( h(0), h(9) )
            h(11) = ishftc( h(11), 42 )
            h(10) = h(10) + h(0)
            h(1)  = ieor( h(1), h(10) )
            h(0)  = ishftc( h(0), 54 )

        end subroutine endpartial

    end subroutine spookyhash_end


     pure module subroutine spookysubhash_init( self, seed )
        type(spooky_subhash), intent(out) :: self
        integer(int64), intent(in)     :: seed(2)

        self % state(0:1) = seed
        self % length     = 0
        self % remainder  = 0_int8

    end subroutine spookysubhash_init


! add a message fragment to the state
     module subroutine spookyhash_update( spooky, key )
        type(spooky_subhash), intent(inout) :: spooky
        integer(int8), intent(in)           :: key(0:)

        integer(int8)  :: dummy(0:7)
        integer(int64) :: h(0:11)
        integer(int64)  :: bend,       &
                           length,     &
                           new_length, &
                           p8,         &
                           remainder

        length = size(key, kind=int64)
        new_length = length + spooky % remainder

    ! Is this message fragment too short?  If it is, stuff it away.
        if ( new_Length < sc_buffsize ) then
            remainder = spooky % remainder
            spooky % data( remainder:remainder+length-1 ) = key
            spooky % length = length + spooky % length
            dummy = transfer( new_length, 0_int8, 8 )
            if ( little_endian ) then
                spooky % remainder = transfer( [ dummy(0), 0_int8 ], 0_int16 )
            else
                spooky % remainder = transfer( [ 0_int8, dummy(7) ], 0_int16 )
            end if
            return
        end if

    ! init the variables
        if ( spooky % length < sc_buffsize ) then
            h( [ 0, 3, 6,  9 ] ) = spooky % state(0)
            h( [ 1, 4, 7, 10 ] ) = spooky % state(1)
            h( [ 2, 5, 8, 11 ] ) = sc_const
        else
            h(0:11)  = spooky % state(0:11)
        end if

        spooky % length = length + spooky % length

    ! if we've got anything stuffed away, use it now
        if ( spooky % remainder /= 0_int16 ) then
            block
                integer(int16) :: prefix
                prefix = sc_buffsize - spooky % remainder
                remainder = spooky % remainder
                spooky % data(remainder:remainder+prefix-1) = key(0:prefix-1)
                call spookyhash_mix( transfer(spooky % data(0:sc_blocksize-1), &
                                              0_int64, sc_numvars), h )
                call spookyhash_mix(                                       &
                    transfer(spooky % data(sc_blocksize:2*sc_blocksize-1), &
                             0_int64, sc_numvars), h )
                p8 = prefix
                length = length - prefix
            end block
        else
            p8 = 0
        end if

    ! handle all whole blocks of sc_blocksize bytes requiring aligned bytes
        bend = p8 + 8*(length/sc_blocksize)*sc_numVars
        remainder = length - ( bend - p8 )
        do while( p8 < bend )
            spooky % data(0:sc_blocksize-1) = key( p8:p8+sc_blocksize-1 )
            call spookyhash_mix( transfer( spooky % data(0:sc_blocksize-1), &
                                 0_int64, sc_numvars), h )
            p8 = p8 + sc_blocksize
        end do

    ! stuff away the last few bytes
        spooky % remainder = remainder

        if ( remainder > 0 ) then
            spooky % data(0:remainder-1) = &
                key(bend:bend+remainder-1)
        end if

    ! stuff away the variables
        spooky % state(0:11) = h(0:11)

    end subroutine spookyhash_update


! report the hash for the concatenation of all message fragments so far
     module subroutine spookyhash_final(spooky, hash_code)
        type(spooky_subhash), intent(inout) :: spooky
        integer(int64), intent(inout)       :: hash_code(2)

        integer(int64) :: h(0:11)
        integer(int64) :: index, remainder
        integer(int8)  :: dummy(2)

    ! init the variables
        if ( spooky % length < sc_buffsize ) then
            hash_code = spooky % state(0:1)
            call spookyhash_short( spooky % data(0:spooky % length-1), &
                                   hash_code )
            return
        end if

        remainder = spooky % remainder

        h(0:11)  = spooky % state(0:11)

        if ( remainder >= sc_blocksize ) then
          ! m_data can contain two blocks; handle any whole first block
            call spookyhash_mix( transfer( spooky % data, 0_int64, &
                                           2*sc_numvars), h )
            index = sc_blocksize
            remainder = remainder - sc_blocksize
        else
            index = 0
        end if

    ! mix in the last partial block, and the length mod sc_blocksize
        spooky % data(sc_blocksize+remainder:) = 0_int8
        dummy = transfer( remainder, 0_int8, 2 )

        if  ( little_endian ) then
            spooky % data(sc_blocksize-1) = dummy(1)
        else
            spooky % data(sc_blocksize-1) = dummy(2)
        end if

    ! do some final mixing
        call spookyhash_end( transfer(spooky % data, 0_int64, 2*sc_numvars), h )

        hash_code(1:2) = h(0:1)

    end subroutine spookyhash_final


    pure function rot_64_32( a, k )
        integer(int64)             :: rot_64_32
        integer(int64), intent(in) :: a
        integer, intent(in)           :: k

        rot_64_32 = iand( ior( shiftl( a, k ), shiftr( a, 32-k ) ), two_32-1 )

    end function rot_64_32


    module subroutine new_spooky_hash_seed( seed )
! Random SEED generator for
        integer(int64), intent(inout) :: seed(2)

        integer(int64) :: old_seed(2)
        real(dp)       :: sample(4)
        integer(int32) :: part(4)

        old_seed = seed
        find_seed: do
            call random_number( sample )
            part = int( floor( sample * 2_int64**32, int64 ) - 2_int64**31, &
                int32 )
            seed = transfer( part, seed, 2 )
            if ( seed(1) /= old_seed(1) .or. seed(2) /= old_seed(2) ) return
        end do find_seed

    end subroutine new_spooky_hash_seed


end submodule stdlib_hash_64bit_spookyv2
