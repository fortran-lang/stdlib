! This is free and unencumbered software released into the public domain.
! SPDX-Identifier: Unlicense

!> Provides implementations for the waterhash algorithm.
submodule(stdlib_hash) stdlib_hash_waterhash
    implicit none


contains

    !>```c
    !>uint64_t r = A * B;
    !>return r - (r >> 32);
    !>```
    pure function watermum(a, b) result(r)
        integer(i8), intent(in) :: a, b
        integer(i8) :: r
        r = a * b
        r = r - (ishft(r, -32))
    end function watermum

    !>```c
    !>uint8_t v;
    !>memcpy(&v, p, 1);
    !>return v;
    !>```
    pure function waterr08(p) result(v)
        integer(i1), intent(in) :: p(:)
        integer(i8) :: v
        integer(i1), parameter :: t = 0_i1
        v = int(transfer(p(1:1), t), i8)
    end function waterr08

    !>```c
    !>uint16_t v;
    !>memcpy(&v, p, 2);
    !>return v;
    !>```
    pure function waterr16(p) result(v)
        integer(i1), intent(in) :: p(:)
        integer(i8) :: v
        integer(i2), parameter :: t = 0_i2
        v = int(transfer(p(1:2), t), i8)
    end function waterr16

    !>```c
    !>uint32_t v;
    !>memcpy(&v, p, 4);
    !>return v;
    !>```
    pure function waterr32(p) result(v)
        integer(i1), intent(in) :: p(:)
        integer(i8) :: v
        integer(i4), parameter :: t = 0_i4
        v = int(transfer(p(1:4), t), i8)
    end function waterr32

    !> Portation of the waterhash algorithm.
    pure module function waterhash_impl(p, seed) result(val)
        integer(i1), intent(in) :: p(:)
        integer(i8), intent(in) :: seed
        integer(i4) :: val
        integer(i8) :: h

        integer(i8), parameter :: waterp0 = int(z'a0761d65', i8), &
            & waterp1 = int(z'e7037ed1', i8), waterp2 = int(z'8ebc6af1', i8), &
            & waterp3 = int(z'589965cd', i8), waterp4 = int(z'1d8e4e27', i8), &
            & waterp5 = int(z'eb44accb', i8)
        integer :: i
        integer :: len

        len = size(p)
        h = seed

        i = 0
        do
            if (i + 16 > len) exit
            h = watermum(watermum(ieor(waterr32(p(i+1:i+4)), waterp1), &
                &                 ieor(waterr32(p(i+5:i+8)), waterp2)) + h, &
                &        watermum(ieor(waterr32(p(i+9:i+12)), waterp3), &
                &                 ieor(waterr32(p(i+13:i+16)), waterp4)))
            i = i + 16
        end do
        h = h + waterp5

        select case(iand(len, 15))
        case(1)
            h = watermum(ieor(waterp2, h), &
                &        ieor(waterr08(p(i+1:i+1)), waterp1))
        case(2)
            h = watermum(ieor(waterp3, h), &
                &        ieor(waterr16(p(i+1:i+2)), waterp4))
        case(3)
            h = watermum(ieor(waterr16(p(i+1:i+2)), h), &
                &        ieor(waterr08(p(i+3:i+3)), waterp2))
        case(4)
            h = watermum(ieor(waterr16(p(i+1:i+2)), h), &
                &        ieor(waterr16(p(i+3:i+4)), waterp3))
        case(5)
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(waterr08(p(i+5:i+5)), waterp1))
        case(6)
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(waterr16(p(i+5:i+6)), waterp1))
        case(7)
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(ior(ishft(waterr16(p(i+5:i+6)), 8), waterr08(p(i+7:i+7))), &
                &             waterp1))
        case(8)
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(waterr32(p(i+5:i+8)), waterp0))
        case(9)
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterp4), &
                &             ieor(waterr08(p(i+9:i+9)), waterp3)))
        case(10)
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(h, &
                &             ieor(waterr16(p(i+9:i+10)), waterp3)))
        case(11)
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(h, &
                &             ieor(ior(ishft(waterr16(p(i+9:i+10)), 8), &
                &                      waterr08(p(i+11:i+11))), &
                &                  waterp3)))
        case(12)
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             waterp4))
        case(13)
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             ieor(waterr08(p(i+13:i+13)), waterp4)))
        case(14)
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             ieor(waterr16(p(i+13:i+14)), waterp4)))
        case(15)
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             ieor(ior(ishft(waterr16(p(i+13:i+14)), 8), &
                &                      waterr08(p(i+15:i+15))), &
                &                  waterp4)))
        end select

        h = ieor(h, ishft(h, 16)) * ieor(int(len, i8), waterp0)
        val = transfer(h - ishft(h, -32), val)

    end function waterhash_impl

end submodule stdlib_hash_waterhash
