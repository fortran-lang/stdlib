! SPDX-Identifier: MIT

!> Provides interfaces and implementations for hashing algorithms.
!>
!> The specification of this module is available [here](../page/specs/stdlib_hash.html).
module stdlib_hash
    use, intrinsic :: iso_c_binding, only : c_loc, c_ptr
    use stdlib_kinds, only : i1 => int8, i2 => int16, i4 => int32, i8 => int64
    implicit none
    private

    public :: hash, waterhash


    !> Hash function to obtain a hashed value of the provided variable
    !>
    !> Version: experimental
    interface hash
        module procedure :: hash_character
    end interface hash


contains


    !> Interface to the waterhash implementation for character scalars
    pure function hash_character(var, seed) result(val)
        !> Variable to hash
        character(len=*), target, intent(in) :: var
        !> Seed for the hashing function
        integer(i8), intent(in) :: seed
        !> Hash value
        integer :: val

        integer(i1), parameter :: t = 0_i1

        val = waterhash(transfer(var, t, len(var)), seed)

    end function hash_character

    !>```c
    !>uint64_t r = A * B;
    !>return r - (r >> 32);
    !>```
    !  SPDX-Identifier: Unlicense
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
    !  SPDX-Identifier: Unlicense
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
    !  SPDX-Identifier: Unlicense
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
    !  SPDX-Identifier: Unlicense
    pure function waterr32(p) result(v)
        integer(i1), intent(in) :: p(:)
        integer(i8) :: v
        integer(i4), parameter :: t = 0_i4
        v = int(transfer(p(1:4), t), i8)
    end function waterr32

    !> Portation of the waterhash algorithm.
    !  SPDX-Identifier: Unlicense
    pure function waterhash(p, seed) result(val)
        integer(i1), intent(in) :: p(:)
        integer(i8), intent(in) :: seed
        integer :: val
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
               &                  ieor(waterr32(p(i+5:i+8)), waterp2)) + h, &
               &         watermum(ieor(waterr32(p(i+9:i+12)), waterp3), &
               &                  ieor(waterr32(p(i+13:i+16)), waterp4)))
            i = i + 16
        end do
        h = h + waterp5

        select case(iand(len, 15))
        case(1)
            !seed = _watermum(_waterp2 ^ seed, _waterr08(p) ^ _waterp1); break;
            h = watermum(ieor(waterp2, h), &
                &        ieor(waterr08(p(i+1:i+1)), waterp1))
        case(2)
            !seed = _watermum(_waterp3 ^ seed, _waterr16(p) ^ _waterp4); break;
            h = watermum(ieor(waterp3, h), &
                &        ieor(waterr16(p(i+1:i+2)), waterp4))
        case(3)
            !seed = _watermum(_waterr16(p) ^ seed, _waterr08(p + 2) ^ _waterp2); break;
            h = watermum(ieor(waterr16(p(i+1:i+2)), h), &
                &        ieor(waterr08(p(i+3:i+3)), waterp2))
        case(4)
            !seed = _watermum(_waterr16(p) ^ seed, _waterr16(p + 2) ^ _waterp3); break;
            h = watermum(ieor(waterr16(p(i+1:i+2)), h), &
                &        ieor(waterr16(p(i+3:i+4)), waterp3))
        case(5)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr08(p + 4) ^ _waterp1); break;
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(waterr08(p(i+5:i+5)), waterp1))
        case(6)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr16(p + 4) ^ _waterp1); break;
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(waterr16(p(i+5:i+6)), waterp1))
        case(7)
            !seed = _watermum(_waterr32(p) ^ seed, (_waterr16(p + 4) << 8 | _waterr08(p + 6)) ^ _waterp1); break;
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(ior(ishft(waterr16(p(i+5:i+6)), 8), waterr08(p(i+7:i+7))), &
                &             waterp1))
        case(8)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp0); break;
            h = watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &        ieor(waterr32(p(i+5:i+8)), waterp0))
        case(9)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp2) ^ _watermum(seed ^ _waterp4, _waterr08(p + 8) ^ _waterp3); break;
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterp4), &
                &             ieor(waterr08(p(i+9:i+9)), waterp3)))
        case(10)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp2) ^ _watermum(seed, _waterr16(p + 8) ^ _waterp3); break;
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(h, &
                &             ieor(waterr16(p(i+9:i+10)), waterp3)))
        case(11)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp2) ^ _watermum(seed, ((_waterr16(p + 8) << 8) | _waterr08(p + 10)) ^ _waterp3); break;
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(h, &
                &             ieor(ior(ishft(waterr16(p(i+9:i+10)), 8), &
                &                      waterr08(p(i+11:i+11))), &
                &                  waterp3)))
        case(12)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp2) ^ _watermum(seed ^ _waterr32(p + 8), _waterp4); break;
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             waterp4))
        case(13)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp2) ^ _watermum(seed ^ _waterr32(p + 8), (_waterr08(p + 12)) ^ _waterp4); break;
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             ieor(waterr08(p(i+13:i+13)), waterp4)))
        case(14)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp2) ^ _watermum(seed ^ _waterr32(p + 8), (_waterr16(p + 12)) ^ _waterp4); break;
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             ieor(waterr16(p(i+13:i+14)), waterp4)))
        case(15)
            !seed = _watermum(_waterr32(p) ^ seed, _waterr32(p + 4) ^ _waterp2) ^ _watermum(seed ^ _waterr32(p + 8), (_waterr16(p + 12) << 8 | _waterr08(p + 14)) ^ _waterp4); break;
            h = ieor(watermum(ieor(waterr32(p(i+1:i+4)), h), &
                &             ieor(waterr32(p(i+5:i+8)), waterp2)), &
                &    watermum(ieor(h, waterr32(p(i+9:i+12))), &
                &             ieor(ior(ishft(waterr16(p(i+13:i+14)), 8), &
                &                      waterr08(p(i+15:i+15))), &
                &                  waterp4)))
        end select

        h = ieor(h, ishft(h, 16)) * ieor(int(len, i8), waterp0)
        val = transfer(h - ishft(h, -32), val)

    end function waterhash

end module stdlib_hash
