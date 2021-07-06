! SPDX-Identifier: MIT

!> Provides interfaces and implementations for hashing algorithms.
!>
!> The specification of this module is available [here](../page/specs/stdlib_hash.html).
module stdlib_hash
    use stdlib_kinds, only : i1 => int8, i2 => int16, i4 => int32, i8 => int64
    use stdlib_string_type, only : string_type, char, len
    implicit none
    private

    public :: waterhash


    !> Version: experimental
    !>
    !> Waterhash function to obtain a hashed value of the provided variable
    interface waterhash
        pure module function waterhash_impl(p, seed) result(val)
            integer(i1), intent(in) :: p(:)
            integer(i8), intent(in) :: seed
            integer(i4) :: val
        end function waterhash_impl
        module procedure :: waterhash_character
        module procedure :: waterhash_string
    end interface waterhash


contains


    !> Interface to the waterhash implementation for character scalars
    pure function waterhash_character(var, seed) result(val)
        !> Variable to hash
        character(len=*), intent(in) :: var
        !> Seed for the hashing function
        integer(i8), intent(in) :: seed
        !> Hash value
        integer(i4) :: val

        integer(i1), parameter :: t = 0_i1

        val = waterhash(transfer(var, t, len(var)), seed)

    end function waterhash_character


    !> Interface to the waterhash implementation for character scalars
    pure function waterhash_string(var, seed) result(val)
        !> Variable to hash
        type(string_type), intent(in) :: var
        !> Seed for the hashing function
        integer(i8), intent(in) :: seed
        !> Hash value
        integer(i4) :: val

        integer(i1), parameter :: t = 0_i1

        val = waterhash(transfer(char(var), t, len(var)), seed)

    end function waterhash_string


end module stdlib_hash
