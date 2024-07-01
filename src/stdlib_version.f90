! SPDX-Identifier: MIT


!> Version information on stdlib
module stdlib_version
    implicit none
    private

    public :: get_stdlib_version
    public :: stdlib_version_string, stdlib_version_compact


    !> String representation of the standard library version
    character(len=*), parameter :: stdlib_version_string = "0.7.0"

    !> Major version number of the above standard library version
    integer, parameter :: stdlib_major = 0

    !> Minor version number of the above standard library version
    integer, parameter :: stdlib_minor = 7

    !> Patch version number of the above standard library version
    integer, parameter :: stdlib_patch = 0

    !> Compact numeric representation of the standard library version
    integer, parameter :: stdlib_version_compact = &
        & stdlib_major*10000 + stdlib_minor*100 + stdlib_patch


contains


!> Getter function to retrieve standard library version
pure subroutine get_stdlib_version(major, minor, patch, string)

    !> Major version number of the standard library version
    integer, intent(out), optional :: major

    !> Minor version number of the standard library version
    integer, intent(out), optional :: minor

    !> Patch version number of the standard library version
    integer, intent(out), optional :: patch

    !> String representation of the standard library version
    character(len=:), allocatable, intent(out), optional :: string

    if (present(major)) then
        major = stdlib_major
    end if
    if (present(minor)) then
        minor = stdlib_minor
    end if
    if (present(patch)) then
        patch = stdlib_patch
    end if
    if (present(string)) then
        string = stdlib_version_string
    end if

end subroutine get_stdlib_version


end module stdlib_version
