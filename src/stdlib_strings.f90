! SPDX-Identifier: MIT

!> This module implements basic string handling routines.
!>
!> The specification of this module is available [here](../page/specs/stdlib_strings.html).
module stdlib_strings
    use stdlib_ascii, only : whitespace
    use stdlib_string_type, only : string_type, char
    implicit none
    private

    public :: strip, chomp


    !> Remove leading and trailing whitespace characters.
    !>
    !> Version: experimental
    interface strip
        module procedure :: strip_string
        module procedure :: strip_char
    end interface strip

    !> Remove trailing characters in set from string.
    !> If no character set is provided trailing whitespace is removed.
    !>
    !> Version: experimental
    interface chomp
        module procedure :: chomp_string
        module procedure :: chomp_char
        module procedure :: chomp_string_string
        module procedure :: chomp_char_string
        module procedure :: chomp_string_char
        module procedure :: chomp_char_char
    end interface chomp


contains


    !> Remove leading and trailing whitespace characters.
    pure function strip_string(string) result(stripped_string)
        ! Avoid polluting the module scope and use the assignment only in this scope 
        use stdlib_string_type, only : assignment(=)
        type(string_type), intent(in) :: string
        type(string_type) :: stripped_string

        stripped_string = strip(char(string))
    end function strip_string

    !> Remove leading and trailing whitespace characters.
    pure function strip_char(string) result(stripped_string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: stripped_string
        integer :: first, last

        first = verify(string, whitespace)
        if (first == 0) then
           stripped_string = ""
        else
           last = verify(string, whitespace, back=.true.)
           stripped_string = string(first:last)
        end if

    end function strip_char


    !> Remove trailing characters in set from string.
    !> Default character set variant where trailing whitespace is removed.
    pure function chomp_string(string) result(chomped_string)
        ! Avoid polluting the module scope and use the assignment only in this scope 
        use stdlib_string_type, only : assignment(=)
        type(string_type), intent(in) :: string
        type(string_type) :: chomped_string

        chomped_string = chomp(char(string), whitespace)
    end function chomp_string

    !> Remove trailing characters in set from string.
    !> Default character set variant where trailing whitespace is removed.
    pure function chomp_char(string) result(chomped_string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: chomped_string

        chomped_string = chomp(string, whitespace)
    end function chomp_char

    !> Remove trailing characters in set from string.
    pure function chomp_string_string(string, set) result(chomped_string)
        ! Avoid polluting the module scope and use the assignment only in this scope 
        use stdlib_string_type, only : assignment(=)
        type(string_type), intent(in) :: string
        type(string_type), intent(in) :: set
        type(string_type) :: chomped_string

        chomped_string = chomp(char(string), char(set))
    end function chomp_string_string

    !> Remove trailing characters in set from string.
    pure function chomp_string_char(string, set) result(chomped_string)
        ! Avoid polluting the module scope and use the assignment only in this scope 
        use stdlib_string_type, only : assignment(=)
        type(string_type), intent(in) :: string
        character(len=*), intent(in) :: set
        type(string_type) :: chomped_string

        chomped_string = chomp(char(string), set)
    end function chomp_string_char

    !> Remove trailing characters in set from string.
    pure function chomp_char_string(string, set) result(chomped_string)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: set
        character(len=:), allocatable :: chomped_string

        chomped_string = chomp(string, char(set))
    end function chomp_char_string

    !> Remove trailing characters in set from string.
    pure function chomp_char_char(string, set) result(chomped_string)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: set
        character(len=:), allocatable :: chomped_string
        integer :: last

        last = verify(string, set, back=.true.)
        chomped_string = string(1:last)

    end function chomp_char_char


end module stdlib_strings
