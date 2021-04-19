! SPDX-Identifier: MIT

!> This module implements basic string handling routines.
!>
!> The specification of this module is available [here](../page/specs/stdlib_strings.html).
module stdlib_strings
    use stdlib_ascii, only : whitespace
    use stdlib_string_type, only : string_type, char, verify
    implicit none
    private

    public :: strip, chomp
    public :: starts_with, ends_with


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
        module procedure :: chomp_set_string_char
        module procedure :: chomp_set_char_char
        module procedure :: chomp_substring_string_string
        module procedure :: chomp_substring_char_string
        module procedure :: chomp_substring_string_char
        module procedure :: chomp_substring_char_char
    end interface chomp


    !> Check whether a string starts with substring or not
    !>
    !> Version: experimental
    interface starts_with
        module procedure :: starts_with_string_string
        module procedure :: starts_with_string_char
        module procedure :: starts_with_char_string
        module procedure :: starts_with_char_char
    end interface starts_with


    !> Check whether a string ends with substring or not
    !>
    !> Version: experimental
    interface ends_with
        module procedure :: ends_with_string_string
        module procedure :: ends_with_string_char
        module procedure :: ends_with_char_string
        module procedure :: ends_with_char_char
    end interface ends_with


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
        integer :: last

        last = verify(string, whitespace, back=.true.)
        chomped_string = char(string, 1, last)
    end function chomp_string

    !> Remove trailing characters in set from string.
    !> Default character set variant where trailing whitespace is removed.
    pure function chomp_char(string) result(chomped_string)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: chomped_string
        integer :: last

        last = verify(string, whitespace, back=.true.)
        chomped_string = string(1:last)
    end function chomp_char

    !> Remove trailing characters in set from string.
    pure function chomp_set_string_char(string, set) result(chomped_string)
        ! Avoid polluting the module scope and use the assignment only in this scope 
        use stdlib_string_type, only : assignment(=)
        type(string_type), intent(in) :: string
        character(len=1), intent(in) :: set(:)
        type(string_type) :: chomped_string

        chomped_string = chomp(char(string), set)
    end function chomp_set_string_char

    !> Remove trailing characters in set from string.
    pure function chomp_set_char_char(string, set) result(chomped_string)
        character(len=*), intent(in) :: string
        character(len=1), intent(in) :: set(:)
        character(len=:), allocatable :: chomped_string
        integer :: last

        last = verify(string, set_to_string(set), back=.true.)
        chomped_string = string(1:last)

    end function chomp_set_char_char

    !> Remove trailing substrings from string.
    pure function chomp_substring_string_string(string, substring) result(chomped_string)
        ! Avoid polluting the module scope and use the assignment only in this scope 
        use stdlib_string_type, only : assignment(=)
        type(string_type), intent(in) :: string
        type(string_type), intent(in) :: substring
        type(string_type) :: chomped_string

        chomped_string = chomp(char(string), char(substring))
    end function chomp_substring_string_string

    !> Remove trailing substrings from string.
    pure function chomp_substring_string_char(string, substring) result(chomped_string)
        ! Avoid polluting the module scope and use the assignment only in this scope 
        use stdlib_string_type, only : assignment(=)
        type(string_type), intent(in) :: string
        character(len=*), intent(in) :: substring
        type(string_type) :: chomped_string

        chomped_string = chomp(char(string), substring)
    end function chomp_substring_string_char

    !> Remove trailing substrings from string.
    pure function chomp_substring_char_string(string, substring) result(chomped_string)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: substring
        character(len=:), allocatable :: chomped_string

        chomped_string = chomp(string, char(substring))
    end function chomp_substring_char_string

    !> Remove trailing substrings from string.
    pure function chomp_substring_char_char(string, substring) result(chomped_string)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: substring
        character(len=:), allocatable :: chomped_string
        integer :: last, nsub

        last = len(string)
        nsub = len(substring)
        if (nsub > 0) then
            do while(string(last-nsub+1:last) == substring)
                last = last - nsub
            end do
        end if
        chomped_string = string(1:last)

    end function chomp_substring_char_char

    !> Implementation to transfer a set of characters to a string representing the set.
    !>
    !> This function is internal and not part of the public API.
    pure function set_to_string(set) result(string)
        character(len=1), intent(in) :: set(:)
        character(len=size(set)) :: string

        string = transfer(set, string)
    end function set_to_string


    !> Check whether a string starts with substring or not
    pure function starts_with_char_char(string, substring) result(match)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: match
        integer :: nsub

        nsub = len(substring)
        if (len(string) < nsub) then
            match = .false.
            return
        end if
        match = string(1:nsub) == substring

    end function starts_with_char_char

    !> Check whether a string starts with substring or not
    elemental function starts_with_string_char(string, substring) result(match)
        type(string_type), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: match

        match = starts_with(char(string), substring)

    end function starts_with_string_char

    !> Check whether a string starts with substring or not
    elemental function starts_with_char_string(string, substring) result(match)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: substring
        logical :: match

        match = starts_with(string, char(substring))

    end function starts_with_char_string

    !> Check whether a string starts with substring or not
    elemental function starts_with_string_string(string, substring) result(match)
        type(string_type), intent(in) :: string
        type(string_type), intent(in) :: substring
        logical :: match

        match = starts_with(char(string), char(substring))

    end function starts_with_string_string


    !> Check whether a string ends with substring or not
    pure function ends_with_char_char(string, substring) result(match)
        character(len=*), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: match
        integer :: last, nsub

        last = len(string)
        nsub = len(substring)
        if (last < nsub) then
            match = .false.
            return
        end if
        match = string(last-nsub+1:last) == substring

    end function ends_with_char_char

    !> Check whether a string ends with substring or not
    elemental function ends_with_string_char(string, substring) result(match)
        type(string_type), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical :: match

        match = ends_with(char(string), substring)

    end function ends_with_string_char

    !> Check whether a string ends with substring or not
    elemental function ends_with_char_string(string, substring) result(match)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: substring
        logical :: match

        match = ends_with(string, char(substring))

    end function ends_with_char_string

    !> Check whether a string ends with substring or not
    elemental function ends_with_string_string(string, substring) result(match)
        type(string_type), intent(in) :: string
        type(string_type), intent(in) :: substring
        logical :: match

        match = ends_with(char(string), char(substring))

    end function ends_with_string_string


end module stdlib_strings
