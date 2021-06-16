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
    public :: slice


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
    
    !> Extracts characters from the input string to return a new string
    !> 
    !> Version: experimental
    interface slice
        module procedure :: slice_string
        module procedure :: slice_char
    end interface slice


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

    !> Extract the characters from the region between 'first' and 'last' index (both inclusive)
    !> of the input 'string' by taking strides of length 'stride'
    !> Returns a new string
    elemental function slice_string(string, first, last, stride) result(sliced_string)
        type(string_type), intent(in) :: string
        integer, intent(in), optional :: first, last, stride
        type(string_type) :: sliced_string

        sliced_string = string_type(slice(char(string), first, last, stride))

    end function slice_string

    !> Extract the characters from the region between 'first' and 'last' index (both inclusive)
    !> of the input 'string' by taking strides of length 'stride'
    !> Returns a new string
    pure function slice_char(string, first, last, stride) result(sliced_string)
        character(len=*), intent(in) :: string
        integer, intent(in), optional :: first, last, stride
        integer :: first_index, last_index, stride_vector, strides_taken, length_string, i, j
        character(len=:), allocatable :: sliced_string
        length_string = len(string)

        first_index = 0                 ! first_index = -infinity
        last_index = length_string + 1  ! last_index = +infinity
        stride_vector = 1

        if (present(stride)) then
            if (stride /= 0) then
                if (stride < 0) then
                    first_index = length_string + 1     ! first_index = +infinity
                    last_index = 0                      ! last_index = -infinity
                end if
                stride_vector = stride
            end if
        else
            if (present(first) .and. present(last)) then
                if (last < first) then
                    stride_vector = -1
                end if
            end if
        end if

        if (present(first)) then
            first_index =  first
        end if
        if (present(last)) then
            last_index = last
        end if
        
        if (stride_vector > 0) then
            first_index = max(first_index, 1)
            last_index = min(last_index, length_string)
        else
            first_index = min(first_index, length_string)
            last_index = max(last_index, 1)
        end if
        
        strides_taken = floor( real(last_index - first_index)/real(stride_vector) )
        allocate(character(len=max(0, strides_taken + 1)) :: sliced_string)
        
        j = 1
        do i = first_index, last_index, stride_vector
            sliced_string(j:j) = string(i:i)
            j = j + 1
        end do
    end function slice_char


end module stdlib_strings
