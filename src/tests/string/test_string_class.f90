!> Minimal implementation of a string based on the stdlib string abstract base class
module string_implementation
    use stdlib_string_class, only : string_class, &
        len, len_trim, trim, index, scan, verify, repeat, adjustr, adjustl, &
        lgt, lge, llt, lle, char, ichar, iachar
    implicit none
    private

    public :: my_string_type
    public :: len, len_trim, trim, index, scan, verify, repeat, adjustr, adjustl
    public :: lgt, lge, llt, lle, char, ichar, iachar

    !> Definition of a string class implementation
    type, extends(string_class) :: my_string_type
        private
        character(len=:), allocatable :: raw
    contains
        !> Assign a character sequence to a string object.
        procedure :: assign_object_char
        !> Returns the length of the character sequence represented by the string.
        procedure :: get_len
        !> Returns the length of the character sequence without trailing spaces
        !> represented by the string.
        procedure :: get_len_trim
        !> Return the character sequence represented by the string.
        procedure :: get_char
        !> Return the character sequence represented by the string.
        procedure :: get_char_pos
        !> Return the character sequence represented by the string.
        procedure :: get_char_range
    end type my_string_type

    !> Constructor for string class implementation
    interface my_string_type
        module procedure :: new_string
    end interface my_string_type

contains

    !> Constructor for new string instances from a scalar character value.
    elemental function new_string(string) result(new)
        character(len=*), intent(in), optional :: string
        type(my_string_type) :: new
        if (present(string)) then
            new%raw = string
        end if
    end function new_string

    !> Assign a character sequence to a string object.
    elemental subroutine assign_object_char(lhs, rhs)
        class(my_string_type), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%raw = rhs
    end subroutine assign_object_char

    !> Returns the length of the character sequence represented by the string.
    elemental function get_len(self) result(val)
        class(my_string_type), intent(in) :: self
        integer :: val
        val = merge(len(self%raw), 0, allocated(self%raw))
    end function get_len

    !> Returns the length of the character sequence without trailing spaces
    !> represented by the string.
    elemental function get_len_trim(self) result(val)
        class(my_string_type), intent(in) :: self
        integer :: val
        val = merge(len_trim(self%raw), 0, allocated(self%raw))
    end function get_len_trim

    !> Return the character sequence represented by the string.
    pure function get_char(self) result(character_string)
        class(my_string_type), intent(in) :: self
        character(len=:), allocatable :: character_string
        if (allocated(self%raw)) then
            character_string = self%raw
        else
            character_string = ""
        end if
    end function get_char

    !> Return the character sequence represented by the string.
    elemental function get_char_pos(self, pos) result(character_string)
        class(my_string_type), intent(in) :: self
        integer, intent(in) :: pos
        character(len=1) :: character_string
        if (allocated(self%raw)) then
            character_string = self%raw(pos:pos)
        else
            character_string = ""
        end if
    end function get_char_pos

    !> Return the character sequence represented by the string.
    pure function get_char_range(self, start, last) result(character_string)
        class(my_string_type), intent(in) :: self
        integer, intent(in) :: start
        integer, intent(in) :: last
        character(len=last-start+1) :: character_string
        if (allocated(self%raw)) then
            character_string = self%raw(start:last)
        else
            character_string = ""
        end if
    end function get_char_range

end module string_implementation

! SPDX-Identifer: MIT
module test_string_class
    use stdlib_error, only : check
    use stdlib_string_type
    use string_implementation
    implicit none

    abstract interface
        !> Actual tester working on a string type and a fixed length character
        !> representing the same character sequence
        subroutine check1_interface(str1, chr1)
            import :: my_string_type
            type(my_string_type), intent(in) :: str1
            character(len=*), intent(in) :: chr1
        end subroutine check1_interface

        !> Actual tester working on two pairs of string type and fixed length
        !> character representing the same character sequences
        subroutine check2_interface(str1, chr1, str2, chr2)
            import :: my_string_type
            type(my_string_type), intent(in) :: str1, str2
            character(len=*), intent(in) :: chr1, chr2
        end subroutine check2_interface
    end interface

contains

    !> Generate then checker both for the string type created from the character
    !> sequence by the contructor and the assignment operation
    subroutine check1(chr1, checker)
        character(len=*), intent(in) :: chr1
        procedure(check1_interface) :: checker
        call constructor_check1(chr1, checker)
        call assignment_check1(chr1, checker)
    end subroutine check1

    !> Run the actual checker with a string type generated by the custom constructor
    subroutine constructor_check1(chr1, checker)
        character(len=*), intent(in) :: chr1
        procedure(check1_interface) :: checker
        call checker(my_string_type(chr1), chr1)
    end subroutine constructor_check1

    !> Run the actual checker with a string type generated by assignment
    subroutine assignment_check1(chr1, checker)
        character(len=*), intent(in) :: chr1
        type(my_string_type) :: str1
        procedure(check1_interface) :: checker
        str1 = chr1
        call checker(str1, chr1)
    end subroutine assignment_check1

    !> Generate then checker both for the string type created from the character
    !> sequence by the contructor and the assignment operation as well as the
    !> mixed assigment and constructor setup
    subroutine check2(chr1, chr2, checker)
        character(len=*), intent(in) :: chr1, chr2
        procedure(check2_interface) :: checker
        call constructor_check2(chr1, chr2, checker)
        call assignment_check2(chr1, chr2, checker)
        call mixed_check2(chr1, chr2, checker)
    end subroutine check2

    !> Run the actual checker with both string types generated by the custom constructor
    subroutine constructor_check2(chr1, chr2, checker)
        character(len=*), intent(in) :: chr1, chr2
        procedure(check2_interface) :: checker
        call checker(my_string_type(chr1), chr1, my_string_type(chr2), chr2)
    end subroutine constructor_check2

    !> Run the actual checker with one string type generated by the custom constructor
    !> and the other by assignment
    subroutine mixed_check2(chr1, chr2, checker)
        character(len=*), intent(in) :: chr1, chr2
        type(my_string_type) :: str1, str2
        procedure(check2_interface) :: checker
        str1 = chr1
        str2 = chr2
        call checker(str1, chr1, my_string_type(chr2), chr2)
        call checker(my_string_type(chr1), chr1, str2, chr2)
    end subroutine mixed_check2

    !> Run the actual checker with both string types generated by assignment
    subroutine assignment_check2(chr1, chr2, checker)
        character(len=*), intent(in) :: chr1, chr2
        type(my_string_type) :: str1, str2
        procedure(check2_interface) :: checker
        str1 = chr1
        str2 = chr2
        call checker(str1, chr1, str2, chr2)
    end subroutine assignment_check2

    !> Generator for checking the lexical comparison
    subroutine gen_lgt(str1, chr1, str2, chr2)
        type(my_string_type), intent(in) :: str1, str2
        character(len=*), intent(in) :: chr1, chr2
        call check(lgt(str1, str2) .eqv. lgt(chr1, chr2))
        call check(lgt(str1, chr2) .eqv. lgt(chr1, chr2))
        call check(lgt(chr1, str2) .eqv. lgt(chr1, chr2))
    end subroutine gen_lgt

    subroutine test_lgt
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = lgt(string, "abc")
        call check(res .eqv. .true.)

        res = lgt(string, "bcd")
        call check(res .eqv. .false.)

        res = lgt(string, "cde")
        call check(res .eqv. .false.)

        call check2("bcd", "abc", gen_lgt)
        call check2("bcd", "bcd", gen_lgt)
        call check2("bcd", "cde", gen_lgt)
    end subroutine test_lgt

    !> Generator for checking the lexical comparison
    subroutine gen_llt(str1, chr1, str2, chr2)
        type(my_string_type), intent(in) :: str1, str2
        character(len=*), intent(in) :: chr1, chr2
        call check(llt(str1, str2) .eqv. llt(chr1, chr2))
        call check(llt(str1, chr2) .eqv. llt(chr1, chr2))
        call check(llt(chr1, str2) .eqv. llt(chr1, chr2))
    end subroutine gen_llt

    subroutine test_llt
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = llt(string, "abc")
        call check(res .eqv. .false.)

        res = llt(string, "bcd")
        call check(res .eqv. .false.)

        res = llt(string, "cde")
        call check(res .eqv. .true.)

        call check2("bcd", "abc", gen_llt)
        call check2("bcd", "bcd", gen_llt)
        call check2("bcd", "cde", gen_llt)
    end subroutine test_llt

    !> Generator for checking the lexical comparison
    subroutine gen_lge(str1, chr1, str2, chr2)
        type(my_string_type), intent(in) :: str1, str2
        character(len=*), intent(in) :: chr1, chr2
        call check(lge(str1, str2) .eqv. lge(chr1, chr2))
        call check(lge(str1, chr2) .eqv. lge(chr1, chr2))
        call check(lge(chr1, str2) .eqv. lge(chr1, chr2))
    end subroutine gen_lge

    subroutine test_lge
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = lge(string, "abc")
        call check(res .eqv. .true.)

        res = lge(string, "bcd")
        call check(res .eqv. .true.)

        res = lge(string, "cde")
        call check(res .eqv. .false.)

        call check2("bcd", "abc", gen_lge)
        call check2("bcd", "bcd", gen_lge)
        call check2("bcd", "cde", gen_lge)
    end subroutine test_lge

    !> Generator for checking the lexical comparison
    subroutine gen_lle(str1, chr1, str2, chr2)
        type(my_string_type), intent(in) :: str1, str2
        character(len=*), intent(in) :: chr1, chr2
        call check(lle(str1, str2) .eqv. lle(chr1, chr2))
        call check(lle(str1, chr2) .eqv. lle(chr1, chr2))
        call check(lle(chr1, str2) .eqv. lle(chr1, chr2))
    end subroutine gen_lle

    subroutine test_lle
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = lle(string, "abc")
        call check(res .eqv. .false.)

        res = lle(string, "bcd")
        call check(res .eqv. .true.)

        res = lle(string, "cde")
        call check(res .eqv. .true.)

        call check2("bcd", "abc", gen_lle)
        call check2("bcd", "bcd", gen_lle)
        call check2("bcd", "cde", gen_lle)
    end subroutine test_lle

    !> Generator for checking the trimming of whitespace
    subroutine gen_trim(str1, chr1)
        type(my_string_type), intent(in) :: str1
        character(len=*), intent(in) :: chr1
        call check(len(trim(str1)) == len(trim(chr1)))
    end subroutine gen_trim

    subroutine test_trim
       type(my_string_type) :: string, trimmed_str

       string = "Whitespace                            "
       trimmed_str = trim(string)
       call check(len(trimmed_str) == 10)

       call check1(" Whitespace  ", gen_trim)
       call check1(" W h i t e s p a ce  ", gen_trim)
       call check1("SPACE    SPACE", gen_trim)
       call check1("                           ", gen_trim)
    end subroutine test_trim

    !> Generator for checking the length of the character sequence
    subroutine gen_len(str1, chr1)
        type(my_string_type), intent(in) :: str1
        character(len=*), intent(in) :: chr1
        call check(len(str1) == len(chr1))
    end subroutine gen_len

    subroutine test_len
        type(my_string_type) :: string
        integer :: length

        string = "Some longer sentence for this example."
        length = len(string)
        call check(length == 38)

        string = "Whitespace                            "
        length = len(string)
        call check(length == 38)

        call check1("Example string", gen_len)
        call check1("S P A C E D   S T R I N G", gen_len)
        call check1("With trailing whitespace               ", gen_len)
        call check1("     centered      ", gen_len)
    end subroutine test_len

    !> Generator for checking the length of the character sequence without whitespace
    subroutine gen_len_trim(str1, chr1)
        type(my_string_type), intent(in) :: str1
        character(len=*), intent(in) :: chr1
        call check(len_trim(str1) == len_trim(chr1))
    end subroutine gen_len_trim

    subroutine test_len_trim
        type(my_string_type) :: string
        integer :: length

        string = "Some longer sentence for this example."
        length = len_trim(string)
        call check(length == 38)

        string = "Whitespace                            "
        length = len_trim(string)
        call check(length == 10)

        call check1("Example string", gen_len_trim)
        call check1("S P A C E D   S T R I N G", gen_len_trim)
        call check1("With trailing whitespace               ", gen_len_trim)
        call check1("     centered      ", gen_len_trim)
    end subroutine test_len_trim

    !> Generator for checking the left adjustment of the character sequence
    subroutine gen_adjustl(str1, chr1)
        type(my_string_type), intent(in) :: str1
        character(len=*), intent(in) :: chr1
        call check(adjustl(str1) == adjustl(chr1))
    end subroutine gen_adjustl

    subroutine test_adjustl
        type(my_string_type) :: string

        string = "                            Whitespace"
        string = adjustl(string)
        call check(char(string) == "Whitespace                            ")

        call check1("           B L A N K S        ", gen_adjustl)
    end subroutine test_adjustl

    !> Generator for checking the right adjustment of the character sequence
    subroutine gen_adjustr(str1, chr1)
        type(my_string_type), intent(in) :: str1
        character(len=*), intent(in) :: chr1
        call check(adjustr(str1) == adjustr(chr1))
    end subroutine gen_adjustr

    subroutine test_adjustr
        type(my_string_type) :: string

        string = "Whitespace                            "
        string = adjustr(string)
        call check(char(string) == "                            Whitespace")

        call check1("           B L A N K S        ", gen_adjustr)
    end subroutine test_adjustr

    !> Generator for checking the presence of a character set in a character sequence
    subroutine gen_scan(str1, chr1, str2, chr2)
        type(my_string_type), intent(in) :: str1, str2
        character(len=*), intent(in) :: chr1, chr2
        call check(scan(str1, str2) == scan(chr1, chr2))
        call check(scan(str1, chr2) == scan(chr1, chr2))
        call check(scan(chr1, str2) == scan(chr1, chr2))
        call check(scan(str1, str2, back=.true.) == scan(chr1, chr2, back=.true.))
        call check(scan(str1, chr2, back=.true.) == scan(chr1, chr2, back=.true.))
        call check(scan(chr1, str2, back=.true.) == scan(chr1, chr2, back=.true.))
    end subroutine gen_scan

    subroutine test_scan
        type(my_string_type) :: string
        integer :: pos

        string = "fortran"
        pos = scan(string, "ao")
        call check(pos == 2)

        pos = scan(string, "ao", .true.)
        call check(pos == 6)

        pos = scan(string, "c++")
        call check(pos == 0)

        call check2("fortran", "ao", gen_scan)
        call check2("c++", "fortran", gen_scan)

    end subroutine test_scan

    !> Generator for checking the absence of a character set in a character sequence
    subroutine gen_verify(str1, chr1, str2, chr2)
        type(my_string_type), intent(in) :: str1, str2
        character(len=*), intent(in) :: chr1, chr2
        call check(verify(str1, str2) == verify(chr1, chr2))
        call check(verify(str1, chr2) == verify(chr1, chr2))
        call check(verify(chr1, str2) == verify(chr1, chr2))
        call check(verify(str1, str2, back=.true.) == verify(chr1, chr2, back=.true.))
        call check(verify(str1, chr2, back=.true.) == verify(chr1, chr2, back=.true.))
        call check(verify(chr1, str2, back=.true.) == verify(chr1, chr2, back=.true.))
    end subroutine gen_verify

    subroutine test_verify
        type(my_string_type) :: string
        integer :: pos

        string = "fortran"
        pos = verify(string, "ao")
        call check(pos == 1)

        pos = verify(string, "fo")
        call check(pos == 3)

        pos = verify(string, "c++")
        call check(pos == 1)

        pos = verify(string, "c++", back=.true.)
        call check(pos == 7)

        pos = verify(string, string)
        call check(pos == 0)

        call check2("fortran", "ao", gen_verify)
        call check2("c++", "fortran", gen_verify)

    end subroutine test_verify

    !> Generator for the repeatition of a character sequence
    subroutine gen_repeat(str1, chr1)
        type(my_string_type), intent(in) :: str1
        character(len=*), intent(in) :: chr1
        integer :: i
        do i = 12, 3, -2
            call check(repeat(str1, i) == repeat(chr1, i))
        end do
    end subroutine gen_repeat

    subroutine test_repeat
        type(my_string_type) :: string

        string = "What? "
        string = repeat(string, 3)
        call check(string == "What? What? What? ")

        call check1("!!1!", gen_repeat)
        call check1("This sentence is repeated multiple times. ", gen_repeat)

    end subroutine test_repeat

    !> Generator for checking the substring search in a character string
    subroutine gen_index(str1, chr1, str2, chr2)
        type(my_string_type), intent(in) :: str1, str2
        character(len=*), intent(in) :: chr1, chr2
        call check(index(str1, str2) == index(chr1, chr2))
        call check(index(str1, chr2) == index(chr1, chr2))
        call check(index(chr1, str2) == index(chr1, chr2))
        call check(index(str1, str2, back=.true.) == index(chr1, chr2, back=.true.))
        call check(index(str1, chr2, back=.true.) == index(chr1, chr2, back=.true.))
        call check(index(chr1, str2, back=.true.) == index(chr1, chr2, back=.true.))
    end subroutine gen_index

    subroutine test_index
        type(my_string_type) :: string
        integer :: pos

        string = "Search this string for this expression"
        pos = index(string, "this")
        call check(pos == 8)

        pos = index(string, "this", back=.true.)
        call check(pos == 24)

        pos = index(string, "This")
        call check(pos == 0)

        call check2("Search this string for this expression", "this", gen_index)
        call check2("Search this string for this expression", "This", gen_index)

    end subroutine test_index

    subroutine test_char
        type(my_string_type) :: string
        character(len=:), allocatable :: dlc
        character(len=1), allocatable :: chars(:)

        string = "Character sequence"
        dlc = char(string)
        call check(dlc == "Character sequence")

        dlc = char(string, 3)
        call check(dlc == "a")
        chars = char(string, [3, 5, 8, 12, 14, 15, 18])
        call check(all(chars == ["a", "a", "e", "e", "u", "e", "e"]))

        string = "Fortran"
        dlc = char(string, 1, 4)
        call check(dlc == "Fort")
    end subroutine test_char

    subroutine test_ichar
        type(my_string_type) :: string
        integer :: code

        string = "Fortran"
        code = ichar(string)
        call check(code == ichar("F"))
    end subroutine test_ichar

    subroutine test_iachar
        type(my_string_type) :: string
        integer :: code

        string = "Fortran"
        code = iachar(string)
        call check(code == iachar("F"))
    end subroutine test_iachar

    subroutine test_listdirected_io
        type(my_string_type) :: string
        integer :: io, stat
        string = "Important saved value"

        open(newunit=io, form="formatted", status="scratch")
        write(io, *) string
        write(io, *) ! Pad with a newline or we might run into EOF while reading

        string = ""
        rewind(io)

        read(io, *, iostat=stat) string
        close(io)

        call check(stat == 0)
        call check(len(string) == 21)
        call check(string == "Important saved value")
    end subroutine test_listdirected_io

    subroutine test_formatted_io
        type(my_string_type) :: string
        integer :: io, stat
        string = "Important saved value"

        open(newunit=io, form="formatted", status="scratch")
        write(io, '(dt)') string
        write(io, '(a)') ! Pad with a newline or we might run into EOF while reading

        string = ""
        rewind(io)

        read(io, *, iostat=stat) string
        close(io)

        call check(stat == 0)
        call check(len(string) == 21)
        call check(string == "Important saved value")
    end subroutine test_formatted_io

    subroutine test_unformatted_io
        type(my_string_type) :: string
        integer :: io
        string = "Important saved value"

        open(newunit=io, form="unformatted", status="scratch")
        write(io) string

        string = ""
        rewind(io)

        read(io) string
        close(io)

        call check(len(string) == 21)
        call check(string == "Important saved value")
    end subroutine test_unformatted_io

    subroutine test_assignment
        type(my_string_type) :: string

        call check(len(string) == 0)

        string = "Sequence"
        call check(len(string) == 8)
    end subroutine test_assignment

    subroutine test_gt
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = string > "abc"
        call check(res .eqv. .true.)

        res = string > "bcd"
        call check(res .eqv. .false.)

        res = string > "cde"
        call check(res .eqv. .false.)
    end subroutine test_gt

    subroutine test_lt
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = string < "abc"
        call check(res .eqv. .false.)

        res = string < "bcd"
        call check(res .eqv. .false.)

        res = string < "cde"
        call check(res .eqv. .true.)
    end subroutine test_lt

    subroutine test_ge
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = string >= "abc"
        call check(res .eqv. .true.)

        res = string >= "bcd"
        call check(res .eqv. .true.)

        res = string >= "cde"
        call check(res .eqv. .false.)
    end subroutine test_ge

    subroutine test_le
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = string <= "abc"
        call check(res .eqv. .false.)

        res = string <= "bcd"
        call check(res .eqv. .true.)

        res = string <= "cde"
        call check(res .eqv. .true.)
    end subroutine test_le

    subroutine test_eq
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = string == "abc"
        call check(res .eqv. .false.)

        res = string == "bcd"
        call check(res .eqv. .true.)

        res = string == "cde"
        call check(res .eqv. .false.)
    end subroutine test_eq

    subroutine test_ne
        type(my_string_type) :: string
        logical :: res

        string = "bcd"
        res = string /= "abc"
        call check(res .eqv. .true.)

        res = string /= "bcd"
        call check(res .eqv. .false.)

        res = string /= "cde"
        call check(res .eqv. .true.)
    end subroutine test_ne

    subroutine test_concat
        type(my_string_type) :: string

        string = "Hello, "
        string = string // "World!"
        call check(len(string) == 13)
    end subroutine test_concat

end module test_string_class

program tester
    use test_string_class
    implicit none

    call test_lgt
    call test_llt
    call test_lge
    call test_lle
    call test_trim
    call test_len
    call test_len_trim
    call test_adjustl
    call test_adjustr
    call test_scan
    call test_verify
    call test_repeat
    call test_index
    call test_char
    call test_ichar
    call test_iachar
    call test_listdirected_io
    call test_formatted_io
    call test_unformatted_io
    call test_assignment
    call test_gt
    call test_lt
    call test_ge
    call test_le
    call test_eq
    call test_ne
    call test_concat

end program tester
