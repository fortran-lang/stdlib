program test_ascii

    use stdlib_experimental_error, only: assert
    use stdlib_experimental_ascii, only: lowercase, uppercase, digits, &
        octal_digits, fullhex_digits, hex_digits, lowerhex_digits, &
        whitespace, letters, is_alphanum, is_alpha, is_lower, is_upper, &
        is_digit, is_octal_digit, is_hex_digit, is_white, is_blank, &
        is_control, is_punctuation, is_graphical, is_printable, is_ascii, &
        to_lower, to_upper, LF, TAB, NUL, DEL

    implicit none

    print *, "Lowercase letters: ", lowercase
    print *, "Uppercase letters: ", uppercase
    print *, "Digits: ", digits
    print *, "Octal digits: ", octal_digits
    print *, "Full hex digits: ", fullhex_digits
    print *, "Hex digits: ", hex_digits
    print *, "Lower hex digits: ", lowerhex_digits

    call test_is_alphanum_short
    call test_is_alphanum_long

    call test_is_alpha_short
    call test_is_alpha_long

    call test_is_lower_short
    call test_is_lower_long

    call test_is_upper_short
    call test_is_upper_long

    call test_is_digit_short
    call test_is_digit_long

    call test_is_octal_digit_short
    call test_is_octal_digit_long

    call test_is_hex_digit_short
    call test_is_hex_digit_long

    call test_is_white_short
    call test_is_white_long

    call test_is_blank_short
    call test_is_blank_long

    call test_is_control_short
    call test_is_control_long

    call test_is_punctuation_short
    call test_is_punctuation_long

    call test_is_graphical_short
    call test_is_graphical_long

    call test_is_printable_short
    call test_is_printable_long

    call test_is_ascii_short
    call test_is_ascii_long

    call test_to_lower_short
    call test_to_lower_long

    call test_to_upper_short
    call test_to_upper_long

    ! call test_ascii_table

contains

    subroutine test_is_alphanum_short
        print *, "test_is_alphanum_short"
        call assert(is_alphanum('A'))
        call assert(is_alphanum('1'))
        call assert(.not. is_alphanum('#'))

        ! N.B.: does not return true for non-ASCII Unicode alphanumerics
        call assert(.not. is_alphanum('á'))
    end subroutine

    subroutine test_is_alphanum_long
        integer :: i
        character(len=:), allocatable :: clist

        print *, "test_is_alphanum_long"

        clist = digits//octal_digits//fullhex_digits//letters//lowercase//uppercase
        do i = 1, len(clist)
            call assert(is_alphanum(clist(i:i)))
        end do

        clist = whitespace
        do i = 1, len(clist)
            call assert(.not. is_alphanum(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_alpha_short
        print *, "test_is_alpha_short"
        call assert(is_alpha('A'))
        call assert(.not. is_alpha('1'))
        call assert(.not. is_alpha('#'))

        ! N.B.: does not return true for non-ASCII Unicode alphabetic characters
        call assert(.not. is_alpha('á'))
    end subroutine

    subroutine test_is_alpha_long
        integer :: i
        character(len=:), allocatable :: clist

        print *, "test_is_alpha_long"

        clist = letters//lowercase//uppercase
        do i = 1, len(clist)
            call assert(is_alpha(clist(i:i)))
        end do

        clist = digits//octal_digits//whitespace
        do i = 1, len(clist)
            call assert(.not. is_alpha(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_lower_short
        print *, "test_is_lower_short"
        call assert(is_lower('a'))
        call assert(.not. is_lower('A'))
        call assert(.not. is_lower('#'))

        ! N.B.: does not return true for non-ASCII Unicode lowercase letters
        call assert(.not. is_lower('á'))
        call assert(.not. is_lower('Á'))
    end subroutine

    subroutine test_is_lower_long
        integer :: i
        character(len=:), allocatable :: clist

        print *, "test_is_lower_long"
        do i = 1, len(lowercase)
            call assert(is_lower(lowercase(i:i)))
        end do

        clist = digits//uppercase//whitespace
        do i = 1, len(clist)
            call assert(.not. is_lower(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_upper_short
        print *, "test_is_upper_short"
        call assert(is_upper('A'))
        call assert(.not. is_upper('a'))
        call assert(.not. is_upper('#'))

        ! N.B.: does not return true for non-ASCII Unicode uppercase letters
        call assert(.not. is_upper('á'))
        call assert(.not. is_upper('Á'))
    end subroutine

    subroutine test_is_upper_long
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_upper_long"
        do i = 1, len(uppercase)
            call assert(is_upper(uppercase(i:i)))
        end do

        clist = digits//lowercase//whitespace
        do i = 1, len(clist)
            call assert(.not. is_upper(clist(i:i)))
        end do
    end subroutine


    subroutine test_is_digit_short
        print *, "test_is_digit_short"
        call assert(is_digit('3'))
        call assert(is_digit('8'))
        call assert(.not. is_digit('B'))
        call assert(.not. is_digit('#'))

        ! N.B.: does not return true for non-ASCII Unicode numbers
        call assert(.not. is_digit('０')) ! full-width digit zero (U+FF10)
        call assert(.not. is_digit('４')) ! full-width digit four (U+FF14))
    end subroutine

    subroutine test_is_digit_long
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_digit_long"
        do i = 1, len(digits)
            call assert(is_digit(digits(i:i)))
        end do

        clist = letters//whitespace
        do i = 1, len(clist)
            call assert(.not. is_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_octal_digit_short
        print *, "test_is_octal_digit_short"
        call assert(is_octal_digit('0'))
        call assert(is_octal_digit('7'))
        call assert(.not. is_octal_digit('8'))
        call assert(.not. is_octal_digit('A'))
        call assert(.not. is_octal_digit('#'))
    end subroutine

    subroutine test_is_octal_digit_long
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_octal_digit_long"
        do i = 1, len(octal_digits)
            call assert(is_octal_digit(octal_digits(i:i)))
        end do
        clist = letters//'89'//whitespace
        do i = 1, len(clist)
            call assert(.not. is_octal_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_hex_digit_short
        print *, "test_is_hex_digit_short"
        call assert(is_hex_digit('0'))
        call assert(is_hex_digit('A'))
        call assert(is_hex_digit('f')) !! lowercase hex digits are accepted
        call assert(.not. is_hex_digit('g'))
        call assert(.not. is_hex_digit('G'))
        call assert(.not. is_hex_digit('#'))
    end subroutine

    subroutine test_is_hex_digit_long
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_hex_digit_long"
        do i = 1, len(fullhex_digits)
            call assert(is_hex_digit(fullhex_digits(i:i)))
        end do
        clist = lowercase(7:)//uppercase(7:)//whitespace
        do i = 1, len(clist)
            call assert(.not. is_hex_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_white_short
        print *, "test_is_white_short"
        call assert(is_white(' '))
        call assert(is_white(TAB))
        call assert(is_white(LF))
        call assert(.not. is_white('1'))
        call assert(.not. is_white('a'))
        call assert(.not. is_white('#'))
    end subroutine

    subroutine test_is_white_long
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_white_long"
        do i = 1, len(whitespace)
            call assert(is_white(whitespace(i:i)))
        end do
        clist = digits//letters
        do i = 1, len(clist)
            call assert(.not. is_white(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_blank_short
        print *, "test_is_blank_short"
        call assert(is_blank(' '))
        call assert(is_blank(TAB))
        call assert(.not. is_blank('1'))
        call assert(.not. is_blank('a'))
        call assert(.not. is_blank('#'))
    end subroutine

    subroutine test_is_blank_long
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_blank_long"
        do i = 1, len(whitespace)
            if (whitespace(i:i) == ' ' .or. whitespace(i:i) == TAB) then
                call assert(is_blank(whitespace(i:i)))
            else
                call assert(.not. is_blank(whitespace(i:i)))
            end if
        end do
        clist = digits//letters
        do i = 1, len(clist)
            call assert(.not. is_blank(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_control_short
        print *, "test_is_control_short"
        ! print *, is_control('\0')
        ! print *, is_control('\022')
        call assert(is_control(new_line('a'))) ! newline is both whitespace and control
        call assert(.not. is_control(' '))
        call assert(.not. is_control('1'))
        call assert(.not. is_control('a'))
        call assert(.not. is_control('#'))

        ! N.B.: non-ASCII Unicode control characters are not recognized:
        ! print *, .not. is_control('\u0080')
        ! print *, .not. is_control('\u2028')
        ! print *, .not. is_control('\u2029')
    end subroutine

    subroutine test_is_control_long
        integer :: i
        character(len=:), allocatable :: clist
        print *, "test_is_control_long"
        do i = 0, 31
            call assert(is_control(achar(i)))
        end do
        call assert(is_control(DEL))

        clist = digits//letters//' '
        do i = 1, len(clist)
            call assert(.not. is_control(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_punctuation_short
        print *, "test_is_punctuation_short"
        call assert(is_punctuation('.'))
        call assert(is_punctuation(','))
        call assert(is_punctuation(':'))
        call assert(is_punctuation('!'))
        call assert(is_punctuation('#'))
        call assert(is_punctuation('~'))
        call assert(is_punctuation('+'))
        call assert(is_punctuation('_'))

        call assert(.not. is_punctuation('1'))
        call assert(.not. is_punctuation('a'))
        call assert(.not. is_punctuation(' '))
        call assert(.not. is_punctuation(LF)) ! new line character
        call assert(.not. is_punctuation(NUL))

        ! N.B.: Non-ASCII Unicode punctuation characters are not recognized.
        ! print *, is_punctuation('\u2012') ! (U+2012 = en-dash)
    end subroutine

    subroutine test_is_punctuation_long
        integer :: i
        character(len=1) :: c
        print *, "test_is_punctuation_long"
        do i = 0, 127
            c = achar(i)
            if (is_control(c) .or. is_alphanum(c) .or. c == ' ') then
                call assert(.not. is_punctuation(c))
            else
                call assert(is_punctuation(c))
            end if
        end do
    end subroutine

    subroutine test_is_graphical_short
        print *, "test_is_graphical"
        call assert(is_graphical('1'))
        call assert(is_graphical('a'))
        call assert(is_graphical('#'))
        call assert(.not. is_graphical(' ')) ! whitespace is not graphical
        call assert(.not. is_graphical(LF))
        call assert(.not. is_graphical(NUL))

        ! N.B.: Unicode graphical characters are not regarded as such.
        call assert(.not. is_graphical('ä'))
    end subroutine

    subroutine test_is_graphical_long
        integer :: i
        character(len=1) :: c
        print *, "test_is_graphical_long"
        do i = 0, 127
            c = achar(i)
            if (is_control(c) .or. c == ' ') then
                call assert(.not. is_graphical(c))
            else
                call assert(is_graphical(c))
            end if
        end do
    end subroutine

    subroutine test_is_printable_short
        print *, "test_is_printable_short"
        call assert(is_printable(' ')) ! whitespace is printable
        call assert(is_printable('1'))
        call assert(is_printable('a'))
        call assert(is_printable('#'))
        call assert(.not. is_printable(NUL)) ! control characters are not printable

        ! N.B.: Printable non-ASCII Unicode characters are not recognized.
        call assert(.not. is_printable('ä'))
    end subroutine

    subroutine test_is_printable_long
        integer :: i
        character(len=1) :: c
        print *, "test_is_printable_long"
        do i = 0, 127
            c = achar(i)
            if (is_control(c)) then
                call assert(.not. is_printable(c))
            else
                call assert(is_printable(c))
            end if
        end do
    end subroutine

    subroutine test_is_ascii_short()
        print *, "test_is_ascii_short"
        call assert(is_ascii('a'))
        call assert(.not. is_ascii('ä'))
    end subroutine

    subroutine test_is_ascii_long()
        integer :: i
        print *, "test_is_ascii_long"
        do i = 0, 127
            call assert(is_ascii(achar(i)))
        end do
        call assert(.not. is_ascii(achar(128))) ! raises compiler warning

    end subroutine

    subroutine test_to_lower_short()
        print *, "test_to_lower_short"
        call assert(to_lower('a') == 'a')
        call assert(to_lower('A') == 'a')
        call assert(to_lower('#') == '#')
    end subroutine

    subroutine test_to_lower_long()
        integer :: i
        character(len=1) :: c
        print *, "test_to_lower_long"
        do i = 1, len(uppercase)
            call assert(to_lower(uppercase(i:i)) == lowercase(i:i))
        end do
        do i = 0, 127
            c = achar(i)
            if (c < 'A' .or. c > 'Z') then
                call assert(to_lower(c) == c)
            else
                call assert(to_lower(c) /= c)
            end if
        end do
    end subroutine

    subroutine test_to_upper_short()
        print *, "test_to_upper_short"
        call assert(to_upper('a') == 'A')
        call assert(to_upper('A') == 'A')
        call assert(to_upper('#') == '#')
    end subroutine

    subroutine test_to_upper_long()
        integer :: i
        character(len=1) :: c
        print *, "test_to_upper_long"
        do i = 1, len(lowercase)
            call assert(to_upper(lowercase(i:i)) == uppercase(i:i))
        end do

        do i = 0, 127
            c = achar(i)
            if (c < 'a' .or. c > 'z') then
                call assert(to_upper(c) == c)
            else
                call assert(to_upper(c) /= c)
            end if
        end do
    end subroutine

    !
    !   This test reproduces the true/false table found at
    !   https://en.cppreference.com/w/cpp/string/byte
    !
    subroutine test_ascii_table
        integer :: i, j
        character(len=1) :: c
        logical :: table(15,12)

        abstract interface
            pure logical function validation_func_interface(c)
                character(len=1), intent(in) :: c
            end function
        end interface

        type :: proc_pointer_array
            procedure(validation_func_interface), pointer, nopass :: pcf
        end type proc_pointer_array

        type(proc_pointer_array) :: pcfs(12)

        pcfs(1)%pcf => is_control
        pcfs(2)%pcf => is_printable
        pcfs(3)%pcf => is_white
        pcfs(4)%pcf => is_blank
        pcfs(5)%pcf => is_graphical
        pcfs(6)%pcf => is_punctuation
        pcfs(7)%pcf => is_alphanum
        pcfs(8)%pcf => is_alpha
        pcfs(9)%pcf => is_upper
        pcfs(10)%pcf => is_lower
        pcfs(11)%pcf => is_digit
        pcfs(12)%pcf => is_hex_digit

        ! loop through functions
        do i = 1, 12
            table(1,i)  = all([(pcfs(i)%pcf(achar(j)),j=0,8)])      ! control codes
            table(2,i)  = pcfs(i)%pcf(achar(9))                     ! tab
            table(3,i)  = all([(pcfs(i)%pcf(achar(j)),j=10,13)])    ! whitespaces
            table(4,i)  = all([(pcfs(i)%pcf(achar(j)),j=14,31)])    ! control codes
            table(5,i)  = pcfs(i)%pcf(achar(32))                    ! space
            table(6,i)  = all([(pcfs(i)%pcf(achar(j)),j=33,47)])    ! !"#$%&'()*+,-./
            table(7,i)  = all([(pcfs(i)%pcf(achar(j)),j=48,57)])    ! 0123456789
            table(8,i)  = all([(pcfs(i)%pcf(achar(j)),j=58,64)])    ! :;<=>?@
            table(9,i)  = all([(pcfs(i)%pcf(achar(j)),j=65,70)])    ! ABCDEF
            table(10,i) = all([(pcfs(i)%pcf(achar(j)),j=71,90)])    ! GHIJKLMNOPQRSTUVWXYZ
            table(11,i) = all([(pcfs(i)%pcf(achar(j)),j=91,96)])    ! [\]^_`
            table(12,i) = all([(pcfs(i)%pcf(achar(j)),j=97,102)])   ! abcdef
            table(13,i) = all([(pcfs(i)%pcf(achar(j)),j=103,122)])  ! ghijklmnopqrstuvwxyz
            table(14,i) = all([(pcfs(i)%pcf(achar(j)),j=123,126)])  ! {|}~
            table(15,i) = pcfs(i)%pcf(achar(127))                   ! backspace character
        end do

        ! output table for verification
        write(*,'(5X,12(I4))') (i,i=1,12)
        do j = 1, 15
            write(*,'(I3,2X,12(L4),2X,I3)') j, (table(j,i),i=1,12), count(table(j,:))
        end do
        write(*,'(5X,12(I4))') (count(table(:,i)),i=1,12)
    end subroutine

end program