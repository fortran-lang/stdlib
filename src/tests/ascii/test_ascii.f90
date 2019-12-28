program test_ascii

    use stdlib_experimental_error, only: assert
    use stdlib_experimental_ascii, only: lowercase, uppercase, digits, &
        octal_digits, fullhex_digits, hex_digits, lowerhex_digits, &
        whitespace, letters, is_alphanum, is_alpha, is_lower, is_upper, &
        is_digit, is_octal_digit, is_hex_digit, is_white, is_blank, &
        is_control, is_punctuation, is_graphical, is_printable, is_ascii, &
        to_lower, to_upper, ascii_control_char

    write(*,*) "Lowercase letters: ", lowercase
    write(*,*) "Uppercase letters: ", uppercase
    write(*,*) "Digits: ", digits
    write(*,*) "Octal digits: ", octal_digits
    write(*,*) "Full hex digits: ", fullhex_digits
    write(*,*) "Hex digits: ", hex_digits
    write(*,*) "Lower hex digits: ", lowerhex_digits
    write(*,*)

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

    call test_ascii_table

contains

    subroutine test_is_alphanum_short
        write(*,*) "test_is_alphanum_short"
        call assert(is_alphanum('A'))
        call assert(is_alphanum('1'))
        call assert(.not. is_alphanum('#'))

        ! N.B.: does not return true for non-ASCII Unicode alphanumerics
        call assert(.not. is_alphanum('á'))
    end subroutine

    subroutine test_is_alphanum_long
        integer :: i
        character(len=:), allocatable :: clist

        write(*,*) "test_is_alphanum_long"

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
        write(*,*) "test_is_alpha_short"
        call assert(is_alpha('A'))
        call assert(.not. is_alpha('1'))
        call assert(.not. is_alpha('#'))

        ! N.B.: does not return true for non-ASCII Unicode alphabetic characters
        call assert(.not. is_alpha('á'))
    end subroutine

    subroutine test_is_alpha_long
        integer :: i
        character(len=:), allocatable :: clist

        write(*,*) "test_is_alpha_long"

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
        write(*,*) "test_is_lower_short"
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

        write(*,*) "test_is_lower_long"
        do i = 1, len(lowercase)
            call assert(is_lower(lowercase(i:i)))
        end do

        clist = digits//uppercase//whitespace
        do i = 1, len(clist)
            call assert(.not. is_lower(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_upper_short
        write(*,*) "test_is_upper_short"
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
        write(*,*) "test_is_upper_long"
        do i = 1, len(uppercase)
            call assert(is_upper(uppercase(i:i)))
        end do

        clist = digits//lowercase//whitespace
        do i = 1, len(clist)
            call assert(.not. is_upper(clist(i:i)))
        end do
    end subroutine


    subroutine test_is_digit_short
        write(*,*) "test_is_digit_short"
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
        write(*,*) "test_is_digit_long"
        do i = 1, len(digits)
            call assert(is_digit(digits(i:i)))
        end do

        clist = letters//whitespace
        do i = 1, len(clist)
            call assert(.not. is_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_octal_digit_short
        write(*,*) "test_is_octal_digit_short"
        call assert(is_octal_digit('0'))
        call assert(is_octal_digit('7'))
        call assert(.not. is_octal_digit('8'))
        call assert(.not. is_octal_digit('A'))
        call assert(.not. is_octal_digit('#'))
    end subroutine

    subroutine test_is_octal_digit_long
        integer :: i
        character(len=:), allocatable :: clist
        write(*,*) "test_is_octal_digit_long"
        do i = 1, len(octal_digits)
            call assert(is_octal_digit(octal_digits(i:i)))
        end do
        clist = letters//'89'//whitespace
        do i = 1, len(clist)
            call assert(.not. is_octal_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_hex_digit_short
        write(*,*) "test_is_hex_digit_short"
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
        write(*,*) "test_is_hex_digit_long"
        do i = 1, len(fullhex_digits)
            call assert(is_hex_digit(fullhex_digits(i:i)))
        end do
        clist = lowercase(7:)//uppercase(7:)//whitespace
        do i = 1, len(clist)
            call assert(.not. is_hex_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_white_short
        write(*,*) "test_is_white_short"
        call assert(is_white(' '))
        call assert(is_white(ascii_control_char%TAB))
        call assert(is_white(ascii_control_char%LF))
        call assert(.not. is_white('1'))
        call assert(.not. is_white('a'))
        call assert(.not. is_white('#'))
    end subroutine

    subroutine test_is_white_long
        integer :: i
        character(len=:), allocatable :: clist
        write(*,*) "test_is_white_long"
        do i = 1, len(whitespace)
            call assert(is_white(whitespace(i:i)))
        end do
        clist = digits//letters
        do i = 1, len(clist)
            call assert(.not. is_white(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_blank_short
        write(*,*) "test_is_blank_short"
        call assert(is_blank(' '))
        call assert(is_blank(ascii_control_char%TAB))
        call assert(.not. is_blank('1'))
        call assert(.not. is_blank('a'))
        call assert(.not. is_blank('#'))
    end subroutine

    subroutine test_is_blank_long
        integer :: i
        character(len=:), allocatable :: clist
        write(*,*) "test_is_blank_long"
        do i = 1, len(whitespace)
            if (whitespace(i:i) == ' ' .or. &
                whitespace(i:i) == ascii_control_char%TAB) then
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
        write(*,*) "test_is_control_short"
        ! write(*,*) is_control('\0')
        ! write(*,*) is_control('\022')
        call assert(is_control(new_line('a'))) ! newline is both whitespace and control
        call assert(.not. is_control(' '))
        call assert(.not. is_control('1'))
        call assert(.not. is_control('a'))
        call assert(.not. is_control('#'))

        ! N.B.: non-ASCII Unicode control characters are not recognized:
        ! write(*,*) .not. is_control('\u0080')
        ! write(*,*) .not. is_control('\u2028')
        ! write(*,*) .not. is_control('\u2029')
    end subroutine

    subroutine test_is_control_long
        integer :: i
        character(len=:), allocatable :: clist
        write(*,*) "test_is_control_long"
        do i = 0, 31
            call assert(is_control(achar(i)))
        end do
        call assert(is_control(ascii_control_char%DEL))

        clist = digits//letters//' '
        do i = 1, len(clist)
            call assert(.not. is_control(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_punctuation_short
        write(*,*) "test_is_punctuation_short"
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
        call assert(.not. is_punctuation(ascii_control_char%LF)) ! new line character
        call assert(.not. is_punctuation(ascii_control_char%NUL))

        ! N.B.: Non-ASCII Unicode punctuation characters are not recognized.
        ! write(*,*) is_punctuation('\u2012') ! (U+2012 = en-dash)
    end subroutine

    subroutine test_is_punctuation_long
        integer :: i
        character(len=1) :: c
        write(*,*) "test_is_punctuation_long"
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
        write(*,*) "test_is_graphical"
        call assert(is_graphical('1'))
        call assert(is_graphical('a'))
        call assert(is_graphical('#'))
        call assert(.not. is_graphical(' ')) ! whitespace is not graphical
        call assert(.not. is_graphical(ascii_control_char%LF))
        call assert(.not. is_graphical(ascii_control_char%NUL))

        ! N.B.: Unicode graphical characters are not regarded as such.
        call assert(.not. is_graphical('ä'))
    end subroutine

    subroutine test_is_graphical_long
        integer :: i
        character(len=1) :: c
        write(*,*) "test_is_graphical_long"
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
        write(*,*) "test_is_printable_short"
        call assert(is_printable(' ')) ! whitespace is printable
        call assert(is_printable('1'))
        call assert(is_printable('a'))
        call assert(is_printable('#'))
        call assert(.not. is_printable(ascii_control_char%NUL)) ! control characters are not printable

        ! N.B.: Printable non-ASCII Unicode characters are not recognized.
        call assert(.not. is_printable('ä'))
    end subroutine

    subroutine test_is_printable_long
        integer :: i
        character(len=1) :: c
        write(*,*) "test_is_printable_long"
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
        write(*,*) "test_is_ascii_short"
        call assert(is_ascii('a'))
        call assert(.not. is_ascii('ä'))
    end subroutine

    subroutine test_is_ascii_long()
        integer :: i
        write(*,*) "test_is_ascii_long"
        do i = 0, 127
            call assert(is_ascii(achar(i)))
        end do
        call assert(.not. is_ascii(achar(128))) ! raises compiler warning

    end subroutine

    subroutine test_to_lower_short()
        write(*,*) "test_to_lower_short"
        call assert(to_lower('a') == 'a')
        call assert(to_lower('A') == 'a')
        call assert(to_lower('#') == '#')
    end subroutine

    subroutine test_to_lower_long()
        integer :: i
        character(len=1) :: c
        write(*,*) "test_to_lower_long"
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
        write(*,*) "test_to_upper_short"
        call assert(to_upper('a') == 'A')
        call assert(to_upper('A') == 'A')
        call assert(to_upper('#') == '#')
    end subroutine

    subroutine test_to_upper_long()
        integer :: i
        character(len=1) :: c
        write(*,*) "test_to_upper_long"
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


!>  This test reproduces the true/false table found at
!   https://en.cppreference.com/w/cpp/string/byte
!   by passing allocatable character arrays filled with subsets 
!   of ascii characters to the stdlib character validation functions.
!
    subroutine test_ascii_table
        integer :: i, j
        logical :: table(15,12)
        character(len=7) :: col
        
        character(len=1), allocatable :: ca(:)
        integer :: ic(16) ! 15 + 1

        write(*,*) "test_ascii_table"

        !     0-8   control codes
        !       9   tab
        !   10-13   whitespaces
        !   14-31   control codes
        !      32   space
        !   33-47   !"#$%&'()*+,-./
        !   48-57   0123456789
        !   58-64   :;<=>?@
        !   65-70   ABCDEF
        !   71-90   GHIJKLMNOPQRSTUVWXYZ
        !   91-96   [\]^_`
        !  97-102   abcdef
        ! 103-122   ghijklmnopqrstuvwxyz
        ! 123-126   {|}~
        !     127   backspace character

        ic = [0,9,10,14,32,33,48,58,65,71,91,97,103,123,127,128]

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,1) = all(is_control(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,2) = all(is_printable(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,3) = all(is_white(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,4) = all(is_blank(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,5) = all(is_graphical(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,6) = all(is_punctuation(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,7) = all(is_alphanum(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,8) = all(is_alpha(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,9) = all(is_upper(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,10) = all(is_lower(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,11) = all(is_digit(ca))
        end do

        do i = 1, 15
            ca = [(achar(j),j=ic(i),ic(i+1)-1)]
            table(i,12) = all(is_hex_digit(ca))
        end do


        ! Output true/false table for verification
        write(*,*)
        write(*,'(10X,A)')              "is_control"
        write(*,'(10X,A)')              "| is_printable"
        write(*,'(10X,A)')              "| | is_whitespace"
        write(*,'(10X,A)')              "| | | is_blank"
        write(*,'(10X,A)')              "| | | | is_graphical"
        write(*,'(10X,A)')              "| | | | | is_punctuation"
        write(*,'(10X,A)')              "| | | | | | is_alphanum"
        write(*,'(10X,A)')              "| | | | | | | is_alpha"
        write(*,'(10X,A)')              "| | | | | | | | is_upper"
        write(*,'(10X,A)')              "| | | | | | | | | is_lower"
        write(*,'(10X,A)')              "| | | | | | | | | | is_digit"
        write(*,'(A10,A)') " decimal  ","| | | | | | | | | | | is_hex_digit"
        write(*,*)         "-------------------------------------------"
        do i = 1, 15
            ! Process first column
            if (ic(i) /= ic(i+1)-1) then
                write(col,'(I0,"-",I0)') ic(i), ic(i+1)-1 
            else
                write(col,'(I0)') ic(i)
            end if

            write(*,'(1X,A7,2X,12(L1,:,X),2X,I3)') adjustr(col), (table(i,j),j=1,12)
        end do
    end subroutine

end program