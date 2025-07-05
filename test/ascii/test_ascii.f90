module test_ascii
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_ascii, only: lowercase, uppercase, digits, &
        octal_digits, fullhex_digits, hex_digits, lowerhex_digits, &
        whitespace, letters, is_alphanum, is_alpha, is_lower, is_upper, &
        is_digit, is_octal_digit, is_hex_digit, is_white, is_blank, &
        is_control, is_punctuation, is_graphical, is_printable, is_ascii, &
        to_lower, to_upper, to_title, to_sentence, reverse, LF, TAB, NUL, DEL
    use stdlib_kinds, only : int8, int16, int32, int64, lk
    implicit none
    private

    public :: collect_ascii

contains

    !> Collect all exported unit tests
    subroutine collect_ascii(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("is_alphanum_short", test_is_alphanum_short), &
            new_unittest("is_alphanum_long", test_is_alphanum_long), &
            new_unittest("is_alpha_short", test_is_alpha_short), &
            new_unittest("is_alpha_long", test_is_alpha_long), &
            new_unittest("is_lower_short", test_is_lower_short), &
            new_unittest("is_lower_long", test_is_lower_long), &
            new_unittest("is_upper_short", test_is_upper_short), &
            new_unittest("is_upper_long", test_is_upper_long), &
            new_unittest("is_digit_short", test_is_digit_short), &
            new_unittest("is_digit_long", test_is_digit_long), &
            new_unittest("is_octal_digit_short", test_is_octal_digit_short), &
            new_unittest("is_octal_digit_long", test_is_octal_digit_long), &
            new_unittest("is_hex_digit_short", test_is_hex_digit_short), &
            new_unittest("is_hex_digit_long", test_is_hex_digit_long), &
            new_unittest("is_white_short", test_is_white_short), &
            new_unittest("is_white_long", test_is_white_long), &
            new_unittest("is_blank_short", test_is_blank_short), &
            new_unittest("is_blank_long", test_is_blank_long), &
            new_unittest("is_control_short", test_is_control_short), &
            new_unittest("is_control_long", test_is_control_long), &
            new_unittest("is_punctuation_short", test_is_punctuation_short), &
            new_unittest("is_punctuation_long", test_is_punctuation_long), &
            new_unittest("is_graphical_short", test_is_graphical_short), &
            new_unittest("is_graphical_long", test_is_graphical_long), &
            new_unittest("is_printable_short", test_is_printable_short), &
            new_unittest("is_printable_long", test_is_printable_long), &
            new_unittest("is_ascii_short", test_is_ascii_short), &
            new_unittest("is_ascii_long", test_is_ascii_long), &
            new_unittest("to_lower_short", test_to_lower_short), &
            new_unittest("to_lower_long", test_to_lower_long), &
            new_unittest("to_upper_short", test_to_upper_short), &
            new_unittest("to_upper_long", test_to_upper_long), &
            new_unittest("ascii_table", test_ascii_table), &
            new_unittest("to_upper_string", test_to_upper_string), &
            new_unittest("to_lower_string", test_to_lower_string), &
            new_unittest("to_title_string", test_to_title_string), &
            new_unittest("to_sentence_string", test_to_sentence_string), &
            new_unittest("reverse_string", test_reverse_string) &
            ]
    end subroutine collect_ascii

    subroutine test_is_alphanum_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_alphanum('A'))
        if (allocated(error)) return

        call check(error, is_alphanum('1'))
        if (allocated(error)) return

        call check(error, .not. is_alphanum('#'))
        if (allocated(error)) return

        ! N.B.: does not return true for non-ASCII Unicode alphanumerics
        call check(error, .not. is_alphanum('á'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_alphanum_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        clist = digits//octal_digits//fullhex_digits//letters//lowercase//uppercase
        do i = 1, len(clist)
            call check(error, is_alphanum(clist(i:i)))
            if (allocated(error)) return
        end do

        clist = whitespace
        do i = 1, len(clist)
            call check(error, .not. is_alphanum(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_alpha_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_alpha('A'))
        if (allocated(error)) return

        call check(error, .not. is_alpha('1'))
        if (allocated(error)) return

        call check(error, .not. is_alpha('#'))
        if (allocated(error)) return

        ! N.B.: does not return true for non-ASCII Unicode alphabetic characters
        call check(error, .not. is_alpha('á'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_alpha_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        clist = letters//lowercase//uppercase
        do i = 1, len(clist)
            call check(error, is_alpha(clist(i:i)))
            if (allocated(error)) return
        end do

        clist = digits//octal_digits//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_alpha(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_lower_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_lower('a'))
        if (allocated(error)) return

        call check(error, .not. is_lower('A'))
        if (allocated(error)) return

        call check(error, .not. is_lower('#'))
        if (allocated(error)) return

        ! N.B.: does not return true for non-ASCII Unicode lowercase letters
        call check(error, .not. is_lower('á'))
        if (allocated(error)) return

        call check(error, .not. is_lower('Á'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_lower_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(lowercase)
            call check(error, is_lower(lowercase(i:i)))
            if (allocated(error)) return
        end do

        clist = digits//uppercase//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_lower(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_upper_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_upper('A'))
        if (allocated(error)) return

        call check(error, .not. is_upper('a'))
        if (allocated(error)) return

        call check(error, .not. is_upper('#'))
        if (allocated(error)) return

        ! N.B.: does not return true for non-ASCII Unicode uppercase letters
        call check(error, .not. is_upper('á'))
        if (allocated(error)) return

        call check(error, .not. is_upper('Á'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_upper_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(uppercase)
            call check(error, is_upper(uppercase(i:i)))
            if (allocated(error)) return
        end do

        clist = digits//lowercase//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_upper(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine


    subroutine test_is_digit_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_digit('3'))
        if (allocated(error)) return

        call check(error, is_digit('8'))
        if (allocated(error)) return

        call check(error, .not. is_digit('B'))
        if (allocated(error)) return

        call check(error, .not. is_digit('#'))
        if (allocated(error)) return

        ! N.B.: does not return true for non-ASCII Unicode numbers
        call check(error, .not. is_digit('０')) ! full-width digit zero (U+FF10)
        if (allocated(error)) return

        call check(error, .not. is_digit('４')) ! full-width digit four (U+FF14))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_digit_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(digits)
            call check(error, is_digit(digits(i:i)))
            if (allocated(error)) return
        end do

        clist = letters//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_digit(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_octal_digit_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_octal_digit('0'))
        if (allocated(error)) return

        call check(error, is_octal_digit('7'))
        if (allocated(error)) return

        call check(error, .not. is_octal_digit('8'))
        if (allocated(error)) return

        call check(error, .not. is_octal_digit('A'))
        if (allocated(error)) return

        call check(error, .not. is_octal_digit('#'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_octal_digit_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(octal_digits)
            call check(error, is_octal_digit(octal_digits(i:i)))
            if (allocated(error)) return
        end do
        clist = letters//'89'//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_octal_digit(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_hex_digit_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_hex_digit('0'))
        if (allocated(error)) return

        call check(error, is_hex_digit('A'))
        if (allocated(error)) return

        call check(error, is_hex_digit('f')) !! lowercase hex digits are accepted
        if (allocated(error)) return

        call check(error, .not. is_hex_digit('g'))
        if (allocated(error)) return

        call check(error, .not. is_hex_digit('G'))
        if (allocated(error)) return

        call check(error, .not. is_hex_digit('#'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_hex_digit_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(fullhex_digits)
            call check(error, is_hex_digit(fullhex_digits(i:i)))
            if (allocated(error)) return
        end do
        clist = lowercase(7:)//uppercase(7:)//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_hex_digit(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_white_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_white(' '))
        if (allocated(error)) return

        call check(error, is_white(TAB))
        if (allocated(error)) return

        call check(error, is_white(LF))
        if (allocated(error)) return

        call check(error, .not. is_white('1'))
        if (allocated(error)) return

        call check(error, .not. is_white('a'))
        if (allocated(error)) return

        call check(error, .not. is_white('#'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_white_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(whitespace)
            call check(error, is_white(whitespace(i:i)))
            if (allocated(error)) return
        end do
        clist = digits//letters
        do i = 1, len(clist)
            call check(error, .not. is_white(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_blank_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_blank(' '))
        if (allocated(error)) return

        call check(error, is_blank(TAB))
        if (allocated(error)) return

        call check(error, .not. is_blank('1'))
        if (allocated(error)) return

        call check(error, .not. is_blank('a'))
        if (allocated(error)) return

        call check(error, .not. is_blank('#'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_blank_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(whitespace)
            if (whitespace(i:i) == ' ' .or. whitespace(i:i) == TAB) then
                call check(error, is_blank(whitespace(i:i)))
            else
                call check(error, .not. is_blank(whitespace(i:i)))
            end if
            if (allocated(error)) return
        end do
        clist = digits//letters
        do i = 1, len(clist)
            call check(error, .not. is_blank(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_control_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        ! print *, is_control('\0')
        ! print *, is_control('\022')
        call check(error, is_control(new_line('a'))) ! newline is both whitespace and control
        if (allocated(error)) return

        call check(error, .not. is_control(' '))
        if (allocated(error)) return

        call check(error, .not. is_control('1'))
        if (allocated(error)) return

        call check(error, .not. is_control('a'))
        if (allocated(error)) return

        call check(error, .not. is_control('#'))
        if (allocated(error)) return

        ! N.B.: non-ASCII Unicode control characters are not recognized:
        ! print *, .not. is_control('\u0080')
        ! print *, .not. is_control('\u2028')
        ! print *, .not. is_control('\u2029')
    end subroutine

    subroutine test_is_control_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 0, 31
            call check(error, is_control(achar(i)))
            if (allocated(error)) return
        end do
        call check(error, is_control(DEL))
        if (allocated(error)) return

        clist = digits//letters//' '
        do i = 1, len(clist)
            call check(error, .not. is_control(clist(i:i)))
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_punctuation_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_punctuation('.'))
        if (allocated(error)) return

        call check(error, is_punctuation(','))
        if (allocated(error)) return

        call check(error, is_punctuation(':'))
        if (allocated(error)) return

        call check(error, is_punctuation('!'))
        if (allocated(error)) return

        call check(error, is_punctuation('#'))
        if (allocated(error)) return

        call check(error, is_punctuation('~'))
        if (allocated(error)) return

        call check(error, is_punctuation('+'))
        if (allocated(error)) return

        call check(error, is_punctuation('_'))
        if (allocated(error)) return

        call check(error, .not. is_punctuation('1'))
        if (allocated(error)) return

        call check(error, .not. is_punctuation('a'))
        if (allocated(error)) return

        call check(error, .not. is_punctuation(' '))
        if (allocated(error)) return

        call check(error, .not. is_punctuation(LF)) ! new line character
        if (allocated(error)) return

        call check(error, .not. is_punctuation(NUL))
        if (allocated(error)) return

        ! N.B.: Non-ASCII Unicode punctuation characters are not recognized.
        ! print *, is_punctuation('\u2012') ! (U+2012 = en-dash)
    end subroutine

    subroutine test_is_punctuation_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=1) :: c

        do i = 0, 127
            c = achar(i)
            if (is_control(c) .or. is_alphanum(c) .or. c == ' ') then
                call check(error, .not. is_punctuation(c))
            else
                call check(error, is_punctuation(c))
            end if
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_graphical_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_graphical('1'))
        if (allocated(error)) return

        call check(error, is_graphical('a'))
        if (allocated(error)) return

        call check(error, is_graphical('#'))
        if (allocated(error)) return

        call check(error, .not. is_graphical(' ')) ! whitespace is not graphical
        if (allocated(error)) return

        call check(error, .not. is_graphical(LF))
        if (allocated(error)) return

        call check(error, .not. is_graphical(NUL))
        if (allocated(error)) return

        ! N.B.: Unicode graphical characters are not regarded as such.
        call check(error, .not. is_graphical('ä'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_graphical_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=1) :: c

        do i = 0, 127
            c = achar(i)
            if (is_control(c) .or. c == ' ') then
                call check(error, .not. is_graphical(c))
            else
                call check(error, is_graphical(c))
            end if
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_printable_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_printable(' ')) ! whitespace is printable
        if (allocated(error)) return

        call check(error, is_printable('1'))
        if (allocated(error)) return

        call check(error, is_printable('a'))
        if (allocated(error)) return

        call check(error, is_printable('#'))
        if (allocated(error)) return

        call check(error, .not. is_printable(NUL)) ! control characters are not printable
        if (allocated(error)) return

        ! N.B.: Printable non-ASCII Unicode characters are not recognized.
        call check(error, .not. is_printable('ä'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_printable_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=1) :: c

        do i = 0, 127
            c = achar(i)
            if (is_control(c)) then
                call check(error, .not. is_printable(c))
            else
                call check(error, is_printable(c))
            end if
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_is_ascii_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_ascii('a'))
        if (allocated(error)) return

        call check(error, .not. is_ascii('ä'))
        if (allocated(error)) return
    end subroutine

    subroutine test_is_ascii_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i

        do i = 0, 127
            call check(error, is_ascii(achar(i)))
            if (allocated(error)) return
        end do
        call check(error, .not. is_ascii(achar(128))) ! raises compiler warning
        if (allocated(error)) return

    end subroutine

    subroutine test_to_lower_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_lower('a'), 'a')
        if (allocated(error)) return

        call check(error, to_lower('A'), 'a')
        if (allocated(error)) return

        call check(error, to_lower('#'), '#')
        if (allocated(error)) return
    end subroutine

    subroutine test_to_lower_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=1) :: c

        do i = 1, len(uppercase)
            call check(error, to_lower(uppercase(i:i)), lowercase(i:i))
            if (allocated(error)) return
        end do
        do i = 0, 127
            c = achar(i)
            if (c < 'A' .or. c > 'Z') then
                call check(error, to_lower(c), c)
            else
                call check(error, to_lower(c) /= c)
            end if
            if (allocated(error)) return
        end do
    end subroutine

    subroutine test_to_upper_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_upper('a'), 'A')
        if (allocated(error)) return

        call check(error, to_upper('A'), 'A')
        if (allocated(error)) return

        call check(error, to_upper('#'), '#')
        if (allocated(error)) return
    end subroutine

    subroutine test_to_upper_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=1) :: c

        do i = 1, len(lowercase)
            call check(error, to_upper(lowercase(i:i)), uppercase(i:i))
            if (allocated(error)) return
        end do

        do i = 0, 127
            c = achar(i)
            if (c < 'a' .or. c > 'z') then
                call check(error, to_upper(c), c)
            else
                call check(error, to_upper(c) /= c)
            end if
            if (allocated(error)) return
        end do
    end subroutine

    !
    !   This test reproduces the true/false table found at
    !   https://en.cppreference.com/w/cpp/string/byte
    !
    subroutine ascii_table(table)
        logical, intent(out) :: table(15,12)
        integer :: i, j

        ! loop through functions
        do i = 1, 12
            table(1,i)  = all([(validate(j,i), j=0,8)])
            table(2,i)  = validate(9,i)
            table(3,i)  = all([(validate(j,i), j=10,13)])
            table(4,i)  = all([(validate(j,i), j=14,31)])
            table(5,i)  = validate(32,i)
            table(6,i)  = all([(validate(j,i), j=33,47)])
            table(7,i)  = all([(validate(j,i), j=48,57)])
            table(8,i)  = all([(validate(j,i), j=58,64)])
            table(9,i)  = all([(validate(j,i), j=65,70)])
            table(10,i) = all([(validate(j,i), j=71,90)])
            table(11,i) = all([(validate(j,i), j=91,96)])
            table(12,i) = all([(validate(j,i), j=97,102)])
            table(13,i) = all([(validate(j,i), j=103,122)])
            table(14,i) = all([(validate(j,i), j=123,126)])
            table(15,i) = validate(127,i)
        end do

        ! output table for verification
        write(*,'(5X,12(I4))') (i,i=1,12)
        do j = 1, 15
            write(*,'(I3,2X,12(L4),2X,I3)') j, (table(j,i),i=1,12), count(table(j,:))
        end do
        write(*,'(5X,12(I4))') (count(table(:,i)),i=1,12)
 
        contains

            elemental logical function validate(ascii_code, func)
                integer, intent(in) :: ascii_code, func
                character(len=1) :: c
            
                c = achar(ascii_code)
            
                select case (func)
                    case (1);     validate = is_control(c)
                    case (2);     validate = is_printable(c)
                    case (3);     validate = is_white(c)
                    case (4);     validate = is_blank(c)
                    case (5);     validate = is_graphical(c)
                    case (6);     validate = is_punctuation(c)
                    case (7);     validate = is_alphanum(c)
                    case (8);     validate = is_alpha(c)
                    case (9);     validate = is_upper(c)
                    case (10);    validate = is_lower(c)
                    case (11);    validate = is_digit(c)
                    case (12);    validate = is_hex_digit(c)
                    case default; validate = .false.
                end select
            end function validate
    
    end subroutine ascii_table

    subroutine test_ascii_table(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: arr(15, 12)
        logical, parameter :: ascii_class_table(15,12) = transpose(reshape([ &
        ! iscntrl  isprint  isspace  isblank  isgraph  ispunct  isalnum  isalpha  isupper  islower  isdigit  isxdigit
        .true.,   .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., & ! 0–8
        .true.,   .false., .true.,  .true.,  .false., .false., .false., .false., .false., .false., .false., .false., & ! 9
        .true.,   .false., .true.,  .false., .false., .false., .false., .false., .false., .false., .false., .false., & ! 10–13
        .true.,   .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., & ! 14–31
        .false.,  .true.,  .true.,  .true.,  .false., .false., .false., .false., .false., .false., .false., .false., & ! 32 (space)
        .false.,  .true.,  .false., .false., .true.,  .true.,  .false., .false., .false., .false., .false., .false., & ! 33–47
        .false.,  .true.,  .false., .false., .true.,  .false., .true.,  .false., .false., .false., .true.,  .true.,  & ! 48–57
        .false.,  .true.,  .false., .false., .true.,  .true.,  .false., .false., .false., .false., .false., .false., & ! 58–64
        .false.,  .true.,  .false., .false., .true.,  .false., .true.,  .true.,  .true.,  .false., .false., .true.,  & ! 65–70
        .false.,  .true.,  .false., .false., .true.,  .false., .true.,  .true.,  .true.,  .false., .false., .false., & ! 71–90
        .false.,  .true.,  .false., .false., .true.,  .true.,  .false., .false., .false., .false., .false., .false., & ! 91–96
        .false.,  .true.,  .false., .false., .true.,  .false., .true.,  .true.,  .false., .true.,  .false., .true.,  & ! 97–102
        .false.,  .true.,  .false., .false., .true.,  .false., .true.,  .true.,  .false., .true.,  .false., .false., & ! 103–122
        .false.,  .true.,  .false., .false., .true.,  .true.,  .false., .false., .false., .false., .false., .false., & ! 123–126
        .true.,   .false., .false., .false., .false., .false., .false., .false., .false., .false., .false., .false.  & ! 127
        ], shape=[12,15]))

        call ascii_table(arr)
        call check(error, all(arr .eqv. ascii_class_table), "ascii table was not accurately generated")

    end subroutine test_ascii_table

    subroutine test_to_lower_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "UPPERCASE"

        dlc = to_lower("UPPERCASE")
        call check(error, dlc, "uppercase")
        if (allocated(error)) return

        dlc = to_lower(input)
        call check(error, len(dlc), 32)
        if (allocated(error)) return

        call check(error, len_trim(dlc), 9)
        if (allocated(error)) return

        call check(error, trim(dlc), "uppercase")
        if (allocated(error)) return

        dlc = to_lower("0123456789ABCDE")
        call check(error, dlc, "0123456789abcde")
        if (allocated(error)) return
    end subroutine test_to_lower_string

    subroutine test_to_upper_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "lowercase"

        dlc = to_upper("lowercase")
        call check(error, dlc, "LOWERCASE")
        if (allocated(error)) return

        dlc = to_upper(input)
        call check(error, len(dlc), 32)
        if (allocated(error)) return

        call check(error, len_trim(dlc), 9)
        if (allocated(error)) return

        call check(error, trim(dlc), "LOWERCASE")
        if (allocated(error)) return

        dlc = to_upper("0123456789abcde")
        call check(error, dlc, "0123456789ABCDE")
        if (allocated(error)) return
    end subroutine test_to_upper_string

    subroutine test_to_title_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "tHis Is tO bE tiTlEd"

        dlc = to_title("tHis Is tO bE tiTlEd")
        call check(error, dlc, "This Is To Be Titled")
        if (allocated(error)) return

        dlc = to_title(input)
        call check(error, len(dlc), 32)
        if (allocated(error)) return

        call check(error, len_trim(dlc), 20)
        if (allocated(error)) return

        call check(error, trim(dlc), "This Is To Be Titled")
        if (allocated(error)) return

        dlc = to_title(" s P a C e D !")
        call check(error, dlc, " S P A C E D !")
        if (allocated(error)) return

        dlc = to_title("1st, 2nD, 3RD")
        call check(error, dlc, "1st, 2nd, 3rd")
        if (allocated(error)) return

        dlc = to_title("""quOTed""")
        call check(error, dlc, """Quoted""")
        if (allocated(error)) return
    end subroutine test_to_title_string

    subroutine test_to_sentence_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "tHis iS A seNteNcE."

        dlc = to_sentence("tHis iS A seNteNcE.")
        call check(error, dlc, "This is a sentence.")
        if (allocated(error)) return

        dlc = to_sentence(input)
        call check(error, len(dlc), 32)
        if (allocated(error)) return

        call check(error, len_trim(dlc), 19)
        if (allocated(error)) return

        call check(error, trim(dlc), "This is a sentence.")
        if (allocated(error)) return

        dlc = to_sentence(" s P a C e D !")
        call check(error, dlc, " S p a c e d !")
        if (allocated(error)) return

        dlc = to_sentence("1st, 2nd, 3rd")
        call check(error, dlc, "1st, 2nd, 3rd")
        if (allocated(error)) return

        dlc = to_sentence("""quOTed""")
        call check(error, dlc, """Quoted""")
        if (allocated(error)) return
    end subroutine test_to_sentence_string

    subroutine test_reverse_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "reversed"

        dlc = reverse("reversed")
        call check(error, dlc, "desrever")
        if (allocated(error)) return

        dlc = reverse(input)
        call check(error, len(dlc), 32)
        if (allocated(error)) return

        call check(error, len_trim(dlc), 32)
        if (allocated(error)) return

        call check(error, trim(dlc), "                        desrever")
        if (allocated(error)) return

        call check(error, trim(adjustl(dlc)), "desrever")
        if (allocated(error)) return
    end subroutine test_reverse_string


end module test_ascii


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_ascii, only : collect_ascii
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("ascii", collect_ascii) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program
