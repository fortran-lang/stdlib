module test_ascii
    use stdlib_test, only : new_unittest, unittest_type, error_type, check
    use stdlib_ascii, only: lowercase, uppercase, digits, &
        octal_digits, fullhex_digits, hex_digits, lowerhex_digits, &
        whitespace, letters, is_alphanum, is_alpha, is_lower, is_upper, &
        is_digit, is_octal_digit, is_hex_digit, is_white, is_blank, &
        is_control, is_punctuation, is_graphical, is_printable, is_ascii, &
        to_lower, to_upper, to_title, to_sentence, reverse, LF, TAB, NUL, DEL, &
        to_string
    use stdlib_kinds, only : int8, int16, int32, int64, lk, c_bool
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
            new_unittest("to_upper_string", test_to_upper_string), &
            new_unittest("to_lower_string", test_to_lower_string), &
            new_unittest("to_title_string", test_to_title_string), &
            new_unittest("to_sentence_string", test_to_sentence_string), &
            new_unittest("reverse_string", test_reverse_string), &
            new_unittest("to_string", test_to_string) &
            ]
    end subroutine collect_ascii

    subroutine test_is_alphanum_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_alphanum('A'))
        call check(error, is_alphanum('1'))
        call check(error, .not. is_alphanum('#'))
        ! N.B.: does not return true for non-ASCII Unicode alphanumerics
        call check(error, .not. is_alphanum('á'))
    end subroutine

    subroutine test_is_alphanum_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        clist = digits//octal_digits//fullhex_digits//letters//lowercase//uppercase
        do i = 1, len(clist)
            call check(error, is_alphanum(clist(i:i)))
        end do

        clist = whitespace
        do i = 1, len(clist)
            call check(error, .not. is_alphanum(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_alpha_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_alpha('A'))
        call check(error, .not. is_alpha('1'))
        call check(error, .not. is_alpha('#'))

        ! N.B.: does not return true for non-ASCII Unicode alphabetic characters
        call check(error, .not. is_alpha('á'))
    end subroutine

    subroutine test_is_alpha_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        clist = letters//lowercase//uppercase
        do i = 1, len(clist)
            call check(error, is_alpha(clist(i:i)))
        end do

        clist = digits//octal_digits//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_alpha(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_lower_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_lower('a'))
        call check(error, .not. is_lower('A'))
        call check(error, .not. is_lower('#'))

        ! N.B.: does not return true for non-ASCII Unicode lowercase letters
        call check(error, .not. is_lower('á'))
        call check(error, .not. is_lower('Á'))
    end subroutine

    subroutine test_is_lower_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(lowercase)
            call check(error, is_lower(lowercase(i:i)))
        end do

        clist = digits//uppercase//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_lower(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_upper_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_upper('A'))
        call check(error, .not. is_upper('a'))
        call check(error, .not. is_upper('#'))

        ! N.B.: does not return true for non-ASCII Unicode uppercase letters
        call check(error, .not. is_upper('á'))
        call check(error, .not. is_upper('Á'))
    end subroutine

    subroutine test_is_upper_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(uppercase)
            call check(error, is_upper(uppercase(i:i)))
        end do

        clist = digits//lowercase//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_upper(clist(i:i)))
        end do
    end subroutine


    subroutine test_is_digit_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_digit('3'))
        call check(error, is_digit('8'))
        call check(error, .not. is_digit('B'))
        call check(error, .not. is_digit('#'))

        ! N.B.: does not return true for non-ASCII Unicode numbers
        call check(error, .not. is_digit('０')) ! full-width digit zero (U+FF10)
        call check(error, .not. is_digit('４')) ! full-width digit four (U+FF14))
    end subroutine

    subroutine test_is_digit_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(digits)
            call check(error, is_digit(digits(i:i)))
        end do

        clist = letters//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_octal_digit_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_octal_digit('0'))
        call check(error, is_octal_digit('7'))
        call check(error, .not. is_octal_digit('8'))
        call check(error, .not. is_octal_digit('A'))
        call check(error, .not. is_octal_digit('#'))
    end subroutine

    subroutine test_is_octal_digit_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(octal_digits)
            call check(error, is_octal_digit(octal_digits(i:i)))
        end do
        clist = letters//'89'//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_octal_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_hex_digit_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_hex_digit('0'))
        call check(error, is_hex_digit('A'))
        call check(error, is_hex_digit('f')) !! lowercase hex digits are accepted
        call check(error, .not. is_hex_digit('g'))
        call check(error, .not. is_hex_digit('G'))
        call check(error, .not. is_hex_digit('#'))
    end subroutine

    subroutine test_is_hex_digit_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(fullhex_digits)
            call check(error, is_hex_digit(fullhex_digits(i:i)))
        end do
        clist = lowercase(7:)//uppercase(7:)//whitespace
        do i = 1, len(clist)
            call check(error, .not. is_hex_digit(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_white_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_white(' '))
        call check(error, is_white(TAB))
        call check(error, is_white(LF))
        call check(error, .not. is_white('1'))
        call check(error, .not. is_white('a'))
        call check(error, .not. is_white('#'))
    end subroutine

    subroutine test_is_white_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=:), allocatable :: clist

        do i = 1, len(whitespace)
            call check(error, is_white(whitespace(i:i)))
        end do
        clist = digits//letters
        do i = 1, len(clist)
            call check(error, .not. is_white(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_blank_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_blank(' '))
        call check(error, is_blank(TAB))
        call check(error, .not. is_blank('1'))
        call check(error, .not. is_blank('a'))
        call check(error, .not. is_blank('#'))
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
        end do
        clist = digits//letters
        do i = 1, len(clist)
            call check(error, .not. is_blank(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_control_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        ! print *, is_control('\0')
        ! print *, is_control('\022')
        call check(error, is_control(new_line('a'))) ! newline is both whitespace and control
        call check(error, .not. is_control(' '))
        call check(error, .not. is_control('1'))
        call check(error, .not. is_control('a'))
        call check(error, .not. is_control('#'))

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
        end do
        call check(error, is_control(DEL))

        clist = digits//letters//' '
        do i = 1, len(clist)
            call check(error, .not. is_control(clist(i:i)))
        end do
    end subroutine

    subroutine test_is_punctuation_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_punctuation('.'))
        call check(error, is_punctuation(','))
        call check(error, is_punctuation(':'))
        call check(error, is_punctuation('!'))
        call check(error, is_punctuation('#'))
        call check(error, is_punctuation('~'))
        call check(error, is_punctuation('+'))
        call check(error, is_punctuation('_'))

        call check(error, .not. is_punctuation('1'))
        call check(error, .not. is_punctuation('a'))
        call check(error, .not. is_punctuation(' '))
        call check(error, .not. is_punctuation(LF)) ! new line character
        call check(error, .not. is_punctuation(NUL))

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
        end do
    end subroutine

    subroutine test_is_graphical_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_graphical('1'))
        call check(error, is_graphical('a'))
        call check(error, is_graphical('#'))
        call check(error, .not. is_graphical(' ')) ! whitespace is not graphical
        call check(error, .not. is_graphical(LF))
        call check(error, .not. is_graphical(NUL))

        ! N.B.: Unicode graphical characters are not regarded as such.
        call check(error, .not. is_graphical('ä'))
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
        end do
    end subroutine

    subroutine test_is_printable_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_printable(' ')) ! whitespace is printable
        call check(error, is_printable('1'))
        call check(error, is_printable('a'))
        call check(error, is_printable('#'))
        call check(error, .not. is_printable(NUL)) ! control characters are not printable

        ! N.B.: Printable non-ASCII Unicode characters are not recognized.
        call check(error, .not. is_printable('ä'))
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
        end do
    end subroutine

    subroutine test_is_ascii_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, is_ascii('a'))
        call check(error, .not. is_ascii('ä'))
    end subroutine

    subroutine test_is_ascii_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i

        do i = 0, 127
            call check(error, is_ascii(achar(i)))
        end do
        call check(error, .not. is_ascii(achar(128))) ! raises compiler warning

    end subroutine

    subroutine test_to_lower_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_lower('a'), 'a')
        call check(error, to_lower('A'), 'a')
        call check(error, to_lower('#'), '#')
    end subroutine

    subroutine test_to_lower_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=1) :: c

        do i = 1, len(uppercase)
            call check(error, to_lower(uppercase(i:i)), lowercase(i:i))
        end do
        do i = 0, 127
            c = achar(i)
            if (c < 'A' .or. c > 'Z') then
                call check(error, to_lower(c), c)
            else
                call check(error, to_lower(c) /= c)
            end if
        end do
    end subroutine

    subroutine test_to_upper_short(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        call check(error, to_upper('a'), 'A')
        call check(error, to_upper('A'), 'A')
        call check(error, to_upper('#'), '#')
    end subroutine

    subroutine test_to_upper_long(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        integer :: i
        character(len=1) :: c

        do i = 1, len(lowercase)
            call check(error, to_upper(lowercase(i:i)), uppercase(i:i))
        end do

        do i = 0, 127
            c = achar(i)
            if (c < 'a' .or. c > 'z') then
                call check(error, to_upper(c), c)
            else
                call check(error, to_upper(c) /= c)
            end if
        end do
    end subroutine

    !
    !   This test reproduces the true/false table found at
    !   https://en.cppreference.com/w/cpp/string/byte
    !
    subroutine test_ascii_table
        integer :: i, j
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
    end subroutine test_ascii_table

    subroutine test_to_lower_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "UPPERCASE"

        dlc = to_lower("UPPERCASE")
        call check(error, dlc, "uppercase")

        dlc = to_lower(input)
        call check(error, len(dlc), 32)
        call check(error, len_trim(dlc), 9)
        call check(error, trim(dlc), "uppercase")

        dlc = to_lower("0123456789ABCDE")
        call check(error, dlc, "0123456789abcde")
    end subroutine test_to_lower_string

    subroutine test_to_upper_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "lowercase"

        dlc = to_upper("lowercase")
        call check(error, dlc, "LOWERCASE")

        dlc = to_upper(input)
        call check(error, len(dlc), 32)
        call check(error, len_trim(dlc), 9)
        call check(error, trim(dlc), "LOWERCASE")

        dlc = to_upper("0123456789abcde")
        call check(error, dlc, "0123456789ABCDE")
    end subroutine test_to_upper_string

    subroutine test_to_title_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "tHis Is tO bE tiTlEd"

        dlc = to_title("tHis Is tO bE tiTlEd")
        call check(error, dlc, "This Is To Be Titled")

        dlc = to_title(input)
        call check(error, len(dlc), 32)
        call check(error, len_trim(dlc), 20)
        call check(error, trim(dlc), "This Is To Be Titled")

        dlc = to_title(" s P a C e D !")
        call check(error, dlc, " S P A C E D !")

        dlc = to_title("1st, 2nD, 3RD")
        call check(error, dlc, "1st, 2nd, 3rd")

        dlc = to_title("""quOTed""")
        call check(error, dlc, """Quoted""")
    end subroutine test_to_title_string

    subroutine test_to_sentence_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "tHis iS A seNteNcE."

        dlc = to_sentence("tHis iS A seNteNcE.")
        call check(error, dlc, "This is a sentence.")

        dlc = to_sentence(input)
        call check(error, len(dlc), 32)
        call check(error, len_trim(dlc), 19)
        call check(error, trim(dlc), "This is a sentence.")

        dlc = to_sentence(" s P a C e D !")
        call check(error, dlc, " S p a c e d !")

        dlc = to_sentence("1st, 2nd, 3rd")
        call check(error, dlc, "1st, 2nd, 3rd")

        dlc = to_sentence("""quOTed""")
        call check(error, dlc, """Quoted""")
    end subroutine test_to_sentence_string

    subroutine test_reverse_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=:), allocatable :: dlc
        character(len=32), parameter :: input = "reversed"

        dlc = reverse("reversed")
        call check(error, dlc, "desrever")

        dlc = reverse(input)
        call check(error, len(dlc), 32)
        call check(error, len_trim(dlc), 32)
        call check(error, trim(dlc), "                        desrever")
        call check(error, trim(adjustl(dlc)), "desrever")
    end subroutine test_reverse_string

    subroutine test_to_string(error)
        !> Error handling
        type(error_type), allocatable, intent(out) :: error

        character(len=128) :: flc

        write(flc, '(g0)') 1026192
        call check(error, to_string(1026192), trim(flc))

        write(flc, '(g0)') -124784
        call check(error, to_string(-124784), trim(flc))

        write(flc, '(g0)') 1_int8
        call check(error, to_string(1_int8), trim(flc))

        write(flc, '(g0)') -3_int8
        call check(error, to_string(-3_int8), trim(flc))

        write(flc, '(g0)') 80_int16
        call check(error, to_string(80_int16), trim(flc))

        write(flc, '(g0)') 8924890_int32
        call check(error, to_string(8924890_int32), trim(flc))

        write(flc, '(g0)') -2378401_int32
        call check(error, to_string(-2378401_int32), trim(flc))

        write(flc, '(g0)') -921092378401_int64
        call check(error, to_string(-921092378401_int64), trim(flc))

        write(flc, '(g0)') 1272835771_int64
        call check(error, to_string(1272835771_int64), trim(flc))

        write(flc, '(g0)') .true.
        call check(error, to_string(.true.), trim(flc))

        write(flc, '(g0)') .false.
        call check(error, to_string(.false.), trim(flc))

        write(flc, '(g0)') .true._c_bool
        call check(error, to_string(.true._c_bool), trim(flc))

        write(flc, '(g0)') .false._lk
        call check(error, to_string(.false._lk), trim(flc))
    end subroutine test_to_string

end module test_ascii


program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use stdlib_test, only : run_testsuite, new_testsuite, testsuite_type
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
