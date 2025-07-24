
!> The `stdlib_ascii` module provides procedures for handling and manipulating
!> intrinsic character variables and constants.
!>
!> The specification of this module is available [here](../page/specs/stdlib_ascii.html).
module stdlib_ascii
    use stdlib_kinds, only : int8, int16, int32, int64, lk, c_bool

    implicit none
    private

    ! Character validation functions
    public :: is_alpha, is_alphanum
    public :: is_digit, is_hex_digit, is_octal_digit
    public :: is_control, is_white, is_blank
    public :: is_ascii, is_punctuation
    public :: is_graphical, is_printable
    public :: is_lower, is_upper

    ! Character conversion functions
    public :: to_lower, to_upper, to_title, to_sentence, reverse

    ! All control characters in the ASCII table (see www.asciitable.com).
    character(len=1), public, parameter :: NUL = achar(int(z'00')) !! Null
    character(len=1), public, parameter :: SOH = achar(int(z'01')) !! Start of heading
    character(len=1), public, parameter :: STX = achar(int(z'02')) !! Start of text
    character(len=1), public, parameter :: ETX = achar(int(z'03')) !! End of text
    character(len=1), public, parameter :: EOT = achar(int(z'04')) !! End of transmission
    character(len=1), public, parameter :: ENQ = achar(int(z'05')) !! Enquiry
    character(len=1), public, parameter :: ACK = achar(int(z'06')) !! Acknowledge
    character(len=1), public, parameter :: BEL = achar(int(z'07')) !! Bell
    character(len=1), public, parameter :: BS  = achar(int(z'08')) !! Backspace
    character(len=1), public, parameter :: TAB = achar(int(z'09')) !! Horizontal tab
    character(len=1), public, parameter :: LF  = achar(int(z'0A')) !! NL line feed, new line
    character(len=1), public, parameter :: VT  = achar(int(z'0B')) !! Vertical tab
    character(len=1), public, parameter :: FF  = achar(int(z'0C')) !! NP form feed, new page
    character(len=1), public, parameter :: CR  = achar(int(z'0D')) !! Carriage return
    character(len=1), public, parameter :: SO  = achar(int(z'0E')) !! Shift out
    character(len=1), public, parameter :: SI  = achar(int(z'0F')) !! Shift in
    character(len=1), public, parameter :: DLE = achar(int(z'10')) !! Data link escape
    character(len=1), public, parameter :: DC1 = achar(int(z'11')) !! Device control 1
    character(len=1), public, parameter :: DC2 = achar(int(z'12')) !! Device control 2
    character(len=1), public, parameter :: DC3 = achar(int(z'13')) !! Device control 3
    character(len=1), public, parameter :: DC4 = achar(int(z'14')) !! Device control 4
    character(len=1), public, parameter :: NAK = achar(int(z'15')) !! Negative acknowledge
    character(len=1), public, parameter :: SYN = achar(int(z'16')) !! Synchronous idle
    character(len=1), public, parameter :: ETB = achar(int(z'17')) !! End of transmission block
    character(len=1), public, parameter :: CAN = achar(int(z'18')) !! Cancel
    character(len=1), public, parameter :: EM  = achar(int(z'19')) !! End of medium
    character(len=1), public, parameter :: SUB = achar(int(z'1A')) !! Substitute
    character(len=1), public, parameter :: ESC = achar(int(z'1B')) !! Escape
    character(len=1), public, parameter :: FS  = achar(int(z'1C')) !! File separator
    character(len=1), public, parameter :: GS  = achar(int(z'1D')) !! Group separator
    character(len=1), public, parameter :: RS  = achar(int(z'1E')) !! Record separator
    character(len=1), public, parameter :: US  = achar(int(z'1F')) !! Unit separator
    character(len=1), public, parameter :: DEL = achar(int(z'7F')) !! Delete

    ! Constant character sequences
    character(len=*), public, parameter :: fullhex_digits = "0123456789ABCDEFabcdef" !! 0 .. 9A .. Fa .. f
    character(len=*), public, parameter :: hex_digits = fullhex_digits(1:16) !! 0 .. 9A .. F
    character(len=*), public, parameter :: lowerhex_digits = "0123456789abcdef" !! 0 .. 9a .. f
    character(len=*), public, parameter :: digits = hex_digits(1:10) !! 0 .. 9
    character(len=*), public, parameter :: octal_digits = digits(1:8) !! 0 .. 7
    character(len=*), public, parameter :: letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" !! A .. Za .. z
    character(len=*), public, parameter :: uppercase = letters(1:26) !! A .. Z
    character(len=*), public, parameter :: lowercase = letters(27:) !! a .. z
    character(len=*), public, parameter :: whitespace = " "//TAB//VT//CR//LF//FF !! ASCII _whitespace


    !> Returns a new character sequence which is the lower case 
    !> version of the input character sequence
    !> This method is elemental and returns a character sequence
    interface to_lower
        module procedure :: to_lower
    end interface to_lower

    !> Returns a new character sequence which is the upper case
    !> version of the input character sequence
    !> This method is elemental and returns a character sequence
    interface to_upper
        module procedure :: to_upper
    end interface to_upper

    !> Returns a new character sequence which is the title case
    !> version of the input character sequence
    !> This method is elemental and returns a character sequence
    interface to_title
        module procedure :: to_title
    end interface to_title

    !> Returns a new character sequence which is the sentence case
    !> version of the input character sequence
    !> This method is elemental and returns a character sequence
    interface to_sentence
        module procedure :: to_sentence
    end interface to_sentence

    !> Returns a new character sequence which is reverse of
    !> the input charater sequence
    !> This method is elemental and returns a character sequence
    interface reverse
        module procedure :: reverse
    end interface reverse
    

contains

    !> Checks whether `c` is an ASCII letter (A .. Z, a .. z).
    elemental logical function is_alpha(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function

    !> Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
    elemental logical function is_alphanum(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alphanum = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') &
            .or. (c >= 'A' .and. c <= 'Z')
    end function

    !> Checks whether or not `c` is in the ASCII character set -
    !> i.e. in the range 0 .. 0x7F.
    elemental logical function is_ascii(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_ascii = iachar(c) <= int(z'7F')
    end function

    !> Checks whether `c` is a control character.
    elemental logical function is_control(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_control = ic < int(z'20') .or. ic == int(z'7F')
    end function

    !> Checks whether `c` is a digit (0 .. 9).
    elemental logical function is_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_digit = ('0' <= c) .and. (c <= '9')
    end function

    !> Checks whether `c` is a digit in base 8 (0 .. 7).
    elemental logical function is_octal_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_octal_digit = (c >= '0') .and. (c <= '7');
    end function

    !> Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
    elemental logical function is_hex_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_hex_digit = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'f') &
            .or. (c >= 'A' .and. c <= 'F')
    end function

    !> Checks whether or not `c` is a punctuation character. That includes
    !> all ASCII characters which are not control characters, letters,
    !> digits, or whitespace.
    elemental logical function is_punctuation(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c) !       '~'                 '!'
        is_punctuation = (ic <= int(z'7E')) .and. (ic >= int(z'21')) .and. &
            (.not. is_alphanum(c))
    end function

    !> Checks whether or not `c` is a printable character other than the
    !> space character.
    elemental logical function is_graphical(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        !The character is graphical if it's between '!' and '~' in the ASCII table,
        !that is: printable but not a space
        is_graphical = (int(z'21') <= ic) .and. (ic <= int(z'7E'))
    end function

    !> Checks whether or not `c` is a printable character - including the
    !> space character.
    elemental logical function is_printable(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        !The character is printable if it's between ' ' and '~' in the ASCII table
        is_printable = ic >= iachar(' ') .and. ic <= int(z'7E')
    end function

    !> Checks whether `c` is a lowercase ASCII letter (a .. z).
    elemental logical function is_lower(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_lower = ic >= iachar('a') .and. ic <= iachar('z')
    end function

    !> Checks whether `c` is an uppercase ASCII letter (A .. Z).
    elemental logical function is_upper(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_upper = (c >= 'A') .and. (c <= 'Z')
    end function

    !> Checks whether or not `c` is a whitespace character. That includes the
    !> space, tab, vertical tab, form feed, carriage return, and linefeed
    !> characters.
    elemental logical function is_white(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB, LF, VT, FF, CR
        is_white = (c == ' ') .or. (ic >= int(z'09') .and. ic <= int(z'0D'));
    end function

    !> Checks whether or not `c` is a blank character. That includes the
    !> only the space and tab characters
    elemental logical function is_blank(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB
        is_blank = (c == ' ') .or. (ic == int(z'09'));
    end function

    !> Returns the corresponding lowercase letter, if `c` is an uppercase
    !> ASCII character, otherwise `c` itself.
    elemental function char_to_lower(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1)             :: t
        integer, parameter :: wp= iachar('a')-iachar('A'), BA=iachar('A'), BZ=iachar('Z')
        integer :: k
        !Check whether the integer equivalent is between BA=65 and BZ=90
        k = ichar(c) 
        if (k>=BA.and.k<=BZ) k = k + wp 
        t = char(k)

    end function char_to_lower

    !> Returns the corresponding uppercase letter, if `c` is a lowercase
    !> ASCII character, otherwise `c` itself.
    elemental function char_to_upper(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1)             :: t
        integer, parameter :: wp= iachar('a')-iachar('A'), la=iachar('a'), lz=iachar('z')
        integer :: k
        !Check whether the integer equivalent is between la=97 and lz=122
        k = ichar(c) 
        if (k>=la.and.k<=lz) k = k - wp 
        t = char(k)

    end function char_to_upper

    !> Convert character variable to lower case
    !> ([Specification](../page/specs/stdlib_ascii.html#to_lower))
    !>
    !> Version: experimental
    elemental function to_lower(string) result(lower_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: lower_string
        integer :: i

        do i = 1, len(string)
            lower_string(i:i) = char_to_lower(string(i:i))
        end do

    end function to_lower

    !> Convert character variable to upper case
    !> ([Specification](../page/specs/stdlib_ascii.html#to_upper))
    !>
    !> Version: experimental
    elemental function to_upper(string) result(upper_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: upper_string
        integer :: i

        do i = 1, len(string)
            upper_string(i:i) = char_to_upper(string(i:i))
        end do

    end function to_upper

    !> Converts character sequence to title case
    !> ([Specification](../page/specs/stdlib_ascii.html#to_title))
    !>
    !> Version: experimental
    elemental function to_title(string) result(title_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: title_string
        integer :: i
        logical :: capitalize_switch

        capitalize_switch = .true.
        do i = 1, len(string)
            if (is_alphanum(string(i:i))) then
                if (capitalize_switch) then
                    title_string(i:i) = char_to_upper(string(i:i))
                    capitalize_switch = .false.
                else
                    title_string(i:i) = char_to_lower(string(i:i))
                end if
            else
                title_string(i:i) = string(i:i)
                capitalize_switch = .true.
            end if
        end do

    end function to_title

    !> Converts character sequence to sentence case
    !> ([Specification](../page/specs/stdlib_ascii.html#to_sentence))
    !>
    !> Version: experimental
    elemental function to_sentence(string) result(sentence_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: sentence_string
        integer :: i, n

        n = len(string)
        do i = 1, len(string)
            if (is_alphanum(string(i:i))) then
                sentence_string(i:i) = char_to_upper(string(i:i))
                n = i
                exit
            else
                sentence_string(i:i) = string(i:i)
            end if
        end do

        do i = n + 1, len(string)
            sentence_string(i:i) = char_to_lower(string(i:i))
        end do

    end function to_sentence

    !> Reverse the character order in the input character variable
    !> ([Specification](../page/specs/stdlib_ascii.html#reverse))
    !>
    !> Version: experimental
    elemental function reverse(string) result(reverse_string)
        character(len=*), intent(in) :: string
        character(len=len(string)) :: reverse_string
        integer :: i, n

        n = len(string)
        do i = 1, n
            reverse_string(n-i+1:n-i+1) = string(i:i)
        end do

    end function reverse

end module stdlib_ascii
