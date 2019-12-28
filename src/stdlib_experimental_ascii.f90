module stdlib_experimental_ascii

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
public :: to_lower, to_upper

! Ascii control characters
public :: ascii_control_char

! Constant character sequences
public :: fullhex_digits, hex_digits, lowerhex_digits, digits, octal_digits
public :: letters, uppercase, lowercase, whitespace


! All control characters in the ASCII table (see www.asciitable.com).
type :: ascii_control_char_t
    character(len=1) :: NUL = achar(z'00') !! Null
    character(len=1) :: SOH = achar(z'01') !! Start of heading
    character(len=1) :: STX = achar(z'02') !! Start of text
    character(len=1) :: ETX = achar(z'03') !! End of text
    character(len=1) :: EOT = achar(z'04') !! End of transmission
    character(len=1) :: ENQ = achar(z'05') !! Enquiry
    character(len=1) :: ACK = achar(z'06') !! Acknowledge
    character(len=1) :: BEL = achar(z'07') !! Bell
    character(len=1) :: BS  = achar(z'08') !! Backspace
    character(len=1) :: TAB = achar(z'09') !! Horizontal tab
    character(len=1) :: LF  = achar(z'0A') !! NL line feed, new line
    character(len=1) :: VT  = achar(z'0B') !! Vertical tab
    character(len=1) :: FF  = achar(z'0C') !! NP form feed, new page
    character(len=1) :: CR  = achar(z'0D') !! Carriage return
    character(len=1) :: SO  = achar(z'0E') !! Shift out
    character(len=1) :: SI  = achar(z'0F') !! Shift in
    character(len=1) :: DLE = achar(z'10') !! Data link escape
    character(len=1) :: DC1 = achar(z'11') !! Device control 1
    character(len=1) :: DC2 = achar(z'12') !! Device control 2
    character(len=1) :: DC3 = achar(z'13') !! Device control 3
    character(len=1) :: DC4 = achar(z'14') !! Device control 4
    character(len=1) :: NAK = achar(z'15') !! Negative acknowledge
    character(len=1) :: SYN = achar(z'16') !! Synchronous idle
    character(len=1) :: ETB = achar(z'17') !! End of transmission block
    character(len=1) :: CAN = achar(z'18') !! Cancel
    character(len=1) :: EM  = achar(z'19') !! End of medium
    character(len=1) :: SUB = achar(z'1A') !! Substitute
    character(len=1) :: ESC = achar(z'1B') !! Escape
    character(len=1) :: FS  = achar(z'1C') !! File separator
    character(len=1) :: GS  = achar(z'1D') !! Group separator
    character(len=1) :: RS  = achar(z'1E') !! Record separator
    character(len=1) :: US  = achar(z'1F') !! Unit separator
    character(len=1) :: DEL = achar(z'7F') !! Delete
end type

! A single instance of the ascii control characters (initialized to default values)
type(ascii_control_char_t), parameter :: ascii_control_char = ascii_control_char_t()

! Constant character sequences
character(len=*), parameter :: fullhex_digits = "0123456789ABCDEFabcdef" !! 0 .. 9A .. Fa .. f
character(len=*), parameter :: hex_digits = fullhex_digits(1:16) !! 0 .. 9A .. F
character(len=*), parameter :: lowerhex_digits = "0123456789abcdef" !! 0 .. 9a .. f
character(len=*), parameter :: digits = hex_digits(1:10) !! 0 .. 9
character(len=*), parameter :: octal_digits = digits(1:8) !! 0 .. 7
character(len=*), parameter :: letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" !! A .. Za .. z
character(len=*), parameter :: uppercase = letters(1:26) !! A .. Z
character(len=*), parameter :: lowercase = letters(27:) !! a .. z
character(len=*), parameter :: whitespace = " "//ascii_control_char%TAB//&
                                                 ascii_control_char%VT//&
                                                 ascii_control_char%CR//&
                                                 ascii_control_char%LF//&
                                                 ascii_control_char%FF !! ASCII whitespace

contains

!>  Checks whether `c` is an ASCII letter (A .. Z, a .. z).
    elemental logical function is_alpha(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alpha = (c >= 'A' .and. c <= 'Z') .or. (c >= 'a' .and. c <= 'z')
    end function

!>  Checks whether `c` is a letter or a number (0 .. 9, a .. z, A .. Z).
    elemental logical function is_alphanum(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_alphanum = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'z') &
            .or. (c >= 'A' .and. c <= 'Z')
    end function

!>  Checks whether or not `c` is in the ASCII character set -
!   i.e. in the range 0 .. 0x7F.
    elemental logical function is_ascii(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_ascii = iachar(c) <= z'7F'
    end function

!>  Checks whether `c` is a control character.
    elemental logical function is_control(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)
        is_control = ic < z'20' .or. ic == z'7F'
    end function

!>  Checks whether `c` is a digit (0 .. 9).
    elemental logical function is_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_digit = ('0' <= c) .and. (c <= '9')
    end function

!>  Checks whether `c` is a digit in base 8 (0 .. 7).
    elemental logical function is_octal_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_octal_digit = (c >= '0') .and. (c <= '7');
    end function

!>  Checks whether `c` is a digit in base 16 (0 .. 9, A .. F, a .. f).
    elemental logical function is_hex_digit(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_hex_digit = (c >= '0' .and. c <= '9') .or. (c >= 'a' .and. c <= 'f') &
            .or. (c >= 'A' .and. c <= 'F')
    end function

!>  Checks whether or not `c` is a punctuation character. That includes
!   all ASCII characters which are not control characters, letters,
!   digits, or whitespace.
    elemental logical function is_punctuation(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c) !       '~'                 '!'
        is_punctuation = (ic <= z'7E') .and. (ic >= z'21') .and. &
            (.not. is_alphanum(c))
    end function

!>  Checks whether or not `c` is a printable character other than the
!   space character.
    elemental logical function is_graphical(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c) !  '!'                     '~'
        is_graphical = (z'21' <= ic) .and. (ic <= z'7E')
    end function

!>  Checks whether or not `c` is a printable character - including the
!   space character.
    elemental logical function is_printable(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)                    ! '~'
        is_printable = c >= ' ' .and. ic <= z'7E'
    end function

!>  Checks whether `c` is a lowercase ASCII letter (a .. z).
    elemental logical function is_lower(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_lower = (c >= 'a') .and. (c <= 'z')
    end function

!>  Checks whether `c` is an uppercase ASCII letter (A .. Z).
    elemental logical function is_upper(c)
        character(len=1), intent(in) :: c !! The character to test.
        is_upper = (c >= 'A') .and. (c <= 'Z')
    end function

!>  Checks whether or not `c` is a whitespace character. That includes the
!   space, tab, vertical tab, form feed, carriage return, and linefeed
!   characters.
    elemental logical function is_white(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB, LF, VT, FF, CR
        is_white = (c == ' ') .or. (ic >= z'09' .and. ic <= z'0D');
    end function

!>  Checks whether or not `c` is a blank character. That includes 
!   the space and tab characters
    elemental logical function is_blank(c)
        character(len=1), intent(in) :: c !! The character to test.
        integer :: ic
        ic = iachar(c)             ! TAB
        is_blank = (c == ' ') .or. (ic == z'09');
    end function

!>  Returns the corresponding lowercase letter, if `c` is an uppercase
!   ASCII character, otherwise `c` itself.
    elemental function to_lower(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1) :: t
        integer :: diff
        diff = iachar('A')-iachar('a')
        t = c
        ! if uppercase, make lowercase
        if (is_upper(t)) t = achar(iachar(t) - diff)
    end function

!>  Returns the corresponding uppercase letter, if `c` is a lowercase
!   ASCII character, otherwise `c` itself.
    elemental function to_upper(c) result(t)
        character(len=1), intent(in) :: c !! A character.
        character(len=1) :: t
        integer :: diff
        diff = iachar('A')-iachar('a')
        t = c
        ! if lowercase, make uppercase
        if (is_lower(t)) t = achar(iachar(t) + diff)
    end function

end module