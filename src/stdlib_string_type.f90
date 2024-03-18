! SPDX-Identifier: MIT

!> Implementation of a string type to hold an arbitrary sequence of characters.
!>
!> This module provides string type compatible with all Fortran instrinsic character
!> procedures as well as overloaded operators for working with character variables.
!>
!> A string type can be easily constructed by creating a new instance from a
!> character variable or literal by invoking its constructor or by assigning it
!> to a string type. Generally, the string type behaves similar to a deferred
!> length character in most regards but adds memory access safety.
!>
!> The specification of this module is available [here](../page/specs/stdlib_string_type.html).
module stdlib_string_type
    use stdlib_ascii, only: to_lower_ => to_lower, to_upper_ => to_upper, &
       & to_title_ => to_title, to_sentence_ => to_sentence, reverse_ => reverse
    use stdlib_kinds, only : int8, int16, int32, int64, lk, c_bool
    use stdlib_optval, only: optval
    implicit none
    private

    public :: string_type
    public :: len, len_trim, trim, index, scan, verify, repeat, adjustr, adjustl
    public :: lgt, lge, llt, lle, char, ichar, iachar
    public :: to_lower, to_upper, to_title, to_sentence, reverse, move
    public :: assignment(=)
    public :: operator(>), operator(>=), operator(<), operator(<=)
    public :: operator(==), operator(/=), operator(//)
    public :: write(formatted), write(unformatted)
    public :: read(formatted), read(unformatted)


    integer, parameter :: long = selected_int_kind(18)


    !> String type holding an arbitrary sequence of characters.
    type :: string_type
        ! Use the sequence statement below as a hack to prevent extending this type.
        ! It is not used for storage association.
        sequence
        private
        character(len=:), allocatable :: raw
    end type string_type

    !> Returns the length of the character sequence represented by the string.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface len
        module procedure :: len_string
    end interface len

    !> Constructor for new string instances
    interface string_type
        elemental module function new_string(string) result(new)
            character(len=*), intent(in), optional :: string
            type(string_type) :: new
        end function new_string
        elemental module function new_string_from_integer_int8(val) result(new)
            integer(int8), intent(in) :: val
            type(string_type) :: new
        end function new_string_from_integer_int8
        elemental module function new_string_from_integer_int16(val) result(new)
            integer(int16), intent(in) :: val
            type(string_type) :: new
        end function new_string_from_integer_int16
        elemental module function new_string_from_integer_int32(val) result(new)
            integer(int32), intent(in) :: val
            type(string_type) :: new
        end function new_string_from_integer_int32
        elemental module function new_string_from_integer_int64(val) result(new)
            integer(int64), intent(in) :: val
            type(string_type) :: new
        end function new_string_from_integer_int64
        elemental module function new_string_from_logical_lk(val) result(new)
            logical(lk), intent(in) :: val
            type(string_type) :: new
        end function new_string_from_logical_lk
    end interface string_type        

    !> Returns the length of the character sequence without trailing spaces
    !> represented by the string.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface len_trim
        module procedure :: len_trim_string
    end interface len_trim

    !> Returns the character sequence hold by the string without trailing spaces.
    !>
    !> This method is elemental and returns a scalar character value.
    interface trim
        module procedure :: trim_string
    end interface trim

    !> Left-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    !>
    !> This method is elemental and returns a scalar character value.
    interface adjustl
        module procedure :: adjustl_string
    end interface adjustl

    !> Right-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    !>
    !> This method is elemental and returns a scalar character value.
    interface adjustr
        module procedure :: adjustr_string
    end interface adjustr

    !> Repeats the character sequence hold by the string by the number of
    !> specified copies.
    !>
    !> This method is elemental and returns a scalar character value.
    interface repeat
        module procedure :: repeat_string
    end interface repeat

    !> Returns the lowercase version of the character sequence hold by the input string
    !>
    !> This method is Elemental and returns a new string_type instance which holds this
    !> lowercase character sequence
    interface to_lower
        module procedure :: to_lower_string
    end interface to_lower

    !> Returns the uppercase version of the character sequence hold by the input string
    !>
    !> This method is Elemental and returns a new string_type instance which holds this
    !> uppercase character sequence
    interface to_upper
        module procedure :: to_upper_string
    end interface to_upper

    !> Returns the titlecase version of the character sequence hold by the input string
    !>
    !> This method is Elemental and returns a new string_type instance which holds this
    !> titlecase character sequence
    interface to_title
        module procedure :: to_title_string
    end interface to_title

    !> Returns the sentencecase version of the character sequence hold by the input string
    !>
    !> This method is elemental and returns a new string_type instance which holds this
    !> sentencecase character sequence
    interface to_sentence
        module procedure :: to_sentence_string
    end interface to_sentence

    !> Reverses the character sequence hold by the input string
    !> 
    !> This method is elemental and returns a new string_type instance which holds this
    !> reverse character sequence
    interface reverse
        module procedure :: reverse_string
    end interface reverse

    !> Return the character sequence represented by the string.
    !>
    !> This method is elemental and returns a scalar character value.
    interface char
        module procedure :: char_string
        module procedure :: char_string_pos
        module procedure :: char_string_range
    end interface char

    !> Character-to-integer conversion function.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface ichar
        module procedure :: ichar_string
    end interface ichar

    !> Code in ASCII collating sequence.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface iachar
        module procedure :: iachar_string
    end interface iachar

    !> Position of a *substring* within a *string*.
    !>
    !> Returns the position of the start of the leftmost or rightmost occurrence
    !> of string *substring* in *string*, counting from one. If *substring* is not
    !> present in *string*, zero is returned.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface index
        module procedure :: index_string_string
        module procedure :: index_string_char
        module procedure :: index_char_string
    end interface index

    !> Scan a *string* for the presence of a *set* of characters. Scans a *string* for
    !> any of the characters in a *set* of characters.
    !>
    !> If *back* is either absent or *false*, this function returns the position
    !> of the leftmost character of *string* that is in *set*. If *back* is *true*,
    !> the rightmost position is returned. If no character of *set* is found in
    !> *string*, the result is zero.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface scan
        module procedure :: scan_string_string
        module procedure :: scan_string_char
        module procedure :: scan_char_string
    end interface scan

    !> Scan a string for the absence of a set of characters. Verifies that all
    !> the characters in string belong to the set of characters in set.
    !>
    !> If *back* is either absent or *false*, this function returns the position
    !> of the leftmost character of *string* that is not in *set*. If *back* is *true*,
    !> the rightmost position is returned. If all characters of *string* are found
    !> in *set*, the result is zero.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface verify
        module procedure :: verify_string_string
        module procedure :: verify_string_char
        module procedure :: verify_char_string
    end interface verify

    !> Version: experimental
    !>
    !> Moves the allocated character scalar from 'from' to 'to'
    !> [Specifications](../page/specs/stdlib_string_type.html#move)
    interface move
        module procedure :: move_string_string
        module procedure :: move_string_char
        module procedure :: move_char_string
        module procedure :: move_char_char
    end interface move

    !> Lexically compare the order of two character sequences being greater,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface lgt
        module procedure :: lgt_string_string
        module procedure :: lgt_string_char
        module procedure :: lgt_char_string
    end interface lgt

    !> Lexically compare the order of two character sequences being less,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface llt
        module procedure :: llt_string_string
        module procedure :: llt_string_char
        module procedure :: llt_char_string
    end interface llt

    !> Lexically compare the order of two character sequences being greater equal,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface lge
        module procedure :: lge_string_string
        module procedure :: lge_string_char
        module procedure :: lge_char_string
    end interface lge

    !> Lexically compare the order of two character sequences being less equal,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface lle
        module procedure :: lle_string_string
        module procedure :: lle_string_char
        module procedure :: lle_char_string
    end interface lle

    !> Assign a character sequence to a string.
    interface assignment(=)
        module procedure :: assign_string_char
    end interface assignment(=)

    !> Compare two character sequences for being greater, the left-hand side,
    !> the right-hand side or both character sequences can be represented by
    !> a string.
    !>
    !> This operator is elemental and returns a default logical scalar value.
    interface operator(>)
        module procedure :: gt_string_string
        module procedure :: gt_string_char
        module procedure :: gt_char_string
    end interface operator(>)

    !> Compare two character sequences for being less, the left-hand side,
    !> the right-hand side or both character sequences can be represented by
    !> a string.
    !>
    !> This operator is elemental and returns a default logical scalar value.
    interface operator(<)
        module procedure :: lt_string_string
        module procedure :: lt_string_char
        module procedure :: lt_char_string
    end interface operator(<)

    !> Compare two character sequences for being greater than, the left-hand side,
    !> the right-hand side or both character sequences can be represented by
    !> a string.
    !>
    !> This operator is elemental and returns a default logical scalar value.
    interface operator(>=)
        module procedure :: ge_string_string
        module procedure :: ge_string_char
        module procedure :: ge_char_string
    end interface operator(>=)

    !> Compare two character sequences for being less than, the left-hand side,
    !> the right-hand side or both character sequences can be represented by
    !> a string.
    !>
    !> This operator is elemental and returns a default logical scalar value.
    interface operator(<=)
        module procedure :: le_string_string
        module procedure :: le_string_char
        module procedure :: le_char_string
    end interface operator(<=)

    !> Compare two character sequences for equality, the left-hand side,
    !> the right-hand side or both character sequences can be represented by
    !> a string.
    !>
    !> This operator is elemental and returns a default logical scalar value.
    interface operator(==)
        module procedure :: eq_string_string
        module procedure :: eq_string_char
        module procedure :: eq_char_string
    end interface operator(==)

    !> Compare two character sequences for inequality, the left-hand side,
    !> the right-hand side or both character sequences can be represented by
    !> a string.
    !>
    !> This operator is elemental and returns a default logical scalar value.
    interface operator(/=)
        module procedure :: ne_string_string
        module procedure :: ne_string_char
        module procedure :: ne_char_string
    end interface operator(/=)

    !> Concatenate two character sequences, the left-hand side, the right-hand side
    !> or both character sequences can be represented by a string.
    !>
    !> This operator is elemental and returns a scalar character value.
    interface operator(//)
        module procedure :: concat_string_string
        module procedure :: concat_string_char
        module procedure :: concat_char_string
    end interface operator(//)

    !> Write the character sequence hold by the string to a connected formatted
    !> unit.
    interface write(formatted)
        module procedure :: write_formatted
    end interface

    !> Write the character sequence hold by the string to a connected unformatted
    !> unit.
    interface write(unformatted)
        module procedure :: write_unformatted
    end interface

    !> Read a character sequence from a connected unformatted unit into the string.
    interface read(formatted)
        module procedure :: read_formatted
    end interface

    !> Read a character sequence from a connected unformatted unit into the string.
    interface read(unformatted)
        module procedure :: read_unformatted
    end interface


contains

    !> Assign a character sequence to a string.
    elemental subroutine assign_string_char(lhs, rhs)
        type(string_type), intent(inout) :: lhs
        character(len=*), intent(in) :: rhs
        lhs%raw = rhs
    end subroutine assign_string_char


    !> Returns the length of the character sequence represented by the string.
    elemental function len_string(string) result(length)
        type(string_type), intent(in) :: string
        integer :: length

        if (allocated(string%raw)) then
            length = len(string%raw)
        else
            length = 0
        end if

    end function len_string


    !> Returns the length of the character sequence without trailing spaces
    !> represented by the string.
    elemental function len_trim_string(string) result(length)
        type(string_type), intent(in) :: string
        integer :: length

        length = merge(len_trim(string%raw), 0, allocated(string%raw))

    end function len_trim_string


    !> Character-to-integer conversion function.
    elemental function ichar_string(string) result(ich)
        type(string_type), intent(in) :: string
        integer :: ich

        if (allocated(string%raw) .and. len(string) > 0) then
          ich = ichar(string%raw(1:1))
        else
          ich = 0
        end if

    end function ichar_string


    !> Code in ASCII collating sequence.
    elemental function iachar_string(string) result(ich)
        type(string_type), intent(in) :: string
        integer :: ich

        if (allocated(string%raw) .and. len(string) > 0) then
          ich = iachar(string%raw(1:1))
        else
          ich = 0
        end if

    end function iachar_string


    !> Return the character sequence represented by the string.
    pure function char_string(string) result(character_string)
        type(string_type), intent(in) :: string
        character(len=len(string)) :: character_string

        character_string = maybe(string)

    end function char_string

    !> Return the character sequence represented by the string.
    elemental function char_string_pos(string, pos) result(character_string)
        type(string_type), intent(in) :: string
        integer, intent(in) :: pos
        character(len=1) :: character_string

        character_string = merge(string%raw(pos:pos), ' ', allocated(string%raw))

    end function char_string_pos

    !> Return the character sequence represented by the string.
    pure function char_string_range(string, start, last) result(character_string)
        type(string_type), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: last
        character(len=last-start+1) :: character_string

        character_string = merge(string%raw(int(start, long):int(last, long)), &
            repeat(' ', int(len(character_string), long)), allocated(string%raw))

    end function char_string_range


    !> Returns the character sequence hold by the string without trailing spaces.
    elemental function trim_string(string) result(trimmed_string)
        type(string_type), intent(in) :: string
        type(string_type) :: trimmed_string

        trimmed_string = trim(maybe(string))

    end function trim_string


    !> Left-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    elemental function adjustl_string(string) result(adjusted_string)
        type(string_type), intent(in) :: string
        type(string_type) :: adjusted_string

        adjusted_string = adjustl(maybe(string))

    end function adjustl_string


    !> Right-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    elemental function adjustr_string(string) result(adjusted_string)
        type(string_type), intent(in) :: string
        type(string_type) :: adjusted_string

        adjusted_string = adjustr(maybe(string))

    end function adjustr_string


    !> Repeats the character sequence hold by the string by the number of
    !> specified copies.
    elemental function repeat_string(string, ncopies) result(repeated_string)
        type(string_type), intent(in) :: string
        integer, intent(in) :: ncopies
        type(string_type) :: repeated_string

        repeated_string = repeat(maybe(string), ncopies)

    end function repeat_string


    !> Convert the character sequence hold by the input string to lower case
    elemental function to_lower_string(string) result(lowercase_string)
        type(string_type), intent(in) :: string
        type(string_type) :: lowercase_string

        lowercase_string%raw = to_lower_(maybe(string))

    end function to_lower_string


    !> Convert the character sequence hold by the input string to upper case
    elemental function to_upper_string(string) result(uppercase_string)
        type(string_type), intent(in) :: string
        type(string_type) :: uppercase_string

        uppercase_string%raw = to_upper_(maybe(string))

    end function to_upper_string


    !> Convert the character sequence hold by the input string to title case
    elemental function to_title_string(string) result(titlecase_string)
        type(string_type), intent(in) :: string
        type(string_type) :: titlecase_string

        titlecase_string%raw = to_title_(maybe(string))

    end function to_title_string

    !> Convert the character sequence hold by the input string to sentence case
    elemental function to_sentence_string(string) result(sentence_string)
        type(string_type), intent(in) :: string
        type(string_type) :: sentence_string

        sentence_string%raw = to_sentence_(maybe(string))

    end function to_sentence_string


    !> Reverse the character sequence hold by the input string
    elemental function reverse_string(string) result(reversed_string)
        type(string_type), intent(in) :: string
        type(string_type) :: reversed_string

        reversed_string%raw = reverse_(maybe(string))

    end function reverse_string


    !> Position of a sequence of character within a character sequence.
    !> In this version both character sequences are represented by a string.
    elemental function index_string_string(string, substring, back) result(pos)
        type(string_type), intent(in) :: string
        type(string_type), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos

        pos = index(maybe(string), maybe(substring), optval(back, .false.))

    end function index_string_string

    !> Position of a sequence of character within a character sequence.
    !> In this version the main character sequence is represented by a string.
    elemental function index_string_char(string, substring, back) result(pos)
        type(string_type), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos

        pos = index(maybe(string), substring, optval(back, .false.))

    end function index_string_char

    !> Position of a sequence of character within a character sequence.
    !> In this version the sub character sequence is represented by a string.
    elemental function index_char_string(string, substring, back) result(pos)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos

        pos = index(string, maybe(substring), optval(back, .false.))

    end function index_char_string


    !> Scan a character sequence for any of the characters in a set of characters.
    !> In this version both the character sequence and the character set are
    !> represented by a string.
    elemental function scan_string_string(string, set, back) result(pos)
        type(string_type), intent(in) :: string
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = scan(maybe(string), maybe(set), optval(back, .false.))

    end function scan_string_string

    !> Scan a character sequence for any of the characters in a set of characters.
    !> In this version the character sequences is represented by a string.
    elemental function scan_string_char(string, set, back) result(pos)
        type(string_type), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = scan(maybe(string), set, optval(back, .false.))

    end function scan_string_char

    !> Scan a character sequence for any of the characters in a set of characters.
    !> In this version the set of characters is represented by a string.
    elemental function scan_char_string(string, set, back) result(pos)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = scan(string, maybe(set), optval(back, .false.))

    end function scan_char_string


    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters. In this version both the character sequence and
    !> the character set are represented by a string.
    elemental function verify_string_string(string, set, back) result(pos)
        type(string_type), intent(in) :: string
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = verify(maybe(string), maybe(set), optval(back, .false.))

    end function verify_string_string

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters. In this version the character sequences is
    !> represented by a string.
    elemental function verify_string_char(string, set, back) result(pos)
        type(string_type), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = verify(maybe(string), set, optval(back, .false.))

    end function verify_string_char

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters. In this version the set of characters is
    !> represented by a string.
    elemental function verify_char_string(string, set, back) result(pos)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = verify(string, maybe(set), optval(back, .false.))

    end function verify_char_string

    !> Moves the allocated character scalar from 'from' to 'to'
    !> No output
    elemental subroutine move_string_string(from, to)
        type(string_type), intent(inout), target :: from
        type(string_type), intent(inout), target :: to
        type(string_type), pointer :: fromp

        fromp => from
        if (associated(fromp,to)) return
        call move_alloc(from%raw, to%raw)

    end subroutine move_string_string

    !> Moves the allocated character scalar from 'from' to 'to'
    !> No output
    pure subroutine move_string_char(from, to)
        type(string_type), intent(inout) :: from
        character(len=:), intent(out), allocatable :: to

        call move_alloc(from%raw, to)

    end subroutine move_string_char

    !> Moves the allocated character scalar from 'from' to 'to'
    !> No output
    pure subroutine move_char_string(from, to)
        character(len=:), intent(inout), allocatable :: from
        type(string_type), intent(out) :: to

        call move_alloc(from, to%raw)

    end subroutine move_char_string

    !> Moves the allocated character scalar from 'from' to 'to'
    !> No output
    pure subroutine move_char_char(from, to)
        character(len=:), intent(inout), allocatable :: from
        character(len=:), intent(out), allocatable :: to

        call move_alloc(from, to)

    end subroutine move_char_char

    !> Compare two character sequences for being greater.
    !> In this version both character sequences are by a string.
    elemental function gt_string_string(lhs, rhs) result(is_gt)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_gt

        is_gt = maybe(lhs) > maybe(rhs)

    end function gt_string_string

    !> Compare two character sequences for being greater.
    !> In this version the left-hand side character sequences is by a string.
    elemental function gt_string_char(lhs, rhs) result(is_gt)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_gt

        is_gt = maybe(lhs) > rhs

    end function gt_string_char

    !> Compare two character sequences for being greater.
    !> In this version the right-hand side character sequences is by a string.
    elemental function gt_char_string(lhs, rhs) result(is_gt)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_gt

        is_gt = lhs > maybe(rhs)

    end function gt_char_string


    !> Compare two character sequences for being less.
    !> In this version both character sequences are by a string.
    elemental function lt_string_string(lhs, rhs) result(is_lt)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_string_string


    !> Compare two character sequences for being less.
    !> In this version the left-hand side character sequences is by a string.
    elemental function lt_string_char(lhs, rhs) result(is_lt)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_string_char

    !> Compare two character sequences for being less.
    !> In this version the right-hand side character sequences is by a string.
    elemental function lt_char_string(lhs, rhs) result(is_lt)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_char_string


    !> Compare two character sequences for being greater or equal.
    !> In this version both character sequences are by a string.
    elemental function ge_string_string(lhs, rhs) result(is_ge)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_string_string

    !> Compare two character sequences for being greater or equal.
    !> In this version the left-hand side character sequences is by a string.
    elemental function ge_string_char(lhs, rhs) result(is_ge)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_string_char

    !> Compare two character sequences for being greater or equal
    !> In this version the right-hand side character sequences is by a string.
    elemental function ge_char_string(lhs, rhs) result(is_ge)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_char_string


    !> Compare two character sequences for being less or equal.
    !> In this version both character sequences are by a string.
    elemental function le_string_string(lhs, rhs) result(is_le)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_string_string

    !> Compare two character sequences for being less or equal.
    !> In this version the left-hand side character sequences is by a string.
    elemental function le_string_char(lhs, rhs) result(is_le)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_string_char

    !> Compare two character sequences for being less or equal
    !> In this version the right-hand side character sequences is by a string.
    elemental function le_char_string(lhs, rhs) result(is_le)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_char_string


    !> Compare two character sequences for equality.
    !> In this version both character sequences are by a string.
    elemental function eq_string_string(lhs, rhs) result(is_eq)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_string_string

    !> Compare two character sequences for equality.
    !> In this version the left-hand side character sequences is by a string.
    elemental function eq_string_char(lhs, rhs) result(is_eq)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_string_char

    !> Compare two character sequences for equality.
    !> In this version the right-hand side character sequences is by a string.
    elemental function eq_char_string(lhs, rhs) result(is_eq)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_char_string


    !> Compare two character sequences for inequality.
    !> In this version both character sequences are by a string.
    elemental function ne_string_string(lhs, rhs) result(is_ne)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_string_string

    !> Compare two character sequences for inequality.
    !> In this version the left-hand side character sequences is by a string.
    elemental function ne_string_char(lhs, rhs) result(is_ne)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_string_char

    !> Compare two character sequences for inequality.
    !> In this version the right-hand side character sequences is by a string.
    elemental function ne_char_string(lhs, rhs) result(is_ne)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_char_string


    !> Lexically compare two character sequences for being greater.
    !> In this version both character sequences are by a string.
    elemental function lgt_string_string(lhs, rhs) result(is_lgt)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(maybe(lhs), maybe(rhs))

    end function lgt_string_string

    !> Lexically compare two character sequences for being greater.
    !> In this version the left-hand side character sequences is by a string.
    elemental function lgt_string_char(lhs, rhs) result(is_lgt)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(maybe(lhs), rhs)

    end function lgt_string_char

    !> Lexically compare two character sequences for being greater.
    !> In this version the right-hand side character sequences is by a string.
    elemental function lgt_char_string(lhs, rhs) result(is_lgt)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(lhs, maybe(rhs))

    end function lgt_char_string


    !> Lexically compare two character sequences for being less.
    !> In this version both character sequences are by a string.
    elemental function llt_string_string(lhs, rhs) result(is_llt)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(maybe(lhs), maybe(rhs))

    end function llt_string_string

    !> Lexically compare two character sequences for being less.
    !> In this version the left-hand side character sequences is by a string.
    elemental function llt_string_char(lhs, rhs) result(is_llt)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(maybe(lhs), rhs)

    end function llt_string_char

    !> Lexically compare two character sequences for being less.
    !> In this version the right-hand side character sequences is by a string.
    elemental function llt_char_string(lhs, rhs) result(is_llt)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(lhs, maybe(rhs))

    end function llt_char_string


    !> Lexically compare two character sequences for being greater or equal.
    !> In this version both character sequences are by a string.
    elemental function lge_string_string(lhs, rhs) result(is_lge)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(maybe(lhs), maybe(rhs))

    end function lge_string_string

    !> Lexically compare two character sequences for being greater or equal.
    !> In this version the left-hand side character sequences is by a string.
    elemental function lge_string_char(lhs, rhs) result(is_lge)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(maybe(lhs), rhs)

    end function lge_string_char

    !> Lexically compare two character sequences for being greater or equal
    !> In this version the right-hand side character sequences is by a string.
    elemental function lge_char_string(lhs, rhs) result(is_lge)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(lhs, maybe(rhs))

    end function lge_char_string


    !> Lexically compare two character sequences for being less or equal.
    !> In this version both character sequences are by a string.
    elemental function lle_string_string(lhs, rhs) result(is_lle)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(maybe(lhs), maybe(rhs))

    end function lle_string_string

    !> Lexically compare two character sequences for being less or equal.
    !> In this version the left-hand side character sequences is by a string.
    elemental function lle_string_char(lhs, rhs) result(is_lle)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(maybe(lhs), rhs)

    end function lle_string_char

    !> Lexically compare two character sequences for being less or equal
    !> In this version the right-hand side character sequences is by a string.
    elemental function lle_char_string(lhs, rhs) result(is_lle)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(lhs, maybe(rhs))

    end function lle_char_string


    !> Concatenate two character sequences.
    !> In this version both character sequences are by a string.
    elemental function concat_string_string(lhs, rhs) result(string)
        type(string_type), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        type(string_type) :: string

        string%raw = maybe(lhs) // maybe(rhs)

    end function concat_string_string

    !> Concatenate two character sequences.
    !> In this version the left-hand side character sequences is by a string.
    elemental function concat_string_char(lhs, rhs) result(string)
        type(string_type), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        type(string_type) :: string

        string%raw = maybe(lhs) // rhs

    end function concat_string_char

    !> Concatenate two character sequences.
    !> In this version the right-hand side character sequences is by a string.
    elemental function concat_char_string(lhs, rhs) result(string)
        character(len=*), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        type(string_type) :: string

        string%raw = lhs // maybe(rhs)

    end function concat_char_string


    !> Write the character sequence hold by the string to a connected unformatted
    !> unit.
    subroutine write_unformatted(string, unit, iostat, iomsg)
        type(string_type), intent(in) :: string
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        write(unit, iostat=iostat, iomsg=iomsg) int(len(string), long)
        if (iostat == 0) then
            write(unit, iostat=iostat, iomsg=iomsg) maybe(string)
        end if

    end subroutine write_unformatted

    !> Write the character sequence hold by the string to a connected formatted
    !> unit.
    subroutine write_formatted(string, unit, iotype, v_list, iostat, iomsg)
        type(string_type), intent(in) :: string
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg

        select case(iotype)
        case("LISTDIRECTED")
            write(unit, '(a)', iostat=iostat, iomsg=iomsg) maybe(string)
        case("NAMELIST")
            error stop "[Fatal] This implementation does not support namelist output"
        case default ! DT*
            select case(size(v_list))
            case(0) ! DT
                write(unit, '(a)', iostat=iostat, iomsg=iomsg) maybe(string)
            case default
                error stop "[Fatal] This implementation does not support v_list formatters"
            end select
        end select

    end subroutine write_formatted


    !> Read a character sequence from a connected unformatted unit into the string.
    subroutine read_unformatted(string, unit, iostat, iomsg)
        type(string_type), intent(inout) :: string
        integer, intent(in)    :: unit
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        character(len=:), allocatable :: buffer
        integer(long) :: chunk

        read(unit, iostat=iostat, iomsg=iomsg) chunk
        if (iostat == 0) then
            allocate(character(len=chunk) :: buffer)
            read(unit, iostat=iostat, iomsg=iomsg) buffer
            string%raw = buffer
        end if

    end subroutine read_unformatted

    !> Read a character sequence from a connected formatted unit into the string.
    subroutine read_formatted(string, unit, iotype, v_list, iostat, iomsg)
        type(string_type), intent(inout) :: string
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        character(len=:), allocatable :: line

        call unused_dummy_argument(v_list)

        select case(iotype)
        case("LISTDIRECTED")
            call read_line(unit, line, iostat, iomsg)
        case("NAMELIST")
            error stop "[Fatal] This implementation does not support namelist input"
        case default ! DT*
            error stop "[Fatal] This implementation does not support dt formatters"
        end select

        string%raw = line

    contains

        !> Internal routine to read a whole record from a formatted unit
        subroutine read_line(unit, line, iostat, iomsg)
            integer, intent(in) :: unit
            character(len=:), allocatable, intent(out) :: line
            integer, intent(out) :: iostat
            character(len=*), intent(inout) :: iomsg
            integer, parameter :: buffer_size = 512
            character(len=buffer_size) :: buffer
            integer :: chunk
            line = ''
            do
                read(unit, '(a)', iostat=iostat, iomsg=iomsg, size=chunk, advance='no') &
                    buffer
                if (iostat > 0) exit
                line = line // buffer(:chunk)
                if (iostat < 0) exit
            end do

            if (is_iostat_eor(iostat)) then
                iostat = 0
            end if
        end subroutine read_line

    end subroutine read_formatted


    !> Do nothing but mark an unused dummy argument as such to acknowledge compile
    !> time warning like:
    !>
    !>   Warning: Unused dummy argument ‘dummy’ at (1) [-Wunused-dummy-argument]
    !>
    !> We deeply trust in the compiler to inline and optimize this piece of code away.
    elemental subroutine unused_dummy_argument(dummy)
        class(*), intent(in) :: dummy
        associate(dummy => dummy); end associate
    end subroutine unused_dummy_argument


    !> Safely return the character sequences represented by the string
    pure function maybe(string) result(maybe_string)
        type(string_type), intent(in) :: string
        character(len=len(string)) :: maybe_string
        if (allocated(string%raw)) then
            maybe_string = string%raw
        else
            maybe_string = ''
        end if
    end function maybe


end module stdlib_string_type
