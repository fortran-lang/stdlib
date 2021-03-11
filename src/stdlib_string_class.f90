! SPDX-Identifier: MIT

!> Abstract base class (ABC) specification of an extendible string type to hold an
!> arbitrary character sequence.
!>
!> A minimal implementation of the string class has to provide only a setter function
!> in form of an assignment from a fixed length character variable and getter functions
!> for returning the whole string, a character at a certain index and a range of
!> characters within the bounds of the character sequence.
!>
!> The ABC takes care of providing the implementations for all functionality that
!> is intrinsic to character variables in Fortran, therefore an implementation should
!> reexport all overloaded generic interfaces from the ABC. Any string class
!> implementation will be compatibile with the non-extendible [[string_type]]
!> and fixed length and deferred length character variables by those means.
!>
!> Implementations of the string class that are encouraged to overwrite the type
!> bound procedures providing those functionality in the ABC with optimized
!> algorithms suitable for their respective representation of the character sequence.
!>
!> The specification of this module is available [here](../page/specs/stdlib_string_class.html).
module stdlib_string_class
    use stdlib_string_type, only : string_type, to_char => char, assignment(=), &
        write(formatted), read(formatted), write(unformatted), read(unformatted)
    implicit none
    private

    public :: string_class
    public :: len, len_trim, trim, index, scan, verify, repeat, adjustr, adjustl
    public :: lgt, lge, llt, lle, char, ichar, iachar


    !> Returns the length of the character sequence represented by the string.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface len
        module procedure :: len_object
    end interface len

    !> Returns the length of the character sequence without trailing spaces
    !> represented by the string.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface len_trim
        module procedure :: len_trim_object
    end interface len_trim

    !> Returns the character sequence hold by the string without trailing spaces.
    !>
    !> This method is elemental and returns a scalar character value.
    interface trim
        module procedure :: trim_object
    end interface trim

    !> Left-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    !>
    !> This method is elemental and returns a scalar character value.
    interface adjustl
        module procedure :: adjustl_object
    end interface adjustl

    !> Right-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    !>
    !> This method is elemental and returns a scalar character value.
    interface adjustr
        module procedure :: adjustr_object
    end interface adjustr

    !> Repeats the character sequence hold by the string by the number of
    !> specified copies.
    !>
    !> This method is elemental and returns a scalar character value.
    interface repeat
        module procedure :: repeat_object
    end interface repeat

    !> Return the character sequence represented by the string.
    !>
    !> This method is elemental and returns a scalar character value.
    interface char
        module procedure :: char_object
        module procedure :: char_object_pos
        module procedure :: char_object_range
    end interface char

    !> Character-to-integer conversion function.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface ichar
        module procedure :: ichar_object
    end interface ichar

    !> Code in ASCII collating sequence.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface iachar
        module procedure :: iachar_object
    end interface iachar

    !> Position of a *substring* within a *string*.
    !>
    !> Returns the position of the start of the leftmost or rightmost occurrence
    !> of string *substring* in *string*, counting from one. If *substring* is not
    !> present in *string*, zero is returned.
    !>
    !> This method is elemental and returns a default integer scalar value.
    interface index
        module procedure :: index_object_object
        module procedure :: index_object_string
        module procedure :: index_object_char
        module procedure :: index_string_object
        module procedure :: index_char_object
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
        module procedure :: scan_object_object
        module procedure :: scan_object_string
        module procedure :: scan_object_char
        module procedure :: scan_string_object
        module procedure :: scan_char_object
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
        module procedure :: verify_object_object
        module procedure :: verify_object_string
        module procedure :: verify_object_char
        module procedure :: verify_string_object
        module procedure :: verify_char_object
    end interface verify

    !> Lexically compare the order of two character sequences being greater,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface lgt
        module procedure :: lgt_object_object
        module procedure :: lgt_object_string
        module procedure :: lgt_string_object
        module procedure :: lgt_object_char
        module procedure :: lgt_char_object
    end interface lgt

    !> Lexically compare the order of two character sequences being less,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface llt
        module procedure :: llt_object_object
        module procedure :: llt_object_string
        module procedure :: llt_string_object
        module procedure :: llt_object_char
        module procedure :: llt_char_object
    end interface llt

    !> Lexically compare the order of two character sequences being greater equal,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface lge
        module procedure :: lge_object_object
        module procedure :: lge_object_string
        module procedure :: lge_string_object
        module procedure :: lge_object_char
        module procedure :: lge_char_object
    end interface lge

    !> Lexically compare the order of two character sequences being less equal,
    !> The left-hand side, the right-hand side or both character sequences can
    !> be represented by a string.
    !>
    !> This method is elemental and returns a default logical scalar value.
    interface lle
        module procedure :: lle_object_object
        module procedure :: lle_object_string
        module procedure :: lle_string_object
        module procedure :: lle_object_char
        module procedure :: lle_char_object
    end interface lle


    !> Abstract base class for string objects
    type, abstract :: string_class
    contains
        private

        !> Assign a character sequence to a string object.
        generic, public :: assignment(=) => assign_object_char
        ! BUG: Intel 2021 requires deferred bindings to be public
        procedure(assign_object_char_interface), public, deferred :: assign_object_char

        !> Assign a string type to a string object.
        generic, public :: assignment(=) => assign_object_string
        procedure :: assign_object_string

        !> Assign a string type to a string object.
        generic, public :: assignment(=) => assign_object_object
        procedure :: assign_object_object

        !> Returns the length of the character sequence represented by the string.
        ! BUG: Intel 2021 requires deferred bindings to be public
        procedure(get_int_interface), public, deferred :: get_len

        !> Returns the length of the character sequence without trailing spaces
        !> represented by the string.
        ! BUG: Intel 2021 requires deferred bindings to be public
        procedure(get_int_interface), public, deferred :: get_len_trim

        !> Character-to-integer conversion function.
        procedure :: get_ichar

        !> Code in ASCII collating sequence.
        procedure :: get_iachar

        !> Return the character sequence represented by the string.
        ! BUG: Intel 2021 requires deferred bindings to be public
        procedure(get_char_interface), public, deferred :: get_char

        !> Return the character sequence represented by the string.
        ! BUG: Intel 2021 requires deferred bindings to be public
        procedure(get_char_pos_interface), public, deferred :: get_char_pos

        !> Return the character sequence represented by the string.
        ! BUG: Intel 2021 requires deferred bindings to be public
        procedure(get_char_range_interface), public, deferred :: get_char_range

        !> Left-adjust the character sequence represented by the string.
        !> The length of the character sequence remains unchanged.
        procedure :: get_trim

        !> Left-adjust the character sequence represented by the string.
        !> The length of the character sequence remains unchanged.
        procedure :: get_adjustl

        !> Right-adjust the character sequence represented by the string.
        !> The length of the character sequence remains unchanged.
        procedure :: get_adjustr

        !> Repeats the character sequence hold by the string by the number of
        !> specified copies.
        procedure :: get_repeat

        !> Scan a *string* for the presence of a *set* of characters. Scans a *string* for
        !> any of the characters in a *set* of characters.
        !>
        !> If *back* is either absent or *false*, this function returns the position
        !> of the leftmost character of *string* that is in *set*. If *back* is *true*,
        !> the rightmost position is returned. If no character of *set* is found in
        !> *string*, the result is zero.
        !>
        !> This method is elemental and returns a default integer scalar value.
        generic :: get_scan => get_scan_object, get_scan_string, get_scan_char
        !> Implementation of scanning against a set provided as string object
        procedure :: get_scan_object
        !> Implementation of scanning against a set provided as string type
        procedure :: get_scan_string
        !> Implementation of scanning against a set provided as character scalar
        procedure :: get_scan_char

        !> Scan a string for the absence of a set of characters. Verifies that all
        !> the characters in string belong to the set of characters in set.
        !>
        !> If *back* is either absent or *false*, this function returns the position
        !> of the leftmost character of *string* that is not in *set*. If *back* is *true*,
        !> the rightmost position is returned. If all characters of *string* are found
        !> in *set*, the result is zero.
        !>
        !> This method is elemental and returns a default integer scalar value.
        generic :: get_verify => get_verify_object, get_verify_string, get_verify_char
        !> Implementation of verifying against a set provided as string object
        procedure :: get_verify_object
        !> Implementation of verifying against a set provided as string type
        procedure :: get_verify_string
        !> Implementation of verifying against a set provided as character scalar
        procedure :: get_verify_char

        !> Position of a *substring* within a *string*.
        !>
        !> Returns the position of the start of the leftmost or rightmost occurrence
        !> of string *substring* in *string*, counting from one. If *substring* is not
        !> present in *string*, zero is returned.
        !>
        !> This method is elemental and returns a default integer scalar value.
        generic :: get_index => get_index_object, get_index_string, get_index_char
        !> Implementation of finding a substring provided as string object
        procedure :: get_index_object
        !> Implementation of finding a substring provided as string type
        procedure :: get_index_string
        !> Implementation of finding a substring provided as character value
        procedure :: get_index_char

        !> Lexically compare two character sequences for being greater.
        generic :: is_lgt => is_lgt_object, is_lgt_string, is_lgt_char
        !> Implementation of lexical comparison with RHS provided as string object
        procedure, pass(lhs) :: is_lgt_object
        !> Implementation of lexical comparison with RHS provided as string type
        procedure, pass(lhs) :: is_lgt_string
        !> Implementation of lexical comparison with RHS provided as character value
        procedure, pass(lhs) :: is_lgt_char

        !> Lexically compare two character sequences for being less than.
        generic :: is_llt => is_llt_object, is_llt_string, is_llt_char
        !> Implementation of lexical comparison with RHS provided as string object
        procedure, pass(lhs) :: is_llt_object
        !> Implementation of lexical comparison with RHS provided as string type
        procedure, pass(lhs) :: is_llt_string
        !> Implementation of lexical comparison with RHS provided as character value
        procedure, pass(lhs) :: is_llt_char

        !> Lexically compare two character sequences for being greater than or equal.
        generic :: is_lge => is_lge_object, is_lge_string, is_lge_char
        !> Implementation of lexical comparison with RHS provided as string object
        procedure, pass(lhs) :: is_lge_object
        !> Implementation of lexical comparison with RHS provided as string type
        procedure, pass(lhs) :: is_lge_string
        !> Implementation of lexical comparison with RHS provided as character value
        procedure, pass(lhs) :: is_lge_char

        !> Lexically compare two character sequences for being less than or equal.
        generic :: is_lle => is_lle_object, is_lle_string, is_lle_char
        !> Implementation of lexical comparison with RHS provided as string object
        procedure, pass(lhs) :: is_lle_object
        !> Implementation of lexical comparison with RHS provided as string type
        procedure, pass(lhs) :: is_lle_string
        !> Implementation of lexical comparison with RHS provided as character value
        procedure, pass(lhs) :: is_lle_char

        !> Compare two character sequences for being greater.
        generic, public :: operator(>) => gt_object_object, gt_object_string, &
            gt_string_object, gt_object_char, gt_char_object
        procedure, pass(lhs) :: gt_object_object
        procedure, pass(lhs) :: gt_object_string
        procedure, pass(rhs) :: gt_string_object
        procedure, pass(lhs) :: gt_object_char
        procedure, pass(rhs) :: gt_char_object

        !> Compare two character sequences for being less.
        generic, public :: operator(<) => lt_object_object, lt_object_string, &
            lt_string_object, lt_object_char, lt_char_object
        procedure, pass(lhs) :: lt_object_object
        procedure, pass(lhs) :: lt_object_string
        procedure, pass(rhs) :: lt_string_object
        procedure, pass(lhs) :: lt_object_char
        procedure, pass(rhs) :: lt_char_object

        !> Compare two character sequences for being greater or equal.
        generic, public :: operator(>=) => ge_object_object, ge_object_string, &
            ge_string_object, ge_object_char, ge_char_object
        procedure, pass(lhs) :: ge_object_object
        procedure, pass(lhs) :: ge_object_string
        procedure, pass(rhs) :: ge_string_object
        procedure, pass(lhs) :: ge_object_char
        procedure, pass(rhs) :: ge_char_object

        !> Compare two character sequences for being less or equal.
        generic, public :: operator(<=) => le_object_object, le_object_string, &
            le_string_object, le_object_char, le_char_object
        procedure, pass(lhs) :: le_object_object
        procedure, pass(lhs) :: le_object_string
        procedure, pass(rhs) :: le_string_object
        procedure, pass(lhs) :: le_object_char
        procedure, pass(rhs) :: le_char_object

        !> Compare two character sequences for equality.
        generic, public :: operator(==) => eq_object_object, eq_object_string, &
            eq_string_object, eq_object_char, eq_char_object
        procedure, pass(lhs) :: eq_object_object
        procedure, pass(lhs) :: eq_object_string
        procedure, pass(rhs) :: eq_string_object
        procedure, pass(lhs) :: eq_object_char
        procedure, pass(rhs) :: eq_char_object

        !> Compare two character sequences for inequality.
        generic, public :: operator(/=) => ne_object_object, ne_object_string, &
            ne_string_object, ne_object_char, ne_char_object
        procedure, pass(lhs) :: ne_object_object
        procedure, pass(lhs) :: ne_object_string
        procedure, pass(rhs) :: ne_string_object
        procedure, pass(lhs) :: ne_object_char
        procedure, pass(rhs) :: ne_char_object

        !> Compare two character sequences for inequality.
        generic, public :: operator(//) => concat_object_object, concat_object_string, &
            concat_string_object, concat_object_char, concat_char_object
        procedure, pass(lhs) :: concat_object_object
        procedure, pass(lhs) :: concat_object_string
        procedure, pass(rhs) :: concat_string_object
        procedure, pass(lhs) :: concat_object_char
        procedure, pass(rhs) :: concat_char_object

        !> Write the character sequence hold by the string to a connected unformatted
        !> unit.
        generic, public :: write(unformatted) => write_unformatted
        procedure :: write_unformatted

        !> Write the character sequence hold by the string to a connected formatted
        !> unit.
        generic, public :: write(formatted) => write_formatted
        procedure :: write_formatted

        !> Read a character sequence from a connected unformatted unit into the string.
        generic, public :: read(unformatted) => read_unformatted
        procedure :: read_unformatted

        !> Read a character sequence from a connected formatted unit into the string.
        generic, public :: read(formatted) => read_formatted
        procedure :: read_formatted

    end type string_class


    abstract interface
        !> Assign a character sequence to a string object.
        elemental subroutine assign_object_char_interface(lhs, rhs)
            import :: string_class
            implicit none
            class(string_class), intent(inout) :: lhs
            character(len=*), intent(in) :: rhs
        end subroutine assign_object_char_interface

        !> Return a integer value representing a property of the character sequence, like
        !> - the length of the character sequence
        !> - the character-to-integer conversion
        elemental function get_int_interface(self) result(val)
            import :: string_class
            implicit none
            class(string_class), intent(in) :: self
            integer :: val
        end function get_int_interface


        !> Return the character sequence represented by the string.
        pure function get_char_interface(self) result(character_string)
            import :: string_class
            implicit none
            class(string_class), intent(in) :: self
            character(len=:), allocatable :: character_string
        end function get_char_interface

        !> Return the character sequence represented by the string.
        elemental function get_char_pos_interface(self, pos) result(character_string)
            import :: string_class
            implicit none
            class(string_class), intent(in) :: self
            integer, intent(in) :: pos
            character(len=1) :: character_string
        end function get_char_pos_interface

        !> Return the character sequence represented by the string.
        pure function get_char_range_interface(self, start, last) result(character_string)
            import :: string_class
            implicit none
            class(string_class), intent(in) :: self
            integer, intent(in) :: start
            integer, intent(in) :: last
            character(len=last-start+1) :: character_string
        end function get_char_range_interface


        !> Logical operator interface for a string class object.
        !> In this version both character sequences are by a string.
        elemental function op_object_object_interface(lhs, rhs) result(res)
            import :: string_class
            implicit none
            class(string_class), intent(in) :: lhs
            class(string_class), intent(in) :: rhs
            logical :: res
        end function op_object_object_interface

        !> Logical operator interface for a string class object.
        !> In this version the left-hand side character sequences is by a string.
        elemental function op_object_char_interface(lhs, rhs) result(res)
            import :: string_class
            implicit none
            class(string_class), intent(in) :: lhs
            character(len=*), intent(in) :: rhs
            logical :: res
        end function op_object_char_interface

        !> Logical operator interface for a string class object.
        !> In this version the right-hand side character sequences is by a string.
        elemental function op_char_object_interface(lhs, rhs) result(res)
            import :: string_class
            implicit none
            character(len=*), intent(in) :: lhs
            class(string_class), intent(in) :: rhs
            logical :: res
        end function op_char_object_interface


        !> Concatenate two character sequences.
        !> In this version both character sequences are by a string.
        elemental function concat_object_object_interface(lhs, rhs) result(string)
            import :: string_class, string_type
            implicit none
            class(string_class), intent(in) :: lhs
            class(string_class), intent(in) :: rhs
            type(string_type) :: string
        end function concat_object_object_interface

        !> Concatenate two character sequences.
        !> In this version the left-hand side character sequences is by a string.
        elemental function concat_object_char_interface(lhs, rhs) result(string)
            import :: string_class, string_type
            implicit none
            class(string_class), intent(in) :: lhs
            character(len=*), intent(in) :: rhs
            type(string_type) :: string
        end function concat_object_char_interface

        !> Concatenate two character sequences.
        !> In this version the right-hand side character sequences is by a string.
        elemental function concat_char_object_interface(lhs, rhs) result(string)
            import :: string_class, string_type
            implicit none
            character(len=*), intent(in) :: lhs
            class(string_class), intent(in) :: rhs
            type(string_type) :: string
        end function concat_char_object_interface
    end interface

contains

    !> Assign a string type to a string object.
    elemental subroutine assign_object_string(lhs, rhs)
        class(string_class), intent(inout) :: lhs
        type(string_type), intent(in) :: rhs
        lhs = to_char(rhs)
    end subroutine assign_object_string

    !> Assign a string object to a string object.
    elemental subroutine assign_object_object(lhs, rhs)
        class(string_class), intent(inout) :: lhs
        class(string_class), intent(in) :: rhs
        lhs = rhs%get_char()
    end subroutine assign_object_object


    !> Character-to-integer conversion function.
    elemental function get_ichar(self) result(ich)
        class(string_class), intent(in) :: self
        integer :: ich
        ich = ichar(char(self))
    end function get_ichar

    !> Code in ASCII collating sequence.
    elemental function get_iachar(self) result(ich)
        class(string_class), intent(in) :: self
        integer :: ich
        ich = iachar(char(self))
    end function get_iachar


    !> Returns the character sequence hold by the string without trailing spaces.
    elemental function get_trim(self) result(trimmed_string)
        class(string_class), intent(in) :: self
        type(string_type) :: trimmed_string

        trimmed_string = trim(char(self))

    end function get_trim


    !> Left-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    elemental function get_adjustl(self) result(adjusted_string)
        class(string_class), intent(in) :: self
        type(string_type) :: adjusted_string

        adjusted_string = adjustl(char(self))

    end function get_adjustl


    !> Right-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    elemental function get_adjustr(self) result(adjusted_string)
        class(string_class), intent(in) :: self
        type(string_type) :: adjusted_string

        adjusted_string = adjustr(char(self))

    end function get_adjustr


    !> Repeats the character sequence hold by the string by the number of
    !> specified copies.
    elemental function get_repeat(self, ncopies) result(repeated_string)
        class(string_class), intent(in) :: self
        integer, intent(in) :: ncopies
        type(string_type) :: repeated_string

        repeated_string = repeat(char(self), ncopies)

    end function get_repeat


    !> Position of a sequence of character within a character sequence.
    elemental function get_index_object(self, substring, back) result(pos)
        class(string_class), intent(in) :: self
        class(string_class), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos

        pos = index(char(self), char(substring), &
            merge(back, .false., present(back)))

    end function get_index_object

    !> Position of a sequence of character within a character sequence.
    elemental function get_index_string(self, substring, back) result(pos)
        class(string_class), intent(in) :: self
        type(string_type), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos

        pos = index(char(self), to_char(substring), &
            merge(back, .false., present(back)))

    end function get_index_string

    !> Position of a sequence of character within a character sequence.
    elemental function get_index_char(self, substring, back) result(pos)
        class(string_class), intent(in) :: self
        character(len=*), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos

        pos = index(char(self), substring, &
            merge(back, .false., present(back)))

    end function get_index_char


    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function get_scan_object(self, set, back) result(pos)
        class(string_class), intent(in) :: self
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = scan(char(self), char(set), &
            merge(back, .false., present(back)))

    end function get_scan_object

    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function get_scan_string(self, set, back) result(pos)
        class(string_class), intent(in) :: self
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = scan(char(self), to_char(set), &
            merge(back, .false., present(back)))

    end function get_scan_string

    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function get_scan_char(self, set, back) result(pos)
        class(string_class), intent(in) :: self
        character(len=*), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = scan(char(self), set, &
            merge(back, .false., present(back)))

    end function get_scan_char


    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function get_verify_object(self, set, back) result(pos)
        class(string_class), intent(in) :: self
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = verify(char(self), char(set), &
            merge(back, .false., present(back)))

    end function get_verify_object

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function get_verify_string(self, set, back) result(pos)
        class(string_class), intent(in) :: self
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = verify(char(self), to_char(set), &
            merge(back, .false., present(back)))

    end function get_verify_string

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function get_verify_char(self, set, back) result(pos)
        class(string_class), intent(in) :: self
        character(len=*), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos

        pos = verify(char(self), set, &
            merge(back, .false., present(back)))

    end function get_verify_char


    !> Lexically compare two character sequences for being greater than.
    elemental function is_lgt_object(lhs, rhs) result(is_lgt)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(char(lhs), char(rhs))

    end function is_lgt_object

    !> Lexically compare two character sequences for being greater than.
    elemental function is_lgt_string(lhs, rhs) result(is_lgt)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(char(lhs), to_char(rhs))

    end function is_lgt_string

    !> Lexically compare two character sequences for being greater than.
    elemental function is_lgt_char(lhs, rhs) result(is_lgt)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(char(lhs), rhs)

    end function is_lgt_char

    !> Lexically compare two character sequences for being less than.
    elemental function is_llt_object(lhs, rhs) result(is_llt)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(char(lhs), char(rhs))

    end function is_llt_object

    !> Lexically compare two character sequences for being less than.
    elemental function is_llt_string(lhs, rhs) result(is_llt)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(char(lhs), to_char(rhs))

    end function is_llt_string

    !> Lexically compare two character sequences for being less than.
    elemental function is_llt_char(lhs, rhs) result(is_llt)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(char(lhs), rhs)

    end function is_llt_char

    !> Lexically compare two character sequences for being greater than or equal.
    elemental function is_lge_object(lhs, rhs) result(is_lge)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(char(lhs), char(rhs))

    end function is_lge_object

    !> Lexically compare two character sequences for being greater or equal.
    elemental function is_lge_string(lhs, rhs) result(is_lge)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(char(lhs), to_char(rhs))

    end function is_lge_string

    !> Lexically compare two character sequences for being greater than or equal.
    elemental function is_lge_char(lhs, rhs) result(is_lge)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(char(lhs), rhs)

    end function is_lge_char

    !> Lexically compare two character sequences for being less than or equal.
    elemental function is_lle_object(lhs, rhs) result(is_lle)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(char(lhs), char(rhs))

    end function is_lle_object

    !> Lexically compare two character sequences for being less than or equal.
    elemental function is_lle_string(lhs, rhs) result(is_lle)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(char(lhs), to_char(rhs))

    end function is_lle_string

    !> Lexically compare two character sequences for being less than or equal.
    elemental function is_lle_char(lhs, rhs) result(is_lle)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(char(lhs), rhs)

    end function is_lle_char



    !> Compare two character sequences for being greater.
    elemental function gt_object_object(lhs, rhs) result(is_gt)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_gt

        is_gt = char(lhs) > char(rhs)

    end function gt_object_object

    !> Compare two character sequences for being greater.
    elemental function gt_object_char(lhs, rhs) result(is_gt)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_gt

        is_gt = char(lhs) > rhs

    end function gt_object_char

    !> Compare two character sequences for being greater.
    elemental function gt_object_string(lhs, rhs) result(is_gt)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_gt

        is_gt = char(lhs) > to_char(rhs)

    end function gt_object_string

    !> Compare two character sequences for being greater.
    elemental function gt_string_object(lhs, rhs) result(is_gt)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_gt

        is_gt = to_char(lhs) > char(rhs)

    end function gt_string_object

    !> Compare two character sequences for being greater.
    elemental function gt_char_object(lhs, rhs) result(is_gt)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_gt

        is_gt = lhs > char(rhs)

    end function gt_char_object


    !> Compare two character sequences for being less.
    elemental function lt_object_object(lhs, rhs) result(is_lt)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_object_object

    !> Compare two character sequences for being less.
    elemental function lt_object_char(lhs, rhs) result(is_lt)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_object_char

    !> Compare two character sequences for being less.
    elemental function lt_object_string(lhs, rhs) result(is_lt)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_object_string

    !> Compare two character sequences for being less.
    elemental function lt_string_object(lhs, rhs) result(is_lt)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_string_object

    !> Compare two character sequences for being less.
    elemental function lt_char_object(lhs, rhs) result(is_lt)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lt

        is_lt = rhs > lhs

    end function lt_char_object


    !> Compare two character sequences for being greater or equal.
    elemental function ge_object_object(lhs, rhs) result(is_ge)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_object_object

    !> Compare two character sequences for being greater or equal.
    elemental function ge_object_char(lhs, rhs) result(is_ge)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_object_char

    !> Compare two character sequences for being greater or equal.
    elemental function ge_object_string(lhs, rhs) result(is_ge)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_object_string

    !> Compare two character sequences for being greater or equal
    elemental function ge_string_object(lhs, rhs) result(is_ge)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_string_object

    !> Compare two character sequences for being greater or equal
    elemental function ge_char_object(lhs, rhs) result(is_ge)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_ge

        is_ge = .not. (rhs > lhs)

    end function ge_char_object


    !> Compare two character sequences for being less or equal.
    elemental function le_object_object(lhs, rhs) result(is_le)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_object_object

    !> Compare two character sequences for being less or equal.
    elemental function le_object_char(lhs, rhs) result(is_le)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_object_char

    !> Compare two character sequences for being less or equal.
    elemental function le_object_string(lhs, rhs) result(is_le)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_object_string

    !> Compare two character sequences for being less or equal
    elemental function le_string_object(lhs, rhs) result(is_le)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_string_object

    !> Compare two character sequences for being less or equal
    elemental function le_char_object(lhs, rhs) result(is_le)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_le

        is_le = .not. (lhs > rhs)

    end function le_char_object


    !> Compare two character sequences for equality.
    elemental function eq_object_object(lhs, rhs) result(is_eq)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_object_object

    !> Compare two character sequences for equality.
    elemental function eq_object_char(lhs, rhs) result(is_eq)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_object_char

    !> Compare two character sequences for equality.
    elemental function eq_object_string(lhs, rhs) result(is_eq)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_object_string

    !> Compare two character sequences for equality.
    elemental function eq_string_object(lhs, rhs) result(is_eq)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_string_object

    !> Compare two character sequences for equality.
    elemental function eq_char_object(lhs, rhs) result(is_eq)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_eq

        is_eq = .not.(lhs > rhs)
        if (is_eq) then
            is_eq = .not.(rhs > lhs)
        end if

    end function eq_char_object


    !> Compare two character sequences for inequality.
    elemental function ne_object_object(lhs, rhs) result(is_ne)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_object_object

    !> Compare two character sequences for inequality.
    elemental function ne_object_char(lhs, rhs) result(is_ne)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_object_char

    !> Compare two character sequences for inequality.
    elemental function ne_object_string(lhs, rhs) result(is_ne)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_object_string

    !> Compare two character sequences for inequality.
    elemental function ne_string_object(lhs, rhs) result(is_ne)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_string_object

    !> Compare two character sequences for inequality.
    elemental function ne_char_object(lhs, rhs) result(is_ne)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_ne

        is_ne = lhs > rhs
        if (.not.is_ne) then
            is_ne = rhs > lhs
        end if

    end function ne_char_object


    !> Compare two character sequences for being greater.
    elemental function concat_object_object(lhs, rhs) result(string)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        type(string_type) :: string

        string = char(lhs) // char(rhs)

    end function concat_object_object

    !> Compare two character sequences for being greater.
    elemental function concat_object_char(lhs, rhs) result(string)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        type(string_type) :: string

        string = char(lhs) // rhs

    end function concat_object_char

    !> Compare two character sequences for being greater.
    elemental function concat_object_string(lhs, rhs) result(string)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        type(string_type) :: string

        string = char(lhs) // to_char(rhs)

    end function concat_object_string

    !> Compare two character sequences for being greater.
    elemental function concat_string_object(lhs, rhs) result(string)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        type(string_type) :: string

        string = to_char(lhs) // char(rhs)

    end function concat_string_object

    !> Compare two character sequences for being greater.
    elemental function concat_char_object(lhs, rhs) result(string)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        type(string_type) :: string

        string = lhs // char(rhs)

    end function concat_char_object


    !> Returns the length of the character sequence represented by the string.
    elemental function len_object(string) result(length)
        class(string_class), intent(in) :: string
        integer :: length
        length = string%get_len()
    end function len_object

    !> Returns the length of the character sequence without trailing spaces
    !> represented by the string.
    elemental function len_trim_object(string) result(length)
        class(string_class), intent(in) :: string
        integer :: length
        length = string%get_len_trim()
    end function len_trim_object

    !> Character-to-integer conversion function.
    elemental function ichar_object(string) result(ich)
        class(string_class), intent(in) :: string
        integer :: ich
        ich = string%get_ichar()
    end function ichar_object

    !> Code in ASCII collating sequence.
    elemental function iachar_object(string) result(ich)
        class(string_class), intent(in) :: string
        integer :: ich
        ich = string%get_iachar()
    end function iachar_object

    !> Return the character sequence represented by the string.
    pure function char_object(string) result(character_string)
        class(string_class), intent(in) :: string
        character(len=:), allocatable :: character_string
        character_string = string%get_char()
    end function char_object

    !> Return the character sequence represented by the string.
    elemental function char_object_pos(string, pos) result(character_string)
        class(string_class), intent(in) :: string
        integer, intent(in) :: pos
        character(len=1) :: character_string
        character_string = string%get_char_pos(pos)
    end function char_object_pos

    !> Return the character sequence represented by the string.
    pure function char_object_range(string, start, last) result(character_string)
        class(string_class), intent(in) :: string
        integer, intent(in) :: start
        integer, intent(in) :: last
        character(len=last-start+1) :: character_string
        character_string = string%get_char_range(start, last)
    end function char_object_range

    !> Returns the character sequence hold by the string without trailing spaces.
    elemental function trim_object(string) result(trimmed_string)
        class(string_class), intent(in) :: string
        type(string_type) :: trimmed_string
        trimmed_string = string%get_trim()
    end function trim_object


    !> Left-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    elemental function adjustl_object(string) result(adjusted_string)
        class(string_class), intent(in) :: string
        type(string_type) :: adjusted_string
        adjusted_string = string%get_adjustl()
    end function adjustl_object


    !> Right-adjust the character sequence represented by the string.
    !> The length of the character sequence remains unchanged.
    elemental function adjustr_object(string) result(adjusted_string)
        class(string_class), intent(in) :: string
        type(string_type) :: adjusted_string
        adjusted_string = string%get_adjustr()
    end function adjustr_object


    !> Repeats the character sequence hold by the string by the number of
    !> specified copies.
    elemental function repeat_object(string, ncopies) result(repeated_string)
        class(string_class), intent(in) :: string
        integer, intent(in) :: ncopies
        type(string_type) :: repeated_string
        repeated_string = string%get_repeat(ncopies)
    end function repeat_object


    !> Position of a sequence of character within a character sequence.
    elemental function index_object_object(string, substring, back) result(pos)
        class(string_class), intent(in) :: string
        class(string_class), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_index(substring, back)
    end function index_object_object

    !> Position of a sequence of character within a character sequence.
    elemental function index_object_string(string, substring, back) result(pos)
        class(string_class), intent(in) :: string
        type(string_type), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_index(substring, back)
    end function index_object_string

    !> Position of a sequence of character within a character sequence.
    elemental function index_object_char(string, substring, back) result(pos)
        class(string_class), intent(in) :: string
        character(len=*), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_index(substring, back)
    end function index_object_char

    !> Position of a sequence of character within a character sequence.
    elemental function index_string_object(string, substring, back) result(pos)
        type(string_type), intent(in) :: string
        class(string_class), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos
        pos = index(to_char(string), char(substring), &
            merge(back, .false., present(back)))
    end function index_string_object

    !> Position of a sequence of character within a character sequence.
    elemental function index_char_object(string, substring, back) result(pos)
        character(len=*), intent(in) :: string
        class(string_class), intent(in) :: substring
        logical, intent(in), optional :: back
        integer :: pos
        pos = index(string, char(substring), &
            merge(back, .false., present(back)))
    end function index_char_object


    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function scan_object_object(string, set, back) result(pos)
        class(string_class), intent(in) :: string
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_scan(set, back)
    end function scan_object_object

    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function scan_object_string(string, set, back) result(pos)
        class(string_class), intent(in) :: string
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_scan(set, back)
    end function scan_object_string

    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function scan_object_char(string, set, back) result(pos)
        class(string_class), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_scan(set, back)
    end function scan_object_char

    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function scan_string_object(string, set, back) result(pos)
        type(string_type), intent(in) :: string
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = scan(to_char(string), char(set), &
            merge(back, .false., present(back)))
    end function scan_string_object

    !> Scan a character sequence for any of the characters in a set of characters.
    elemental function scan_char_object(string, set, back) result(pos)
        character(len=*), intent(in) :: string
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = scan(string, char(set), &
            merge(back, .false., present(back)))
    end function scan_char_object


    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function verify_object_object(string, set, back) result(pos)
        class(string_class), intent(in) :: string
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_verify(set, back)
    end function verify_object_object

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function verify_object_string(string, set, back) result(pos)
        class(string_class), intent(in) :: string
        type(string_type), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_verify(set, back)
    end function verify_object_string

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function verify_object_char(string, set, back) result(pos)
        class(string_class), intent(in) :: string
        character(len=*), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = string%get_verify(set, back)
    end function verify_object_char

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function verify_string_object(string, set, back) result(pos)
        type(string_type), intent(in) :: string
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = verify(to_char(string), char(set), &
            merge(back, .false., present(back)))
    end function verify_string_object

    !> Verify a character sequence for the absence any of the characters in
    !> a set of characters.
    elemental function verify_char_object(string, set, back) result(pos)
        character(len=*), intent(in) :: string
        class(string_class), intent(in) :: set
        logical, intent(in), optional :: back
        integer :: pos
        pos = verify(string, char(set), &
            merge(back, .false., present(back)))
    end function verify_char_object


    !> Lexically compare two character sequences for being greater.
    elemental function lgt_object_object(lhs, rhs) result(is_lgt)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lhs%is_lgt(rhs)

    end function lgt_object_object

    !> Lexically compare two character sequences for being greater.
    elemental function lgt_object_string(lhs, rhs) result(is_lgt)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lhs%is_lgt(rhs)

    end function lgt_object_string

    !> Lexically compare two character sequences for being greater.
    elemental function lgt_string_object(lhs, rhs) result(is_lgt)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(to_char(lhs), char(rhs))

    end function lgt_string_object

    !> Lexically compare two character sequences for being greater.
    elemental function lgt_object_char(lhs, rhs) result(is_lgt)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lhs%is_lgt(rhs)

    end function lgt_object_char

    !> Lexically compare two character sequences for being greater.
    elemental function lgt_char_object(lhs, rhs) result(is_lgt)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lgt

        is_lgt = lgt(lhs, char(rhs))

    end function lgt_char_object

    !> Lexically compare two character sequences for being less.
    elemental function llt_object_object(lhs, rhs) result(is_llt)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_llt

        is_llt = lhs%is_llt(rhs)

    end function llt_object_object

    !> Lexically compare two character sequences for being less.
    elemental function llt_object_string(lhs, rhs) result(is_llt)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_llt

        is_llt = lhs%is_llt(rhs)

    end function llt_object_string

    !> Lexically compare two character sequences for being less.
    elemental function llt_string_object(lhs, rhs) result(is_llt)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(to_char(lhs), char(rhs))

    end function llt_string_object

    !> Lexically compare two character sequences for being less.
    elemental function llt_object_char(lhs, rhs) result(is_llt)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_llt

        is_llt = lhs%is_llt(rhs)

    end function llt_object_char

    !> Lexically compare two character sequences for being less.
    elemental function llt_char_object(lhs, rhs) result(is_llt)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_llt

        is_llt = llt(lhs, char(rhs))

    end function llt_char_object

    !> Lexically compare two character sequences for being greater or equal.
    elemental function lge_object_object(lhs, rhs) result(is_lge)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lge

        is_lge = lhs%is_lge(rhs)

    end function lge_object_object

    !> Lexically compare two character sequences for being greater or equal.
    elemental function lge_object_string(lhs, rhs) result(is_lge)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lge

        is_lge = lhs%is_lge(rhs)

    end function lge_object_string

    !> Lexically compare two character sequences for being greater or equal.
    elemental function lge_string_object(lhs, rhs) result(is_lge)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(to_char(lhs), char(rhs))

    end function lge_string_object

    !> Lexically compare two character sequences for being greater or equal.
    elemental function lge_object_char(lhs, rhs) result(is_lge)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lge

        is_lge = lhs%is_lge(rhs)

    end function lge_object_char

    !> Lexically compare two character sequences for being greater or equal.
    elemental function lge_char_object(lhs, rhs) result(is_lge)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lge

        is_lge = lge(lhs, char(rhs))

    end function lge_char_object

    !> Lexically compare two character sequences for being less or equal.
    elemental function lle_object_object(lhs, rhs) result(is_lle)
        class(string_class), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lle

        is_lle = lhs%is_lle(rhs)

    end function lle_object_object

    !> Lexically compare two character sequences for being less or equal.
    elemental function lle_object_string(lhs, rhs) result(is_lle)
        class(string_class), intent(in) :: lhs
        type(string_type), intent(in) :: rhs
        logical :: is_lle

        is_lle = lhs%is_lle(rhs)

    end function lle_object_string

    !> Lexically compare two character sequences for being less or equal
    elemental function lle_string_object(lhs, rhs) result(is_lle)
        type(string_type), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(to_char(lhs), char(rhs))

    end function lle_string_object

    !> Lexically compare two character sequences for being less or equal.
    elemental function lle_object_char(lhs, rhs) result(is_lle)
        class(string_class), intent(in) :: lhs
        character(len=*), intent(in) :: rhs
        logical :: is_lle

        is_lle = lhs%is_lle(rhs)

    end function lle_object_char

    !> Lexically compare two character sequences for being less or equal
    elemental function lle_char_object(lhs, rhs) result(is_lle)
        character(len=*), intent(in) :: lhs
        class(string_class), intent(in) :: rhs
        logical :: is_lle

        is_lle = lle(lhs, char(rhs))

    end function lle_char_object


    !> Write the character sequence hold by the string to a connected unformatted
    !> unit.
    subroutine write_unformatted(self, unit, iostat, iomsg)
        class(string_class), intent(in) :: self
        integer, intent(in) :: unit
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        type(string_type) :: string
        string = char(self)
        write(unit, iostat=iostat, iomsg=iomsg) string
    end subroutine write_unformatted

    !> Write the character sequence hold by the string to a connected formatted
    !> unit.
    subroutine write_formatted(self, unit, iotype, v_list, iostat, iomsg)
        class(string_class), intent(in) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        type(string_type) :: string
        call unused_dummy_argument(iotype)
        call unused_dummy_argument(v_list)
        string = char(self)
        write(unit, *, iostat=iostat, iomsg=iomsg) string
    end subroutine write_formatted


    !> Read a character sequence from a connected unformatted unit into the string.
    subroutine read_unformatted(self, unit, iostat, iomsg)
        class(string_class), intent(inout) :: self
        integer, intent(in)    :: unit
        integer, intent(out)   :: iostat
        character(len=*), intent(inout) :: iomsg
        type(string_type) :: string
        read(unit, iostat=iostat, iomsg=iomsg) string
        self = to_char(string)
    end subroutine read_unformatted

    !> Read a character sequence from a connected formatted unit into the string.
    subroutine read_formatted(self, unit, iotype, v_list, iostat, iomsg)
        class(string_class), intent(inout) :: self
        integer, intent(in) :: unit
        character(len=*), intent(in) :: iotype
        integer, intent(in) :: v_list(:)
        integer, intent(out) :: iostat
        character(len=*), intent(inout) :: iomsg
        type(string_type) :: string
        call unused_dummy_argument(iotype)
        call unused_dummy_argument(v_list)
        read(unit, *, iostat=iostat, iomsg=iomsg) string
        self = to_char(string)
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

end module stdlib_string_class
