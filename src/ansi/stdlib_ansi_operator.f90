! SPDX-Identifier: MIT

!> Implementation of the conversion to enumerator and identifier types to strings
submodule (stdlib_ansi) stdlib_ansi_operator
    use stdlib_string_type, only : operator(//)
    implicit none

contains

    !> Add two escape sequences, attributes in the right value override the left value ones.
    pure module function add(lval, rval) result(code)
        !> First escape code
        type(ansi_code), intent(in) :: lval
        !> Second escape code
        type(ansi_code), intent(in) :: rval
        !> Combined escape code
        type(ansi_code) :: code

        code%style = merge(rval%style, lval%style, rval%style >= 0)
        code%fg = merge(rval%fg, lval%fg, rval%fg >= 0)
        code%bg = merge(rval%bg, lval%bg, rval%bg >= 0)
    end function add

    !> Concatenate an escape code with a string and turn it into an actual escape sequence
    pure module function concat_left(lval, code) result(str)
        !> String to add the escape code to
        character(len=*), intent(in) :: lval
        !> Escape sequence
        type(ansi_code), intent(in) :: code
        !> Concatenated string
        character(len=:), allocatable :: str

        str = lval // to_string(code)
    end function concat_left

    !> Concatenate an escape code with a string and turn it into an actual escape sequence
    pure module function concat_right(code, rval) result(str)
        !> String to add the escape code to
        character(len=*), intent(in) :: rval
        !> Escape sequence
        type(ansi_code), intent(in) :: code
        !> Concatenated string
        character(len=:), allocatable :: str

        str = to_string(code) // rval
    end function concat_right

    !> Concatenate an escape code with a string and turn it into an actual escape sequence
    pure module function concat_left_str(lval, code) result(str)
        !> String to add the escape code to
        type(string_type), intent(in) :: lval
        !> Escape sequence
        type(ansi_code), intent(in) :: code
        !> Concatenated string
        type(string_type) :: str

        str = lval // to_string(code)
    end function concat_left_str

    !> Concatenate an escape code with a string and turn it into an actual escape sequence
    pure module function concat_right_str(code, rval) result(str)
        !> String to add the escape code to
        type(string_type), intent(in) :: rval
        !> Escape sequence
        type(ansi_code), intent(in) :: code
        !> Concatenated string
        type(string_type) :: str

        str = to_string(code) // rval
    end function concat_right_str

end submodule stdlib_ansi_operator
