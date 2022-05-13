! SPDX-Identifier: MIT

!> Terminal color and style escape sequences
module stdlib_terminal_colors
    use stdlib_kinds, only : i1 => int8
    implicit none
    private

    public :: ansi_color
    public :: style_reset, style_bold, style_dim, style_italic, style_underline, &
        & style_blink, style_blink_fast, style_reverse, style_hidden, style_strikethrough
    public :: fg_color_black, fg_color_red, fg_color_green, fg_color_yellow, fg_color_blue, &
        & fg_color_magenta, fg_color_cyan, fg_color_white, fg_color_default
    public :: bg_color_black, bg_color_red, bg_color_green, bg_color_yellow, bg_color_blue, &
        & bg_color_magenta, bg_color_cyan, bg_color_white, bg_color_default

    public :: to_string, operator(+), operator(//)



    !> Container for terminal escape code
    type :: ansi_color
        private
        !> Style descriptor
        integer(i1) :: style = -1_i1
        !> Background color descriptor
        integer(i1) :: bg = -1_i1
        !> Foreground color descriptor
        integer(i1) :: fg = -1_i1
    end type ansi_color


    !> Identifier for reset style
    type(ansi_color), parameter :: style_reset = ansi_color(style=0)
    !> Identifier for bold style
    type(ansi_color), parameter :: style_bold = ansi_color(style=1)
    !> Identifier for dim style
    type(ansi_color), parameter :: style_dim = ansi_color(style=2)
    !> Identifier for italic style
    type(ansi_color), parameter :: style_italic = ansi_color(style=3)
    !> Identifier for underline style
    type(ansi_color), parameter :: style_underline = ansi_color(style=4)
    !> Identifier for blink style
    type(ansi_color), parameter :: style_blink = ansi_color(style=5)
    !> Identifier for (fast) blink style
    type(ansi_color), parameter :: style_blink_fast = ansi_color(style=6)
    !> Identifier for reverse style
    type(ansi_color), parameter :: style_reverse = ansi_color(style=7)
    !> Identifier for hidden style
    type(ansi_color), parameter :: style_hidden = ansi_color(style=8)
    !> Identifier for strikethrough style
    type(ansi_color), parameter :: style_strikethrough = ansi_color(style=9)

    !> Identifier for black foreground color
    type(ansi_color), parameter :: fg_color_black = ansi_color(fg=0)
    !> Identifier for red foreground color
    type(ansi_color), parameter :: fg_color_red = ansi_color(fg=1)
    !> Identifier for green foreground color
    type(ansi_color), parameter :: fg_color_green = ansi_color(fg=2)
    !> Identifier for yellow foreground color
    type(ansi_color), parameter :: fg_color_yellow = ansi_color(fg=3)
    !> Identifier for blue foreground color
    type(ansi_color), parameter :: fg_color_blue = ansi_color(fg=4)
    !> Identifier for magenta foreground color
    type(ansi_color), parameter :: fg_color_magenta = ansi_color(fg=5)
    !> Identifier for cyan foreground color
    type(ansi_color), parameter :: fg_color_cyan = ansi_color(fg=6)
    !> Identifier for white foreground color
    type(ansi_color), parameter :: fg_color_white = ansi_color(fg=7)
    !> Identifier for the default foreground color
    type(ansi_color), parameter :: fg_color_default = ansi_color(fg=9)

    !> Identifier for black background color
    type(ansi_color), parameter :: bg_color_black = ansi_color(bg=0)
    !> Identifier for red background color
    type(ansi_color), parameter :: bg_color_red = ansi_color(bg=1)
    !> Identifier for green background color
    type(ansi_color), parameter :: bg_color_green = ansi_color(bg=2)
    !> Identifier for yellow background color
    type(ansi_color), parameter :: bg_color_yellow = ansi_color(bg=3)
    !> Identifier for blue background color
    type(ansi_color), parameter :: bg_color_blue = ansi_color(bg=4)
    !> Identifier for magenta background color
    type(ansi_color), parameter :: bg_color_magenta = ansi_color(bg=5)
    !> Identifier for cyan background color
    type(ansi_color), parameter :: bg_color_cyan = ansi_color(bg=6)
    !> Identifier for white background color
    type(ansi_color), parameter :: bg_color_white = ansi_color(bg=7)
    !> Identifier for the default background color
    type(ansi_color), parameter :: bg_color_default = ansi_color(bg=9)


    interface to_string
        !> Transform a color code into an actual ANSI escape sequence
        pure module function to_string_ansi_color(code) result(str)
            !> Color code to be used
            type(ansi_color), intent(in) :: code
            !> ANSI escape sequence representing the color code
            character(len=:), allocatable :: str
        end function to_string_ansi_color
    end interface to_string


    interface operator(+)
        !> Add two escape sequences, attributes in the right value override the left value ones.
        pure module function add(lval, rval) result(code)
            !> First escape code
            type(ansi_color), intent(in) :: lval
            !> Second escape code
            type(ansi_color), intent(in) :: rval
            !> Combined escape code
            type(ansi_color) :: code
        end function add
    end interface operator(+)

    interface operator(//)
        !> Concatenate an escape code with a string and turn it into an actual escape sequence
        pure module function concat_left(lval, code) result(str)
            !> String to add the escape code to
            character(len=*), intent(in) :: lval
            !> Escape sequence
            type(ansi_color), intent(in) :: code
            !> Concatenated string
            character(len=:), allocatable :: str
        end function concat_left

        !> Concatenate an escape code with a string and turn it into an actual escape sequence
        pure module function concat_right(code, rval) result(str)
            !> String to add the escape code to
            character(len=*), intent(in) :: rval
            !> Escape sequence
            type(ansi_color), intent(in) :: code
            !> Concatenated string
            character(len=:), allocatable :: str
        end function concat_right
    end interface operator(//)

end module stdlib_terminal_colors
