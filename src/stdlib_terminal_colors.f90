! SPDX-Identifier: MIT

!> Terminal color and style escape sequences
module stdlib_terminal_colors
    implicit none
    private

    public :: fg_color24, bg_color24

    public :: style_reset, style_bold, style_dim, style_italic, style_underline, &
        & style_blink, style_blink_fast, style_reverse, style_hidden, style_strikethrough
    public :: fg_color_black, fg_color_red, fg_color_green, fg_color_yellow, fg_color_blue, &
        & fg_color_magenta, fg_color_cyan, fg_color_white, fg_color_default
    public :: bg_color_black, bg_color_red, bg_color_green, bg_color_yellow, bg_color_blue, &
        & bg_color_magenta, bg_color_cyan, bg_color_white, bg_color_default

    public :: to_string


    !> True color (24-bit) for foreground color
    type :: fg_color24
        !> Red color component
        integer :: red
        !> Green color component
        integer :: green
        !> Blue color component
        integer :: blue
    end type fg_color24


    !> True color (24-bit) for background color
    type :: bg_color24
        !> Red color component
        integer :: red
        !> Green color component
        integer :: green
        !> Blue color component
        integer :: blue
    end type bg_color24


    !> Enumerator for the terminal style
    type :: style_enum
        private
        !> Unique identifier for the style
        integer :: id
    end type style_enum


    !> Enumerator for the terminal foreground color
    type :: fg_color_enum
        private
        !> Unique identifier for the foreground color
        integer :: id
    end type fg_color_enum


    !> Enumerator for the terminal background color
    type :: bg_color_enum
        private
        !> Unique identifier for the background color
        integer :: id
    end type bg_color_enum


    !> Identifier for reset style
    type(style_enum), parameter :: style_reset = style_enum(0)
    !> Identifier for bold style
    type(style_enum), parameter :: style_bold = style_enum(1)
    !> Identifier for dim style
    type(style_enum), parameter :: style_dim = style_enum(2)
    !> Identifier for italic style
    type(style_enum), parameter :: style_italic = style_enum(3)
    !> Identifier for underline style
    type(style_enum), parameter :: style_underline = style_enum(4)
    !> Identifier for blink style
    type(style_enum), parameter :: style_blink = style_enum(5)
    !> Identifier for (fast) blink style
    type(style_enum), parameter :: style_blink_fast = style_enum(6)
    !> Identifier for reverse style
    type(style_enum), parameter :: style_reverse = style_enum(7)
    !> Identifier for hidden style
    type(style_enum), parameter :: style_hidden = style_enum(8)
    !> Identifier for strikethrough style
    type(style_enum), parameter :: style_strikethrough = style_enum(9)

    !> Identifier for black foreground color
    type(fg_color_enum), parameter :: fg_color_black = fg_color_enum(0)
    !> Identifier for red foreground color
    type(fg_color_enum), parameter :: fg_color_red = fg_color_enum(1)
    !> Identifier for green foreground color
    type(fg_color_enum), parameter :: fg_color_green = fg_color_enum(2)
    !> Identifier for yellow foreground color
    type(fg_color_enum), parameter :: fg_color_yellow = fg_color_enum(3)
    !> Identifier for blue foreground color
    type(fg_color_enum), parameter :: fg_color_blue = fg_color_enum(4)
    !> Identifier for magenta foreground color
    type(fg_color_enum), parameter :: fg_color_magenta = fg_color_enum(5)
    !> Identifier for cyan foreground color
    type(fg_color_enum), parameter :: fg_color_cyan = fg_color_enum(6)
    !> Identifier for white foreground color
    type(fg_color_enum), parameter :: fg_color_white = fg_color_enum(7)
    !> Identifier for the default foreground color
    type(fg_color_enum), parameter :: fg_color_default = fg_color_enum(9)

    !> Offset for foreground color
    integer, parameter :: fg_offset = 30
    !> Prefix for foreground true color
    character(len=*), parameter :: fg_color24_prefix = "38;2;"

    !> Identifier for black background color
    type(bg_color_enum), parameter :: bg_color_black = bg_color_enum(0)
    !> Identifier for red background color
    type(bg_color_enum), parameter :: bg_color_red = bg_color_enum(1)
    !> Identifier for green background color
    type(bg_color_enum), parameter :: bg_color_green = bg_color_enum(2)
    !> Identifier for yellow background color
    type(bg_color_enum), parameter :: bg_color_yellow = bg_color_enum(3)
    !> Identifier for blue background color
    type(bg_color_enum), parameter :: bg_color_blue = bg_color_enum(4)
    !> Identifier for magenta background color
    type(bg_color_enum), parameter :: bg_color_magenta = bg_color_enum(5)
    !> Identifier for cyan background color
    type(bg_color_enum), parameter :: bg_color_cyan = bg_color_enum(6)
    !> Identifier for white background color
    type(bg_color_enum), parameter :: bg_color_white = bg_color_enum(7)
    !> Identifier for the default background color
    type(bg_color_enum), parameter :: bg_color_default = bg_color_enum(9)

    !> Offset for background color
    integer, parameter :: bg_offset = 40
    !> Prefix for background true color
    character(len=*), parameter :: bg_color24_prefix = "48;2;"

    !> Escape sequence for terminal style and color
    character(len=*), parameter :: esc = achar(27) // "["


    interface to_string
        !> Convert the style identifier to a string
        pure module function to_string_style(style) result(str)
            !> Identifier of style
            type(style_enum), intent(in) :: style
            !> Converted string
            character(len=:), allocatable :: str
        end function to_string_style

        !> Convert the color identifier to a string
        pure module function to_string_fg_color(fg_color) result(str)
            !> Identifier of foreground color
            type(fg_color_enum), intent(in) :: fg_color
            !> Converted string
            character(len=:), allocatable :: str
        end function to_string_fg_color

        !> Convert the background color identifier to a string
        pure module function to_string_bg_color(bg_color) result(str)
            !> Identifier of background color
            type(bg_color_enum), intent(in) :: bg_color
            !> Converted string
            character(len=:), allocatable :: str
        end function to_string_bg_color

        !> Convert foreground true color to a string
        pure module function to_string_fg_color24(fg_color) result(str)
            !> 24-bit foreground color
            type(fg_color24), intent(in) :: fg_color
            !> Converted string
            character(len=:), allocatable :: str
        end function to_string_fg_color24

        !> Convert background true color to a string
        pure module function to_string_bg_color24(bg_color) result(str)
            !> 24-bit background color
            type(bg_color24), intent(in) :: bg_color
            !> Converted string
            character(len=:), allocatable :: str
        end function to_string_bg_color24
    end interface to_string


end module stdlib_terminal_colors
