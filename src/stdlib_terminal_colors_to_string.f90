! SPDX-Identifier: MIT

!> Implementation of the conversion to enumerator and identifier types to strings
submodule (stdlib_terminal_colors) stdlib_terminal_colors_to_string
    use stdlib_strings, only : to_string_ => to_string
    implicit none


contains


!> Convert the style identifier to a string
pure module function to_string_style(style) result(str)
    !> Identifier of style
    type(style_enum), intent(in) :: style
    !> Converted string
    character(len=:), allocatable :: str

    str = esc // to_string_(style%id) // "m"
end function to_string_style


!> Convert the color identifier to a string
pure module function to_string_fg_color(fg_color) result(str)
    !> Identifier of foreground color
    type(fg_color_enum), intent(in) :: fg_color
    !> Converted string
    character(len=:), allocatable :: str

    str = esc // to_string_(fg_color%id + fg_offset) // "m"
end function to_string_fg_color


!> Convert the background color identifier to a string
pure module function to_string_bg_color(bg_color) result(str)
    !> Identifier of background color
    type(bg_color_enum), intent(in) :: bg_color
    !> Converted string
    character(len=:), allocatable :: str

    str = esc // to_string_(bg_color%id + bg_offset) // "m"
end function to_string_bg_color


!> Convert foreground true color to a string
pure module function to_string_fg_color24(fg_color) result(str)
    !> 24-bit foreground color
    type(fg_color24), intent(in) :: fg_color
    !> Converted string
    character(len=:), allocatable :: str

    str = esc // fg_color24_prefix // &
        & to_string_(abs(fg_color%red)) // ";" // &
        & to_string_(abs(fg_color%green)) // ";" // &
        & to_string_(abs(fg_color%blue)) // "m"
end function to_string_fg_color24


!> Convert background true color to a string
pure module function to_string_bg_color24(bg_color) result(str)
    !> 24-bit background color
    type(bg_color24), intent(in) :: bg_color
    !> Converted string
    character(len=:), allocatable :: str

    str = esc // bg_color24_prefix // &
        & to_string_(abs(bg_color%red)) // ";" // &
        & to_string_(abs(bg_color%green)) // ";" // &
        & to_string_(abs(bg_color%blue)) // "m"
end function to_string_bg_color24


end submodule stdlib_terminal_colors_to_string
