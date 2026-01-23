! SPDX-Identifier: MIT

!> Implementation of the conversion to enumerator and identifier types to strings
submodule (stdlib_ansi) stdlib_ansi_to_string
    implicit none

    character, parameter :: esc = achar(27), chars(0:9) = &
        ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

contains

    !> Transform a color code into an actual ANSI escape sequence
    pure module function to_string_ansi_code(code) result(str)
        !> Color code to be used
        type(ansi_code), intent(in) :: code
        !> ANSI escape sequence representing the color code
        character(len=:), allocatable :: str

        if (anycolor(code)) then
            str = esc // "[0"  ! Always reset the style
            if (code%style > 0 .and. code%style < 10) str = str // ";" // chars(code%style)
            if (code%fg >= 0 .and. code%fg < 10) str = str // ";3" // chars(code%fg)
            if (code%bg >= 0 .and. code%bg < 10) str = str // ";4" // chars(code%bg)
            str = str // "m"
        else
            str = ""
        end if
    end function to_string_ansi_code

    !> Check whether the code describes any color / style or is just a stub
    pure function anycolor(code)
        !> Escape sequence
        type(ansi_code), intent(in) :: code
        !> Any color / style is active
        logical :: anycolor

        anycolor = code%fg >= 0 .or. code%bg >= 0 .or. code%style >= 0
    end function anycolor

end submodule stdlib_ansi_to_string
