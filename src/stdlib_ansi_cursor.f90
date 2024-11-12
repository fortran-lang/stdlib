module stdlib_ansi_cursor
    use stdlib_strings, only: to_string
    implicit none

    character(len=*), parameter :: esc = achar(27)
    !> moves the cursor to home => `(0,0)`
    character(len=*), parameter :: home = esc//"[H"
    !> erases from the cursor till the end of the screen
    character(len=*), parameter :: clear_till_screen_end = esc//"[OJ"
    !> erases from the cursor to the beginning of the screen
    character(len=*), parameter :: clear_till_screen_start = esc//"[1J"
    !> erases the entire screen
    character(len=*), parameter :: clear_completely = esc//"[2J"
    !> erases from the cursor till the end of line
    character(len=*), parameter :: clear_till_line_end = esc//"[0K"
    !> erases from the cursor till the beginning of the line
    character(len=*), parameter :: clear_till_line_start = esc//"[1K"
    !> erases the entire line
    character(len=*), parameter :: clear_entire_line = esc//"[2K"

contains
    !> moves the cursor to `(line, column)`
    !> returns an empty string if any of them is negative
    pure function move_to(line, col) result(str)
        integer, intent(in) :: line
        integer, intent(in) :: col
        character(:), allocatable :: str

        if (line < 0 .or. col < 0) then
            str = ""
        else
            str = esc//"["//to_string(line)//";"//to_string(col)//"H"
        end if

    end function move_to

    !> moves the cursor to column `col`
    !> returns an empty string if `col` is negative
    pure function move_to_column(col) result(str)
        integer, intent(in) :: col
        character(:), allocatable :: str

        if (col < 0) then
            str = ""
        else
            str = esc//"["//to_string(col)//"G"
        end if

    end function move_to_column

    !> moves the cursor up by `line` lines
    !> returns an empty string if `line` is negative
    pure function move_up(line) result(str)
        integer, intent(in) :: line
        character(:), allocatable :: str

        if (line <= 0) then
            str = ""
        else
            str = esc//"["//to_string(line)//"A"
        end if

    end function move_up

    !> moves the cursor down by `line` lines
    !> returns an empty string if `line` is negative
    pure function move_down(line) result(str)
        integer, intent(in) :: line
        character(:), allocatable :: str

        if (line <= 0) then
            str = ""
        else
            str = esc//"["//to_string(line)//"B"
        end if

    end function move_down

    !> moves the cursor right by `line` lines
    !> returns an empty string if `line` is negative
    pure function move_right(line) result(str)
        integer, intent(in) :: line
        character(:), allocatable :: str

        if (line <= 0) then
            str = ""
        else
            str = esc//"["//to_string(line)//"C"
        end if

    end function move_right

    !> moves the cursor left by `line` lines
    !> returns an empty string if `line` is negative
    pure function move_left(line) result(str)
        integer, intent(in) :: line
        character(:), allocatable :: str

        if (line <= 0) then
            str = ""
        else
            str = esc//"["//to_string(line)//"D"
        end if

    end function move_left

end module stdlib_ansi_cursor
