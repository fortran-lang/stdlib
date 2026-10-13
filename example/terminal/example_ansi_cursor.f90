program ansi_cursor
    use stdlib_ansi_cursor, only: move_to, clear_completely
    use stdlib_ansi, only: fg_color_blue, to_string
    implicit none

    character(len=1) :: input
    character(len=*), parameter :: delim = "#"

    print *, clear_completely
    print *, to_string(fg_color_blue) ! The box will be blue now

    call draw_box(10, 38, 77, 17, delim)

    ! read *, input                   ! Waiting for input to actually see the box drawn

contains
    !> Draws a box on the terminal of `width` width and `height` height
    !> The topmost left vertex of the box is at `(line,col)`
    subroutine draw_box(line, col, width, height, char)
        integer, intent(in) :: line, col, width, height
        character(len=1), intent(in) :: char
        integer :: i

        do i = 0, width - 1
            write (*, "(a,a)", advance="NO") move_to(line, col + i), char
            write (*, "(a,a)", advance="NO") move_to(line + height - 1, col + i), char
        end do

        do i = 0, height - 1
            write (*, "(a,a)", advance="NO") move_to(line + i, col), char
            write (*, "(a,a)", advance="NO") move_to(line + i, col + width - 1), char
        end do

    end subroutine draw_box

end program ansi_cursor
