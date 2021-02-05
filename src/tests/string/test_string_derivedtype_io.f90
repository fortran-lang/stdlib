! SPDX-Identifer: MIT
module test_string_derivedtype_io
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), len, &
        write(formatted), read(formatted), write(unformatted), read(unformatted), &
        operator(.eq.)
    implicit none

contains

    subroutine test_listdirected_io
        type(string_type) :: string
        integer :: io, stat
        string = "Important saved value"

        open(newunit=io, form="formatted", status="scratch")
        write(io, *) string
        write(io, *) ! Pad with a newline or we might run into EOF while reading

        string = ""
        rewind(io)

        read(io, *, iostat=stat) string
        close(io)

        call check(stat == 0)
        call check(len(string) == 21)
        call check(string == "Important saved value")
    end subroutine test_listdirected_io

    subroutine test_formatted_io
        type(string_type) :: string
        integer :: io, stat
        string = "Important saved value"

        !open(newunit=io, form="formatted", status="scratch")
        open(newunit=io, form="formatted", file="scratch.txt")
        write(io, '(dt)') string
        write(io, '(a)') ! Pad with a newline or we might run into EOF while reading

        string = ""
        rewind(io)

        read(io, *, iostat=stat) string
        close(io)

        call check(stat == 0)
        call check(len(string) == 21)
        call check(string == "Important saved value")
    end subroutine test_formatted_io

    subroutine test_unformatted_io
        type(string_type) :: string
        integer :: io
        string = "Important saved value"

        open(newunit=io, form="unformatted", status="scratch")
        write(io) string

        string = ""
        rewind(io)

        read(io) string
        close(io)

        call check(len(string) == 21)
        call check(string == "Important saved value")
    end subroutine test_unformatted_io

end module test_string_derivedtype_io

program tester
    use test_string_derivedtype_io
    implicit none

    call test_listdirected_io
    call test_formatted_io
    call test_unformatted_io

end program tester
