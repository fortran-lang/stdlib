! SPDX-Identifier: MIT
module test_string_assignment
    use stdlib_error, only : check
    use stdlib_string_type, only : string_type, assignment(=), len
    implicit none

contains

    subroutine test_assignment
        type(string_type) :: string

        call check(len(string) == 0)

        string = "Sequence"
        call check(len(string) == 8)
    end subroutine test_assignment

end module test_string_assignment

program tester
    use test_string_assignment
    implicit none

    call test_assignment

end program tester

