! SPDX-Identifier: MIT
module test_string_assignment
    use stdlib_error, only : check
    use stdlib_kinds, only : int8, int16, int32, int64, lk, c_bool
    use stdlib_string_type, only : string_type, assignment(=), operator(==), len
    implicit none

contains

    subroutine test_assignment
        type(string_type) :: string

        call check(len(string) == 0)

        string = "Sequence"
        call check(len(string) == 8)
    end subroutine test_assignment

    subroutine test_char_value
       character(len=128) :: flc

       write(flc, '(g0)') -1026191
       call check(string_type(-1026191) == trim(flc))

       write(flc, '(g0)') 124787
       call check(string_type(124787) == trim(flc))

       write(flc, '(g0)') -2_int8
       call check(string_type(-2_int8) == trim(flc))

       write(flc, '(g0)') 5_int8
       call check(string_type(5_int8) == trim(flc))

       write(flc, '(g0)') -72_int16
       call check(string_type(-72_int16) == trim(flc))

       write(flc, '(g0)') -8924889_int32
       call check(string_type(-8924889_int32) == trim(flc))

       write(flc, '(g0)') 2378405_int32
       call check(string_type(2378405_int32) == trim(flc))

       write(flc, '(g0)') 921092378411_int64
       call check(string_type(921092378411_int64) == trim(flc))

       write(flc, '(g0)') -1272835761_int64
       call check(string_type(-1272835761_int64) == trim(flc))

       write(flc, '(g0)') .true.
       call check(string_type(.true.) == trim(flc))

       write(flc, '(g0)') .false.
       call check(string_type(.false.) == trim(flc))

       write(flc, '(g0)') .false._c_bool
       call check(string_type(.false._c_bool) == trim(flc))

       write(flc, '(g0)') .true._lk
       call check(string_type(.true._lk) == trim(flc))
    end subroutine test_char_value

end module test_string_assignment

program tester
    use test_string_assignment
    implicit none

    call test_assignment
    call test_char_value

end program tester

