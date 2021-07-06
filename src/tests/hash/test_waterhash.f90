module waterhash_testing
    use stdlib_error, only : check
    use stdlib_hash, only : waterhash
    use stdlib_kinds, only : i1 => int8, i4 => int32, i8 => int64
    use stdlib_strings, only : to_string
    implicit none
contains

    subroutine check_eq(actual, expected)
        integer, intent(in) :: actual
        integer, intent(in) :: expected
        logical :: stat
        character(len=:), allocatable :: message
        stat = actual == expected

        if (.not.stat) then
            message = "Expected '"//to_string(expected)//"' but got '"//to_string(actual)//"'"
        end if
        call check(stat, msg=message)
    end subroutine check_eq

    subroutine test_waterhash
        integer(i8), parameter :: seed = 433494437_i8
        character(len=*), parameter :: string = "hashable-test-string"

        call check_eq(waterhash("waterhash",  seed),  1441494093)
        call check_eq(waterhash(string(1: 1), seed), -1365789813)
        call check_eq(waterhash(string(1: 2), seed),   -54733720)
        call check_eq(waterhash(string(1: 3), seed), -1376896642)
        call check_eq(waterhash(string(1: 4), seed),  2049690556)
        call check_eq(waterhash(string(1: 5), seed),  1902039650)
        call check_eq(waterhash(string(1: 6), seed),  -125035208)
        call check_eq(waterhash(string(1: 7), seed), -2091807725)
        call check_eq(waterhash(string(1: 8), seed),  1300010631)
        call check_eq(waterhash(string(1: 9), seed),   -65883164)
        call check_eq(waterhash(string(1:10), seed),  -846809663)
        call check_eq(waterhash(string(1:11), seed),  -684476212)
        call check_eq(waterhash(string(1:12), seed),  1042663538)
        call check_eq(waterhash(string(1:13), seed),  -801644260)
        call check_eq(waterhash(string(1:14), seed),   657065214)
        call check_eq(waterhash(string(1:15), seed), -1222082533)
        call check_eq(waterhash(string(1:16), seed),    28708502)
        call check_eq(waterhash(string(1:17), seed),   908144547)
        call check_eq(waterhash(string(1:18), seed), -1615470564)
        call check_eq(waterhash(string(1:19), seed),  -719426041)
        call check_eq(waterhash(string(1:20), seed), -1564862690)
     end subroutine test_waterhash
end module waterhash_testing

program test_driver
    use waterhash_testing
    implicit none

    call test_waterhash
end program test_driver
