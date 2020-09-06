module stdlib_error
    !! Provides support for catching and handling errors
    !! ([Specification](../page/specs/stdlib_error.html))
use, intrinsic :: iso_fortran_env, only: stderr => error_unit
use stdlib_optval, only: optval
implicit none
private

interface ! f{08,18}estop.f90
    module subroutine error_stop(msg, code)
        !! version: experimental
        !!
        !! Provides a call to `error stop` and allows the user to specify a code and message
        !! ([Specification](..//page/specs/stdlib_error.html#description_1))
        character(*), intent(in) :: msg
        integer, intent(in), optional :: code
    end subroutine error_stop
end interface

public :: check, error_stop

contains

subroutine check(condition, msg, code, warn)
    !! version: experimental
    !!
    !! Checks the value of a logical condition
    !! ([Specification](../page/specs/stdlib_error.html#description))
    !!
    !!##### Behavior
    !!
    !! If `condition == .false.` and:
    !!
    !!   * No other arguments are provided, it stops the program with the default
    !!     message and exit code `1`;
    !!   * `msg` is provided, it prints the value of `msg`;
    !!   * `code` is provided, it stops the program with the given exit code;
    !!   * `warn` is provided and `.true.`, it doesn't stop the program and prints
    !!     the message.
    !!
    !!##### Examples
    !!
    !!* If `a /= 5`, stops the program with exit code `1`
    !!  and prints `Check failed.`
    !!``` fortran
    !!  call check(a == 5)
    !!```
    !!
    !!* As above, but prints `a == 5 failed`.
    !!``` fortran
    !!  call check(a == 5, msg='a == 5 failed.')
    !!```
    !!
    !!* As above, but doesn't stop the program.
    !!``` fortran
    !!  call check(a == 5, msg='a == 5 failed.', warn=.true.)
    !!```
    !!
    !!* As example #2, but stops the program with exit code `77`
    !!``` fortran
    !!  call check(a == 5, msg='a == 5 failed.', code=77)
    !!```

    !
    ! Arguments
    ! ---------

    logical, intent(in) :: condition
    character(*), intent(in), optional :: msg
    integer, intent(in), optional :: code
    logical, intent(in), optional :: warn
    character(*), parameter :: msg_default = 'Check failed.'

    if (.not. condition) then
        if (optval(warn, .false.)) then
            write(stderr,*) optval(msg, msg_default)
        else
            call error_stop(optval(msg, msg_default), optval(code, 1))
        end if
    end if

end subroutine check

end module stdlib_error
