module stdlib_error
implicit none
private
public assert, error_stop

contains

subroutine assert(condition)
! If condition == .false., it aborts the program.
!
! Arguments
! ---------
!
logical, intent(in) :: condition
!
! Example
! -------
!
! call assert(a == 5)

if (.not. condition) call error_stop("Assert failed.")
end subroutine

subroutine error_stop(msg)
! Aborts the program with nonzero exit code
!
! The statement "stop msg" will return 0 exit code when compiled using
! gfortran. error_stop() uses the statement "stop 1" which returns an exit code
! 1 and a print statement to print the message.
!
! Example
! -------
!
! call error_stop("Invalid argument")

character(len=*) :: msg ! Message to print on stdout
print *, msg
stop 1
end subroutine

end module
