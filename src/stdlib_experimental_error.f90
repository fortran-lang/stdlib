module stdlib_experimental_error
use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
implicit none
private
public :: assert, error_stop

contains

subroutine assert(condition, code)
! If condition == .false., it aborts the program.
!
! Arguments
! ---------
!
logical, intent(in) :: condition
integer, intent(in), optional :: code
!
! Example
! -------
!
! call assert(a == 5)

if (.not. condition) call error_stop("Assert failed.", code)
end subroutine

subroutine error_stop(msg, code)
! Aborts the program with nonzero exit code
!
! The "stop <character>" statement generally has return code 0.
! To allow non-zero return code termination with character message,
! error_stop() uses the statement "error stop", which by default
! has exit code 1 and prints the message to stderr.
! An optional integer return code "code" may be specified.
!
! Example
! -------
!
! call error_stop("Invalid argument")

character(len=*) :: msg ! Message to print on stderr
integer, intent(in), optional :: code

integer :: returncode

if(present(code)) then
  write(stderr,*) msg
  error stop code
else
  error stop msg
endif
end subroutine

end module
