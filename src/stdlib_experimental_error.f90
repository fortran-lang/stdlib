module stdlib_experimental_error
use, intrinsic :: iso_fortran_env, only: stderr=>error_unit
implicit none
private

interface ! f{08,18}estop.f90
module subroutine error_stop(msg, code)
character(*), intent(in) :: msg
integer, intent(in), optional :: code
end subroutine error_stop
end interface

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

end module
