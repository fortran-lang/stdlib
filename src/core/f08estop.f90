submodule (stdlib_error) f08estop

implicit none

contains

module procedure error_stop
! Aborts the program with nonzero exit code
! this is a fallback for Fortran 2008 error stop (e.g. Intel 19.1/2020 compiler)
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

write(stderr,*) msg

if(present(code)) then
  select case (code)
  case (1)
    error stop 1
  case (2)
    error stop 2
  case (77)
    error stop 77
  case default
    write(stderr,*) 'ERROR: code ',code,' was specified.'
    error stop
  end select
else
  error stop
endif
end procedure

end submodule f08estop
