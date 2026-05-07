submodule (stdlib_error) f18estop

implicit none

contains

module procedure error_stop
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

if(present(code)) then
  write(stderr,*) msg
  error stop code
else
  error stop msg
endif
end procedure

end submodule f18estop
