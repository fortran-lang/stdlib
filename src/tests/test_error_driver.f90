program test_driver
!! tests return codes from programs.
!! useful for checking "error stop" return codes.
!!
!! Arguments:
!!  prog: program to run and catch return code

implicit none

integer :: ierr, ok_code, cmderr
character(1024) :: prog
character(3) :: ok, actual

call get_command_argument(1, prog, status=ierr)
if(ierr/=0) error stop "please specify a program to catch return codes from"

call get_command_argument(2, ok, status=ierr)
if(ierr/=0) error stop "please specify the expected return code"

read(ok, '(i3)') ok_code

call execute_command_line(trim(prog), exitstat=ierr, cmdstat=cmderr)
if(cmderr/=0) error stop "test_driver had problem running always-fail program"

write(actual, '(i3)') ierr
if(ierr /= ok_code) error stop "expected return code "//ok//", got "//actual//" instead"

end program
