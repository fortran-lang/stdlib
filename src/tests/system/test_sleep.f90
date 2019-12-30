program test_sleep

use stdlib_experimental_system, only : sleep

implicit none

integer :: ierr, millisec
character(8) :: argv

millisec = 780
call get_command_argument(1, argv, status=ierr)
if (ierr==0) read(argv,*) millisec

if (millisec<0) millisec=0

call sleep(millisec)

end program