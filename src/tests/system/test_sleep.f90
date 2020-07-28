program test_sleep
use, intrinsic :: iso_fortran_env, only : int64, real64
use stdlib_system, only : sleep

implicit none

integer :: ierr, millisec
character(8) :: argv
integer(int64) :: tic, toc, trate
real(real64) :: t_ms

call system_clock(count_rate=trate)

millisec = 780
call get_command_argument(1, argv, status=ierr)
if (ierr==0) read(argv,*) millisec

if (millisec<0) millisec=0

call system_clock(count=tic)
call sleep(millisec)
call system_clock(count=toc)

t_ms = (toc-tic) * 1000._real64 / trate

if (millisec > 0) then
  if (t_ms < 0.5 * millisec) error stop 'actual sleep time was too short'
  if (t_ms > 2 * millisec) error stop 'actual sleep time was too long'
endif

print '(A,F8.3)', 'OK: test_sleep: slept for (ms): ',t_ms

end program
