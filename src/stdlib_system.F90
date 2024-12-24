module stdlib_system
use, intrinsic :: iso_c_binding, only : c_int, c_long, c_null_ptr, c_int64_t
use, intrinsic :: iso_c_binding, only : process_handle => c_ptr, null_process => c_null_ptr 
use stdlib_kinds, only: int64, dp
implicit none
private
public :: sleep

!> Public sub-processing interface
public :: run
public :: process_type
public :: is_completed
public :: is_running
     
! CPU clock ticks storage
integer, parameter, private :: TICKS = int64
integer, parameter, private :: RTICKS = dp

! Interoperable types to the C backend
integer, parameter, public :: process_ID = c_int64_t

! Default flag for the runner process
integer(process_ID), parameter, private :: FORKED_PROCESS = 0_process_ID

! Public type to describe a process
type :: process_type
    
    !> Process ID (if external); 0 if run by the program process
    integer(process_ID)  :: id = FORKED_PROCESS
    type(process_handle) :: handle = null_process
    
    !> Process is completed
    logical :: completed = .false.        
    integer(TICKS) :: start_time = 0
    
    !> Process exit code
    integer :: exit_code = 0
    
    !> Stdin file name
    character(:), allocatable :: stdin_file
    
    !> Standard output
    character(:), allocatable :: stdout_file
    character(:), allocatable :: stdout
    
    !> Error output
    character(:), allocatable :: stderr_file
    character(:), allocatable :: stderr
    
    !> Store time at the last update
    integer(TICKS) :: last_update = 0
    
end type process_type

interface run
    !> Open a new, asynchronous process
    module type(process_type) function process_open(args,wait,stdin,want_stdout,want_stderr) result(process)
        !> The command and arguments
        character(*), intent(in) :: args(:)
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin
        !> Define if the process should be synchronous (wait=.true.), or asynchronous(wait=.false.)
        logical, optional, intent(in) :: wait
        !> Require collecting output
        logical, optional, intent(in) :: want_stdout, want_stderr        
    end function process_open
end interface run

!> Live check if a process is still running
interface is_running
    module logical function process_is_running(process) result(is_running)
        class(process_type), intent(inout) :: process
    end function process_is_running
end interface is_running

!> Live check if a process is still running
interface is_completed
    module logical function process_is_completed(process) result(is_completed)
        class(process_type), intent(inout) :: process
    end function process_is_completed
end interface is_completed

!> Return process lifetime so far, in seconds
interface elapsed
    module real(RTICKS) function process_lifetime(process) result(delta_t)
        class(process_type), intent(in) :: process 
    end function process_lifetime
end interface elapsed

!> Wait until a running process is completed
interface wait
    module subroutine wait_for_completion(process, max_wait_time)
        class(process_type), intent(inout) :: process
        ! Optional max wait time in seconds
        real, optional, intent(in) :: max_wait_time
    end subroutine wait_for_completion
end interface wait

interface
#ifdef _WIN32
subroutine winsleep(dwMilliseconds) bind (C, name='Sleep')
!! version: experimental
!!
!! void Sleep(DWORD dwMilliseconds)
!! https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-sleep
import c_long
integer(c_long), value, intent(in) :: dwMilliseconds
end subroutine winsleep
#else
integer(c_int) function usleep(usec) bind (C)
!! version: experimental
!!
!! int usleep(useconds_t usec);
!! https://linux.die.net/man/3/usleep
import c_int
integer(c_int), value, intent(in) :: usec
end function usleep
#endif
end interface




contains

subroutine sleep(millisec)
!! version: experimental
!!
integer, intent(in) :: millisec
integer(c_int) :: ierr

#ifdef _WIN32
!! PGI Windows, Ifort Windows, ....
call winsleep(int(millisec, c_long))
#else
!! Linux, Unix, MacOS, MSYS2, ...
ierr = usleep(int(millisec * 1000, c_int))
if (ierr/=0) error stop 'problem with usleep() system call'
#endif


end subroutine sleep

end module stdlib_system
