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
public :: update
public :: wait
public :: kill
public :: elapsed
public :: has_win32
     
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
    
    !> Stdin file name
    character(:), allocatable :: stdin_file
    
    !> Standard output
    character(:), allocatable :: stdout_file
    character(:), allocatable :: stdout
    
    !> Error output
    integer :: exit_code = 0    
    character(:), allocatable :: stderr_file
    character(:), allocatable :: stderr
    
    !> Store time at the last update
    integer(TICKS) :: last_update = 0
    
end type process_type

interface run
    !> Open a new process from a command line
    module type(process_type) function process_open_cmd(cmd,wait,stdin,want_stdout,want_stderr) result(process)
        !> The command and arguments
        character(*), intent(in) :: cmd
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin
        !> Define if the process should be synchronous (wait=.true.), or asynchronous(wait=.false.)
        logical, optional, intent(in) :: wait
        !> Require collecting output
        logical, optional, intent(in) :: want_stdout, want_stderr        
    end function process_open_cmd
    !> Open a new, asynchronous process from a list of arguments
    module type(process_type) function process_open_args(args,wait,stdin,want_stdout,want_stderr) result(process)
        !> The command and arguments
        character(*), intent(in) :: args(:)
        !> Optional character input to be sent to the process via pipe
        character(*), optional, intent(in) :: stdin
        !> Define if the process should be synchronous (wait=.true.), or asynchronous(wait=.false.)
        logical, optional, intent(in) :: wait
        !> Require collecting output
        logical, optional, intent(in) :: want_stdout, want_stderr        
    end function process_open_args
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

!> Query the system to update a process's state 
interface update
    module subroutine update_process_state(process)
        type(process_type), intent(inout) :: process
    end subroutine update_process_state
end interface update

! Kill a process 
interface kill
    module subroutine process_kill(process, success)
        type(process_type), intent(inout) :: process
        ! Return a boolean flag for successful operation
        logical, intent(out) :: success
    end subroutine process_kill
end interface kill    

!! version: experimental
!!
interface sleep
    module subroutine sleep(millisec)
        integer, intent(in) :: millisec
    end subroutine sleep
end interface sleep        

!! version: experimental
!!
interface 
    module logical function has_win32()
    end function has_win32
end interface    

end module stdlib_system
