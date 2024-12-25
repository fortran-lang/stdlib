module stdlib_system
use, intrinsic :: iso_c_binding, only : c_int, c_long, c_null_ptr, c_int64_t
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

!> Process type holding process information and the connected stdout, stderr, stdin units 
type :: process_type
    
    !> Process ID (if external); 0 if run by the program process
    integer(process_ID)  :: id = FORKED_PROCESS
    
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
    !! version: experimental
    !!
    !! Executes an external process, either synchronously or asynchronously.
    !! ([Specification](../page/specs/stdlib_system.html#run-execute-an-external-process))
    !!
    !! ### Summary
    !! Provides methods for executing external processes via a single command string or an argument list, 
    !! with options for synchronous or asynchronous execution and output collection.
    !!
    !! ### Description
    !! 
    !! This interface allows the user to spawn external processes using either a single command string 
    !! or a list of arguments. Processes can be executed synchronously (blocking) or asynchronously 
    !! (non-blocking), with optional request to collect standard output and error streams, or to provide
    !! a standard input stream via a `character` string.
    !!
    !! @note The implementation depends on system-level process management capabilities.
    !!
    !! #### Methods
    !!
    !! - `process_open_cmd`: Opens a process using a command string.
    !! - `process_open_args`: Opens a process using an array of arguments.
    !! 
    module type(process_type) function process_open_cmd(cmd, wait, stdin, want_stdout, want_stderr) result(process)
        !> The command line string to execute.
        character(*), intent(in) :: cmd
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to wait for process completion (synchronous).
        logical, optional, intent(in) :: wait
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
    end function process_open_cmd

    module type(process_type) function process_open_args(args, wait, stdin, want_stdout, want_stderr) result(process)
        !> List of arguments for the process to execute.
        character(*), intent(in) :: args(:)
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to wait for process completion (synchronous).
        logical, optional, intent(in) :: wait
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
    end function process_open_args
end interface run

interface is_running
    !! version: experimental
    !!
    !! Checks if an external process is still running.
    !! ([Specification](../page/specs/stdlib_system.html#is_running-check-if-a-process-is-still-running))
    !!
    !! ### Summary
    !! Provides a method to determine if an external process is still actively running.
    !!
    !! ### Description
    !! 
    !! This interface checks the status of an external process to determine whether it is still actively running. 
    !! It is particularly useful for monitoring asynchronous processes created using the `run` interface. 
    !! The internal state of the `process_type` object is updated after the call to reflect the current process status.
    !!
    !! @note The implementation relies on system-level process management capabilities.
    !!
    module logical function process_is_running(process) result(is_running)
        !> The process object to check.
        class(process_type), intent(inout) :: process
        !> Logical result: `.true.` if the process is still running, `.false.` otherwise.
    end function process_is_running
end interface is_running


interface is_completed
    !! version: experimental
    !!
    !! Checks if an external process has completed execution.
    !! ([Specification](../page/specs/stdlib_system.html#is_completed-check-if-a-process-has-completed-execution))
    !!
    !! ### Summary
    !! Provides a method to determine if an external process has finished execution.
    !!
    !! ### Description
    !! 
    !! This interface checks the status of an external process to determine whether it has finished execution. 
    !! It is particularly useful for monitoring asynchronous processes created using the `run` interface. 
    !! The internal state of the `process_type` object is updated after the call to reflect the current process status.
    !!
    !! @note The implementation relies on system-level process management capabilities.
    !!
    module logical function process_is_completed(process) result(is_completed)
        !> The process object to check.
        class(process_type), intent(inout) :: process
        !> Logical result: `.true.` if the process has completed, `.false.` otherwise.
    end function process_is_completed
end interface is_completed

interface elapsed
    !! version: experimental
    !!
    !! Returns the lifetime of a process, in seconds.
    !! ([Specification](../page/specs/stdlib_system.html#elapsed-return-process-lifetime-in-seconds))
    !!
    !! ### Summary
    !! Provides the total elapsed time (in seconds) since the creation of the specified process.
    !!
    !! ### Description
    !! 
    !! This interface returns the total elapsed time (in seconds) for a given process since it was started. 
    !! If the process is still running, the value returned reflects the time from the creation of the process 
    !! until the call to this function. Otherwise, the total process duration until completion is returned.
    !!
    module real(RTICKS) function process_lifetime(process) result(delta_t)
        !> The process object for which to calculate elapsed time.
        class(process_type), intent(in) :: process
        !> The elapsed time in seconds since the process started.
        real(RTICKS) :: delta_t
    end function process_lifetime
end interface elapsed


interface wait
    !! version: experimental
    !!
    !! Waits for a running process to complete.
    !! ([Specification](../page/specs/stdlib_system.html#wait-wait-until-a-running-process-is-completed))
    !!
    !! ### Summary
    !! Provides a method to block the execution and wait until the specified process finishes.
    !! Supports an optional maximum wait time, after which the function returns regardless of process completion.
    !!
    !! ### Description
    !! 
    !! This interface allows waiting for a process to complete. If the process is running asynchronously, this subroutine
    !! will block further execution until the process finishes. Optionally, a maximum wait time can be specified; if 
    !! the process doesn't complete within this time, the subroutine returns without further waiting.
    !!
    !! @note The process state is accordingly updated on return from this call.
    !!
    module subroutine wait_for_completion(process, max_wait_time)
        !> The process object to monitor.
        class(process_type), intent(inout) :: process
        !> Optional maximum wait time in seconds. If not provided, waits indefinitely.
        real, optional, intent(in) :: max_wait_time
    end subroutine wait_for_completion
end interface wait

interface update
    !! version: experimental
    !!
    !! Updates the internal state of a process variable.
    !! ([Specification](../page/specs/stdlib_system.html#update-update-the-internal-state-of-a-process))
    !!
    !! ### Summary
    !! Provides a method to query the system and update the internal state of the specified process variable.
    !!
    !! ### Description
    !! 
    !! This subroutine queries the system to retrieve and update information about the state of the process.
    !! Once the process is completed, and if standard output or standard error were requested, their respective
    !! data is loaded into the `process%stdout` and `process%stderr` variables. This routine is useful for keeping
    !! track of the latest state and output of a process, particularly for asynchronous processes.
    !!
    !! @note This subroutine should be called periodically for asynchronous processes to check their completion
    !! and retrieve the output.
    !!
    module subroutine update_process_state(process)
        !> The process object whose state needs to be updated.
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
