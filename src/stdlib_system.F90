module stdlib_system
use, intrinsic :: iso_c_binding, only : c_int, c_long, c_ptr, c_null_ptr, c_int64_t, c_size_t, &
    c_f_pointer
use stdlib_kinds, only: int64, dp, c_bool, c_char
use stdlib_strings, only: to_c_char, find, to_string
use stdlib_string_type, only: string_type
use stdlib_optval, only: optval
use stdlib_error, only: state_type, STDLIB_SUCCESS, STDLIB_FS_ERROR
implicit none
private
public :: sleep

!! version: experimental
!!
!! Cached OS type retrieval with negligible runtime overhead.
!! ([Specification](../page/specs/stdlib_system.html#os_type-cached-os-type-retrieval))
!!
!! ### Summary
!! Provides a cached value for the runtime OS type. 
!!
!! ### Description
!! 
!! This function caches the result of `get_runtime_os` after the first invocation. 
!! Subsequent calls return the cached value, ensuring minimal overhead.
!!
public :: OS_TYPE

!! version: experimental
!!
!! Determine the current operating system (OS) type at runtime.
!! ([Specification](../page/specs/stdlib_system.html#get_runtime_os-determine-the-os-type-at-runtime))
!!
!! ### Summary
!! This function inspects the runtime environment to identify the OS type. 
!!
!! ### Description
!! 
!! The function evaluates environment variables (`OSTYPE` or `OS`) and filesystem attributes
!! to identify the OS. It distinguishes between several common operating systems:
!! - Linux
!! - macOS
!! - Windows
!! - Cygwin
!! - Solaris
!! - FreeBSD
!! - OpenBSD
!!
!! Returns a constant representing the OS type or `OS_UNKNOWN` if the OS cannot be determined.
!!                    
public :: get_runtime_os

!> Version: experimental
!>
!> Integer constants representing known operating system (OS) types
!> ([Specification](../page/specs/stdlib_system.html))
integer, parameter, public :: &
    !> Represents an unknown operating system
    OS_UNKNOWN = 0, &
    !> Represents a Linux operating system
    OS_LINUX = 1, &
    !> Represents a macOS operating system
    OS_MACOS = 2, &
    !> Represents a Windows operating system
    OS_WINDOWS = 3, &
    !> Represents a Cygwin environment
    OS_CYGWIN = 4, &
    !> Represents a Solaris operating system
    OS_SOLARIS = 5, &
    !> Represents a FreeBSD operating system
    OS_FREEBSD = 6, &
    !> Represents an OpenBSD operating system
    OS_OPENBSD = 7

!! Helper function returning the name of an OS parameter
public :: OS_NAME 

!> Public sub-processing interface
public :: run
public :: runasync
public :: process_type
public :: is_completed
public :: is_running
public :: update
public :: wait
public :: kill
public :: elapsed
public :: is_windows

!! Public path related functions and interfaces
public :: path_sep
public :: join_path
public :: operator(/)
public :: split_path
public :: base_name
public :: dir_name

!! version: experimental
!!
!! Tests if a given path matches an existing directory.
!! ([Specification](../page/specs/stdlib_system.html#is_directory-test-if-a-path-is-a-directory))
!!
!!### Summary
!! Function to evaluate whether a specified path corresponds to an existing directory.
!!
!!### Description
!! 
!! This function checks if a given file system path is a directory. It is cross-platform and utilizes
!! native system calls. It supports common operating systems such as Linux, macOS, 
!! Windows, and various UNIX-like environments. On unsupported operating systems, the function will return `.false.`.
!!
public :: is_directory

!! version: experimental
!!
!! Makes an empty directory.
!! ([Specification](../page/specs/stdlib_system.html#make_directory))
!!
!! ### Summary
!! Creates an empty directory with default permissions.
!!
!! ### Description
!! This function makes an empty directory according to the path provided.
!! Relative paths are supported. On Windows, paths involving either `/` or `\` are accepted.
!! An appropriate error message is returned whenever any error occurs.
!!
public :: make_directory

!! version: experimental
!!
!! Makes an empty directory, also creating all the parent directories required.
!! ([Specification](../page/specs/stdlib_system.html#make_directory))
!!
!! ### Summary
!! Creates an empty directory with all the parent directories required to do so.
!!
!! ### Description
!! This function makes an empty directory according to the path provided.
!! It also creates all the necessary parent directories in the path if they do not exist already.
!! Relative paths are supported.
!! An appropriate error message is returned whenever any error occurs.
!!
public :: make_directory_all

!! version: experimental
!!
!! Removes an empty directory.
!! ([Specification](../page/specs/stdlib_system.html#remove_directory))
!!
!! ### Summary
!! Removes an empty directory.
!!
!! ### Description
!! This function Removes an empty directory according to the path provided.
!! Relative paths are supported. On Windows paths involving either `/` or `\` are accepted.
!! An appropriate error message is returned whenever any error occurs.
!!
public :: remove_directory

!! version: experimental
!!
!! Gets the current working directory of the process
!! ([Specification](../page/specs/stdlib_system.html#get_cwd))
!!
!! ### Summary
!! Gets the current working directory.
!!
!! ### Description
!! This subroutine gets the current working directory the process is executing from.
!!
public :: get_cwd

!! version: experimental
!!
!! Sets the current working directory of the process
!! ([Specification](../page/specs/stdlib_system.html#set_cwd))
!!
!! ### Summary
!! Changes the current working directory to the one specified.
!!
!! ### Description
!! This subroutine sets the current working directory the process is executing from.
!!
public :: set_cwd

!! version: experimental
!!
!! Deletes a specified file from the filesystem.
!! ([Specification](../page/specs/stdlib_system.html#delete_file-delete-a-file))
!!
!!### Summary
!! Subroutine to safely delete a file from the filesystem. It handles errors gracefully using the library's `state_type`.
!!
!!### Description
!! 
!! This subroutine deletes a specified file. If the file is a directory or inaccessible, an error is raised.
!! If the file does not exist, a warning is returned, but no error state. Errors are handled using the 
!! library's `state_type` mechanism. If the optional `err` argument is not provided, exceptions trigger 
!! an `error stop`.
!!
public :: delete_file

!! version: experimental
!!
!! Returns the file path of the null device, which discards all data written to it.
!! ([Specification](../page/specs/stdlib_system.html#null_device-return-the-null-device-file-path))
!!
!! ### Summary
!! Function that provides the file path of the null device appropriate for the current operating system.
!!
!! ### Description
!!
!! The null device is a special file that discards all data written to it and always reads as 
!! an empty file. This function returns the null device path, adapted for the operating system in use.
!! 
!! On Windows, this is `NUL`. On UNIX-like systems, this is `/dev/null`.
!!
public :: null_device

!! version: experimental
!!
!! A helper function for returning the `type(state_type)` with the flag `STDLIB_FS_ERROR` set.
!! ([Specification](../page/specs/stdlib_system.html#FS_ERROR))
!!
public :: FS_ERROR

!! version: experimental
!!
!! A helper function for returning the `type(state_type)` with the flag `STDLIB_FS_ERROR` set.
!! It also formats and prefixes the `code` passed to it as the first argument
!! ([Specification](../page/specs/stdlib_system.html#FS_ERROR_CODE))
!!
public :: FS_ERROR_CODE
     
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
    
    !> Standard input
    character(:), allocatable :: stdin_file
    character(:), allocatable :: stdin
    
    !> Standard output
    character(:), allocatable :: stdout_file
    character(:), allocatable :: stdout
    
    !> Error output
    integer :: exit_code = 0    
    character(:), allocatable :: stderr_file
    character(:), allocatable :: stderr
    
    !> Callback function 
    procedure(process_callback), nopass, pointer :: oncomplete => null()
    
    !> Optional payload for the callback function
    class(*), pointer :: payload => null()
    
    !> Store time at the last update
    integer(TICKS) :: last_update = 0
    
contains

    !! Check if process is still running
    procedure :: is_running   => process_is_running
    
    !! Check if process is completed
    procedure :: is_completed => process_is_completed
    
    !! Return elapsed time since inception
    procedure :: elapsed      => process_lifetime
    
    !! Update process state internals
    procedure :: update       => update_process_state
    
    !! Kill a process
    procedure :: kill         => process_kill
    
    !! Get process ID
    procedure :: pid          => process_get_ID
    
end type process_type

interface runasync
    !! version: experimental
    !!
    !! Executes an external process asynchronously.
    !! ([Specification](../page/specs/stdlib_system.html#runasync-execute-an-external-process-asynchronously))
    !!
    !! ### Summary
    !! Provides methods for executing external processes asynchronously, using either a single command string 
    !! or an argument list, with options for output collection and standard input.
    !!
    !! ### Description
    !! 
    !! This interface allows the user to spawn external processes asynchronously (non-blocking). 
    !! Processes can be executed via a single command string or a list of arguments, with options to collect 
    !! standard output and error streams, or to provide a standard input stream via a `character` string.
    !! Additionally, a callback function can be provided, which will be called upon process completion.
    !! A user-defined payload can be attached and passed to the callback for handling process-specific data.
    !!
    !! @note The implementation depends on system-level process management capabilities.
    !!
    module function run_async_cmd(cmd, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> The command line string to execute.
        character(*), intent(in) :: cmd
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload
        !> The output process handler.
        type(process_type) :: process
        
    end function run_async_cmd

    module function run_async_args(args, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> List of arguments for the process to execute.
        character(*), intent(in) :: args(:)
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload        
        !> The output process handler.
        type(process_type) :: process        
    end function run_async_args
end interface runasync

interface run
    !! version: experimental
    !!
    !! Executes an external process synchronously.
    !! ([Specification](../page/specs/stdlib_system.html#run-execute-an-external-process-synchronously))
    !!
    !! ### Summary
    !! Provides methods for executing external processes synchronously, using either a single command string 
    !! or an argument list, with options for output collection and standard input.
    !!
    !! ### Description
    !! 
    !! This interface allows the user to spawn external processes synchronously (blocking), 
    !! via either a single command string or a list of arguments. It also includes options to collect 
    !! standard output and error streams, or to provide a standard input stream via a `character` string.
    !! Additionally, it supports an optional callback function that is invoked upon process completion, 
    !! allowing users to process results dynamically. A user-defined payload can also be provided, 
    !! which is passed to the callback function to facilitate contextual processing.
    !!
    !! @note The implementation depends on system-level process management capabilities.
    !!
    module function run_sync_cmd(cmd, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> The command line string to execute.
        character(*), intent(in) :: cmd
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload            
        !> The output process handler.
        type(process_type) :: process
    end function run_sync_cmd

    module function run_sync_args(args, stdin, want_stdout, want_stderr, callback, payload) result(process)
        !> List of arguments for the process to execute.
        character(*), intent(in) :: args(:)
        !> Optional input sent to the process via standard input (stdin).
        character(*), optional, intent(in) :: stdin
        !> Whether to collect standard output.
        logical, optional, intent(in) :: want_stdout
        !> Whether to collect standard error output.
        logical, optional, intent(in) :: want_stderr
        !> Optional callback function to be called on process completion
        procedure(process_callback), optional :: callback
        !> Optional payload to pass to the callback on completion
        class(*), optional, intent(inout), target :: payload            
        !> The output process handler.
        type(process_type) :: process        
    end function run_sync_args
    
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
    module function process_lifetime(process) result(delta_t)
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
        class(process_type), intent(inout) :: process
    end subroutine update_process_state
end interface update

interface kill
    !! version: experimental
    !!
    !! Terminates a running process.
    !! ([Specification](../page/specs/stdlib_system.html#kill-terminate-a-running-process))
    !!
    !! ### Summary
    !! Provides a method to kill or terminate a running process.
    !! Returns a boolean flag indicating whether the termination was successful.
    !!
    !! ### Description
    !! 
    !! This interface allows for the termination of an external process that is still running.  
    !! If the process is successfully killed, the `success` output flag is set to `.true.`, otherwise `.false.`.
    !! This function is useful for controlling and managing processes that are no longer needed or for forcefully
    !! stopping an unresponsive process.
    !!
    !! @note This operation may be system-dependent and could fail if the underlying user does not have 
    !! the necessary rights to kill a process.
    !!
    module subroutine process_kill(process, success)
        !> The process object to be terminated.
        class(process_type), intent(inout) :: process
        !> Boolean flag indicating whether the termination was successful.
        logical, intent(out) :: success
    end subroutine process_kill
end interface kill
 
interface sleep
    !! version: experimental
    !!
    !! Pauses execution for a specified time in milliseconds.
    !! ([Specification](../page/specs/stdlib_system.html#sleep-pause-execution-for-a-specified-time=in-milliseconds))
    !!
    !! ### Summary
    !! Pauses code execution for a specified number of milliseconds. This routine is a cross-platform
    !! wrapper around platform-specific sleep functions, providing consistent behavior on different operating systems.
    !!
    !! ### Description
    !! 
    !! This interface allows the user to pause the execution of a program for a specified duration, expressed in
    !! milliseconds. It provides a cross-platform wrapper around native sleep functions, ensuring that the program 
    !! will sleep for the requested amount of time on different systems (e.g., using `Sleep` on Windows or `nanosleep`
    !! on Unix-like systems).
    !!
    !! @note The precision of the sleep may vary depending on the system and platform.
    !!
    module subroutine sleep(millisec)
        !> The number of milliseconds to pause execution for.
        integer, intent(in) :: millisec
    end subroutine sleep
end interface sleep
      
abstract interface

    !! version: experimental
    !!
    !! Process callback interface
    !! 
    !! ### Summary 
    !!
    !! The `process_callback` interface defines a user-provided subroutine that will be called 
    !! upon process completion. It provides access to process metadata, including the process ID, 
    !! exit state, and optional input/output streams. If passed on creation, a generic payload can be 
    !! accessed by the callback function. This variable must be a valid `target` in the calling scope.
    !!  
    subroutine process_callback(pid,exit_state,stdin,stdout,stderr,payload)
        import process_ID
        implicit none
        !> Process ID
        integer(process_ID), intent(in) :: pid
        !> Process return state
        integer, intent(in) :: exit_state
        !> Process input/output: presence of these arguments depends on how process was created
        character(len=*), optional, intent(in) :: stdin,stdout,stderr
        !> Optional payload passed by the user on process creation
        class(*), optional, intent(inout) :: payload        
    end subroutine process_callback
end interface          
    
!! Static storage for the current OS
logical :: have_os    = .false.
integer :: OS_CURRENT = OS_UNKNOWN

interface 
    
    !! version: experimental
    !!
    !! Returns a `logical` flag indicating if the system is Windows.
    !! ([Specification](../page/specs/stdlib_system.html#is_windows-check-if-the-system-is-running-on-windows))
    !!
    !! ### Summary
    !! A fast, compile-time check to determine if the system is running Windows, based on the `_WIN32` macro.
    !!
    !! ### Description
    !! 
    !! This interface provides a function to check if the current system is Windows. The check is performed by
    !! wrapping a C function that tests if the `_WIN32` macro is defined. This check is fast and occurs at
    !! compile-time, making it a more efficient alternative to platform-specific runtime checks.
    !!
    !! The `is_windows` function is particularly useful for conditional compilation or system-specific code paths
    !! that are dependent on whether the code is running on Windows.
    !!
    !! @note This function relies on the `_WIN32` macro, which is defined in C compilers when targeting Windows.
    !!
    module logical function is_windows()
    end function is_windows
    
    module function process_get_ID(process) result(ID)
        class(process_type), intent(in) :: process
        !> Return a process ID
        integer(process_ID) :: ID
    end function process_get_ID
    
end interface 

interface join_path
    !! version: experimental
    !!
    !!### Summary
    !! join the paths provided according to the OS-specific path-separator
    !! ([Specification](../page/specs/stdlib_system.html#join_path))
    !!
    module function join2_char_char(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2
    end function join2_char_char

    module function join2_char_string(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1
        type(string_type), intent(in) :: p2
    end function join2_char_string

    module function join2_string_char(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1
        character(*), intent(in) :: p2
    end function join2_string_char

    module function join2_string_string(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1, p2
    end function join2_string_string

    module function joinarr_char(p) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p(:)
    end function joinarr_char

    module function joinarr_string(p) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p(:)
    end function joinarr_string
end interface join_path

interface operator(/)
    !! version: experimental
    !!
    !!### Summary
    !! A binary operator to join the paths provided according to the OS-specific path-separator
    !! ([Specification](../page/specs/stdlib_system.html#operator(/)))
    !!
    module function join_op_char_char(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1, p2
    end function join_op_char_char

    module function join_op_char_string(p1, p2) result(path)
        character(:), allocatable :: path
        character(*), intent(in) :: p1
        type(string_type), intent(in) :: p2
    end function join_op_char_string

    module function join_op_string_char(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1
        character(*), intent(in) :: p2
    end function join_op_string_char

    module function join_op_string_string(p1, p2) result(path)
        type(string_type) :: path
        type(string_type), intent(in) :: p1, p2
    end function join_op_string_string
end interface operator(/)

interface split_path
    !! version: experimental
    !!
    !!### Summary
    !! splits the path immediately following the final path-separator
    !! separating into typically a directory and a file name.
    !! ([Specification](../page/specs/stdlib_system.html#split_path))
    !!
    !!### Description
    !! If the path is empty `head`='.' and tail=''
    !! If the path only consists of separators, `head` is set to the separator and tail is empty
    !! If the path is a root directory, `head` is set to that directory and tail is empty
    !! `head` ends with a path-separator iff the path appears to be a root directory or a child of the root directory
    module subroutine split_path_char(p, head, tail)
        character(*), intent(in) :: p
        character(:), allocatable, intent(out) :: head, tail
    end subroutine split_path_char

    module subroutine split_path_string(p, head, tail)
        type(string_type), intent(in) :: p
        type(string_type), intent(out) :: head, tail
    end subroutine split_path_string
end interface split_path

interface base_name
    !! version: experimental
    !!
    !!### Summary
    !! returns the base name (last component) of the provided path
    !! ([Specification](../page/specs/stdlib_system.html#base_name))
    !!
    !!### Description
    !! The value returned is the `tail` of the interface `split_path`
    module function base_name_char(p) result(base)
        character(:), allocatable :: base
        character(*), intent(in) :: p
    end function base_name_char

    module function base_name_string(p) result(base)
        type(string_type) :: base
        type(string_type), intent(in) :: p
    end function base_name_string
end interface base_name

interface dir_name
    !! version: experimental
    !!
    !!### Summary
    !! returns everything but the last component of the provided path
    !! ([Specification](../page/specs/stdlib_system.html#dir_name))
    !!
    !!### Description
    !! The value returned is the `head` of the interface `split_path`
    module function dir_name_char(p) result(dir)
        character(:), allocatable :: dir
        character(*), intent(in) :: p
    end function dir_name_char

    module function dir_name_string(p) result(dir)
        type(string_type) :: dir
        type(string_type), intent(in) :: p
    end function dir_name_string
end interface dir_name


contains

integer function get_runtime_os() result(os)
    !! The function identifies the OS by inspecting environment variables and filesystem attributes.
    !!
    !! ### Returns:
    !! - **OS_UNKNOWN**: If the OS cannot be determined.
    !! - **OS_LINUX**, **OS_MACOS**, **OS_WINDOWS**, **OS_CYGWIN**, **OS_SOLARIS**, **OS_FREEBSD**, or **OS_OPENBSD**.
    !!
    !! Note: This function performs a detailed runtime inspection, so it has non-negligible overhead.
    
    ! Local variables
    character(len=255) :: val
    integer            :: length, rc
    logical            :: file_exists

    os = OS_UNKNOWN

    ! Check environment variable `OSTYPE`.
    call get_environment_variable('OSTYPE', val, length, rc)

    if (rc == 0 .and. length > 0) then
        ! Linux
        if (index(val, 'linux') > 0) then
            os = OS_LINUX
            return

        ! macOS
        elseif (index(val, 'darwin') > 0) then
            os = OS_MACOS
            return

        ! Windows, MSYS, MinGW, Git Bash
        elseif (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
            os = OS_WINDOWS
            return

        ! Cygwin
        elseif (index(val, 'cygwin') > 0) then
            os = OS_CYGWIN
            return

        ! Solaris, OpenIndiana, ...
        elseif (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
            os = OS_SOLARIS
            return

        ! FreeBSD
        elseif (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
            os = OS_FREEBSD
            return

        ! OpenBSD
        elseif (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
            os = OS_OPENBSD
            return
        end if
    end if

    ! Check environment variable `OS`.
    call get_environment_variable('OS', val, length, rc)

    if (rc == 0 .and. length > 0 .and. index(val, 'Windows_NT') > 0) then
        os = OS_WINDOWS
        return
    end if

    ! Linux
    inquire (file='/etc/os-release', exist=file_exists)

    if (file_exists) then
        os = OS_LINUX
        return
    end if

    ! macOS
    inquire (file='/usr/bin/sw_vers', exist=file_exists)

    if (file_exists) then
        os = OS_MACOS
        return
    end if

    ! FreeBSD
    inquire (file='/bin/freebsd-version', exist=file_exists)

    if (file_exists) then
        os = OS_FREEBSD
        return
    end if
    
end function get_runtime_os

!> Retrieves the cached OS type for minimal runtime overhead.
integer function OS_TYPE() result(os)
    !! This function uses a static cache to avoid recalculating the OS type after the first call.
    !! It is recommended for performance-sensitive use cases where the OS type is checked multiple times.
    if (.not.have_os) then 
        OS_CURRENT = get_runtime_os()
        have_os = .true.        
    end if
    os = OS_CURRENT
end function OS_TYPE

!> Return string describing the OS type flag
pure function OS_NAME(os)
    integer, intent(in) :: os
    character(len=:), allocatable :: OS_NAME

    select case (os)
        case (OS_LINUX);   OS_NAME =  "Linux"
        case (OS_MACOS);   OS_NAME =  "macOS"
        case (OS_WINDOWS); OS_NAME =  "Windows"
        case (OS_CYGWIN);  OS_NAME =  "Cygwin"
        case (OS_SOLARIS); OS_NAME =  "Solaris"
        case (OS_FREEBSD); OS_NAME =  "FreeBSD"
        case (OS_OPENBSD); OS_NAME =  "OpenBSD"
        case default     ; OS_NAME =  "Unknown"
    end select
end function OS_NAME

!! Tests if a given path matches an existing directory.
!! Cross-platform implementation without using external C libraries.
logical function is_directory(path)
    !> Input path to evaluate
    character(*), intent(in) :: path

    interface
        
        logical(c_bool) function stdlib_is_directory(path) bind(c, name="stdlib_is_directory")
            import c_bool, c_char
            character(kind=c_char), intent(in) :: path(*)
        end function stdlib_is_directory

    end interface        
    
    is_directory = logical(stdlib_is_directory(to_c_char(trim(path))))
    
end function is_directory

! A Helper function to convert C character arrays to Fortran character strings
function to_f_char(c_str_ptr, len) result(f_str)
    type(c_ptr), intent(in) :: c_str_ptr
    ! length of the string excluding the null character
    integer(kind=c_size_t), intent(in) :: len
    character(:), allocatable :: f_str

    integer :: i
    character(kind=c_char), pointer :: c_str(:)

    call c_f_pointer(c_str_ptr, c_str, [len])

    allocate(character(len=len) :: f_str)

    do concurrent (i=1:len)
        f_str(i:i) = c_str(i)
    end do
end function to_f_char

! A helper function to get the result of the C function `strerror`.
! `strerror` is a function provided by `<string.h>`. 
! It returns a string describing the meaning of `errno` in the C header `<errno.h>`
function c_get_strerror() result(str)
    character(len=:), allocatable :: str

    interface
        type(c_ptr) function strerror(len) bind(C, name='stdlib_strerror')
            import c_size_t, c_ptr
            implicit none
            integer(c_size_t), intent(out) :: len
        end function strerror
    end interface

    type(c_ptr) :: c_str_ptr
    integer(c_size_t) :: len

    c_str_ptr = strerror(len)

    str = to_f_char(c_str_ptr, len)
end function c_get_strerror

!! makes an empty directory
subroutine make_directory(path, err)
    character(len=*), intent(in) :: path
    type(state_type), optional, intent(out) :: err

    integer :: code
    type(state_type) :: err0

    interface
        integer function stdlib_make_directory(cpath) bind(C, name='stdlib_make_directory')
            import c_char
            character(kind=c_char), intent(in) :: cpath(*)
        end function stdlib_make_directory
    end interface

    code = stdlib_make_directory(to_c_char(trim(path)))

    if (code /= 0) then
        err0 = FS_ERROR_CODE(code, c_get_strerror())
        call err0%handle(err)
    end if

end subroutine make_directory

subroutine make_directory_all(path, err)
    character(len=*), intent(in) :: path
    type(state_type), optional, intent(out) :: err

    integer :: i, indx
    type(state_type) :: err0
    character(len=1) :: sep
    logical :: is_dir, check_is_dir

    sep = path_sep()
    i = 1
    indx = find(path, sep, i)
    check_is_dir = .true.

    do
        ! Base case to exit the loop
        if (indx == 0) then
            is_dir = is_directory(path)

            if (.not. is_dir) then
                call make_directory(path, err0)

                if (err0%error()) then
                    call err0%handle(err)
                end if
            end if

            return
        end if

        if (check_is_dir) then
            is_dir = is_directory(path(1:indx))
        end if

        if (.not. is_dir) then
            ! no need for further `is_dir` checks
            ! all paths going forward need to be created
            check_is_dir = .false.
            call make_directory(path(1:indx), err0)

            if (err0%error()) then
                call err0%handle(err)
                return
            end if
        end if

        i = i + 1 ! the next occurence of `sep`
        indx = find(path, sep, i)
    end do
end subroutine make_directory_all

!! removes an empty directory
subroutine remove_directory(path, err)
    character(len=*), intent(in) :: path
    type(state_type), optional, intent(out) :: err

    integer :: code
    type(state_type) :: err0

    interface
        integer function stdlib_remove_directory(cpath) bind(C, name='stdlib_remove_directory')
            import c_char
            character(kind=c_char), intent(in) :: cpath(*)
        end function stdlib_remove_directory
    end interface

    code = stdlib_remove_directory(to_c_char(trim(path)))

    if (code /= 0) then
        err0 = FS_ERROR_CODE(code, c_get_strerror())
        call err0%handle(err)
    end if

end subroutine remove_directory

subroutine get_cwd(cwd, err)
    character(:), allocatable, intent(out) :: cwd
    type(state_type), optional, intent(out) :: err
    type(state_type) :: err0

    interface
        type(c_ptr) function stdlib_get_cwd(len, stat) bind(C, name='stdlib_get_cwd')
            import c_ptr, c_size_t
            integer(c_size_t), intent(out) :: len
            integer :: stat
        end function stdlib_get_cwd
    end interface

    type(c_ptr) :: c_str_ptr
    integer(c_size_t) :: len
    integer :: stat

    c_str_ptr = stdlib_get_cwd(len, stat)

    if (stat /= 0) then
        err0 = FS_ERROR_CODE(stat, c_get_strerror())
        call err0%handle(err)
    end if

    cwd = to_f_char(c_str_ptr, len)

end subroutine get_cwd

subroutine set_cwd(path, err)
    character(len=*), intent(in) :: path
    type(state_type), optional, intent(out) :: err
    type(state_type) :: err0

    interface
        integer function stdlib_set_cwd(path) bind(C, name='stdlib_set_cwd')
            import c_char
            character(kind=c_char), intent(in) :: path(*)
        end function stdlib_set_cwd
    end interface

    integer :: code

    code = stdlib_set_cwd(to_c_char(trim(path)))

    if (code /= 0) then
        err0 = FS_ERROR_CODE(code, c_get_strerror())
        call err0%handle(err)
    end if
end subroutine set_cwd

!> Returns the file path of the null device for the current operating system.
!>
!> Version: Helper function.
function null_device() result(path)
    !> File path of the null device
    character(:), allocatable :: path
    
    interface
    
        ! No-overhead return path to the null device
        type(c_ptr) function process_null_device(len) bind(C,name='process_null_device')
            import c_ptr, c_size_t    
            implicit none
            integer(c_size_t), intent(out) :: len
        end function process_null_device    
        
    end interface
    
    integer(c_size_t) :: len
    type(c_ptr) :: c_path_ptr

    ! Call the C function to get the null device path and its length
    c_path_ptr = process_null_device(len)

    path = to_f_char(c_path_ptr, len)
end function null_device

!> Delete a file at the given path.
subroutine delete_file(path, err)
    character(*), intent(in) :: path
    type(state_type), optional, intent(out) :: err

    !> Local variables
    integer :: file_unit, ios        
    type(state_type) :: err0
    character(len=512) :: msg
    logical :: file_exists

    ! Verify the file is not a directory.     
    if (is_directory(path)) then 
        ! If unable to open, assume it's a directory or inaccessible
        err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',path,'- is a directory')
        call err0%handle(err)
        return            
    end if

    ! Check if the path exists
    ! Because Intel compilers return .false. if path is a directory, this must be tested
    ! _after_ the directory test
    inquire(file=path, exist=file_exists)
    if (.not. file_exists) then
        ! File does not exist, return non-error status
        err0 = state_type(STDLIB_SUCCESS,path,' not deleted: file does not exist')
        call err0%handle(err)
        return
    endif

    ! Close and delete the file
    open(newunit=file_unit, file=path, status='old', iostat=ios, iomsg=msg)
    if (ios /= 0) then
        err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',path,'-',msg)
        call err0%handle(err)
        return              
    end if        
    close(unit=file_unit, status='delete', iostat=ios, iomsg=msg)
    if (ios /= 0) then
        err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',path,'-',msg)
        call err0%handle(err)
        return              
    end if
end subroutine delete_file

pure function FS_ERROR_CODE(code,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,& 
        a11,a12,a13,a14,a15,a16,a17,a18,a19) result(state)

    type(state_type) :: state
    !> Platform specific error code
    integer, intent(in) :: code
    !> Optional rank-agnostic arguments
    class(*), intent(in), optional, dimension(..) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,&
        a11,a12,a13,a14,a15,a16,a17,a18,a19

    character(32) :: code_msg

    write(code_msg, "('code - ', i0, ',')") code

    state = state_type(STDLIB_FS_ERROR, code_msg,a1,a2,a3,a4,a5,a6,a7,a8,&
        a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19)
end function FS_ERROR_CODE

pure function FS_ERROR(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,&
        a12,a13,a14,a15,a16,a17,a18,a19,a20) result(state)

    type(state_type) :: state
    !> Optional rank-agnostic arguments
    class(*), intent(in), optional, dimension(..) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,&
        a11,a12,a13,a14,a15,a16,a17,a18,a19,a20

    state = state_type(STDLIB_FS_ERROR, a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,&
        a13,a14,a15,a16,a17,a18,a19,a20)
end function FS_ERROR

character function path_sep()
    if (OS_TYPE() == OS_WINDOWS) then
        path_sep = '\'
    else
        path_sep = '/'
    end if
end function path_sep

end module stdlib_system
