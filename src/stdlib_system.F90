module stdlib_system
use, intrinsic :: iso_c_binding, only : c_int, c_long
use stdlib_string_type, only: string_type, assignment(=)
use stdlib_io, only: getfile
use stdlib_error, only: error_stop, state_type
implicit none
private
public :: sleep

!! version: experimental
!!
!! Executes a synchronous command in the system shell and optionally retrieves output and error messages.
!! ([Specification](../page/specs/stdlib_system.html#run-execute-a-synchronous-command))
!!
!! ### Summary
!! Subroutine interface for running a shell command synchronously, capturing its exit and command states, 
!! and optionally retrieving the command's `stdout` and `stderr`.
!!
!! ### Description
!!
!! This interface enables executing a system command with the option to retrieve outputs. The execution 
!! is synchronous, meaning the calling program waits until the command completes before proceeding. 
!! The command's status codes, `stdout`, and `stderr` outputs can be retrieved through optional arguments.
!!
!! @note Implementation is based on Fortran's `execute_command_line`.
!!
public :: run


public :: null_device

!! OS type inquiry
                    public :: OS_NAME
                    public :: OS_TYPE
                    
!! Public parameters defining known OS types                    
integer, parameter, public :: OS_UNKNOWN = 0
integer, parameter, public :: OS_LINUX   = 1
integer, parameter, public :: OS_MACOS   = 2
integer, parameter, public :: OS_WINDOWS = 3
integer, parameter, public :: OS_CYGWIN  = 4
integer, parameter, public :: OS_SOLARIS = 5
integer, parameter, public :: OS_FREEBSD = 6
integer, parameter, public :: OS_OPENBSD = 7

!! Static storage for the current OS
logical :: have_os    = .false.
integer :: OS_CURRENT = OS_UNKNOWN

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
if (ierr/=0) call error_stop('problem with usleep() system call')
#endif


end subroutine sleep

!> Determine the current OS type
integer function OS_TYPE() result(os)
    if (.not.have_os) then 
        OS_CURRENT = runtime_os()
        have_os = .true.        
    end if
    os = OS_CURRENT
end function OS_TYPE

!> Return the file path of the null device.
function null_device()
    character(:), allocatable :: null_device
    if (OS_TYPE()==OS_WINDOWS) then 
        null_device = 'NUL'
    else
        null_device = '/dev/null'
    end if
end function null_device

!> Determine the current OS type at runtime
integer function runtime_os() result(os)
    !!
    !! Returns one of OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_WINDOWS, OS_CYGWIN,
    !! OS_SOLARIS, OS_FREEBSD, OS_OPENBSD.
    !!
    !! At first, the environment variable `OS` is checked, which is usually
    !! found on Windows. Then, `OSTYPE` is read in and compared with common
    !! names. If this fails too, check the existence of files that can be
    !! found on specific system types only.
    !!
    !! Returns OS_UNKNOWN if the operating system cannot be determined.
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
        end if

        ! macOS
        if (index(val, 'darwin') > 0) then
            os = OS_MACOS
            return
        end if

        ! Windows, MSYS, MinGW, Git Bash
        if (index(val, 'win') > 0 .or. index(val, 'msys') > 0) then
            os = OS_WINDOWS
            return
        end if

        ! Cygwin
        if (index(val, 'cygwin') > 0) then
            os = OS_CYGWIN
            return
        end if

        ! Solaris, OpenIndiana, ...
        if (index(val, 'SunOS') > 0 .or. index(val, 'solaris') > 0) then
            os = OS_SOLARIS
            return
        end if

        ! FreeBSD
        if (index(val, 'FreeBSD') > 0 .or. index(val, 'freebsd') > 0) then
            os = OS_FREEBSD
            return
        end if

        ! OpenBSD
        if (index(val, 'OpenBSD') > 0 .or. index(val, 'openbsd') > 0) then
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
end function runtime_os

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

!> Executes a synchronous shell command and optionally retrieves its outputs.
pure subroutine run(cmd, exit_state, command_state, stdout, stderr)
    !> Command to execute as a string
    character(len=*), intent(in) :: cmd
    !> [optional] Exit state of the command
    integer, intent(out), optional :: exit_state
    !> [optional] Command state, indicating issues with command invocation
    integer, intent(out), optional :: command_state
    !> [optional] Captured standard output (stdout)
    type(string_type), optional, intent(out) :: stdout
    !> [optional] Captured standard error (stderr)
    type(string_type), optional, intent(out) :: stderr

    !> Local variables
    character(len=4096) :: iomsg
    type(state_type) :: err
    logical :: want_stdout, want_stderr
    character(:), allocatable :: redirect_file
    integer :: cstat, estat, fh, iostat
    
    want_stdout = present(stdout)
    want_stderr = present(stderr)

    if (want_stdout) then
        ! Redirect output to a file
        redirect_file = scratch_name()
    else
        redirect_file = null_device()
    endif

    ! Execute command        
    call execute_command_line(cmd//" >"//redirect_file//" 2>&1", wait = .true., exitstat=estat,cmdstat=cstat,cmdmsg=iomsg)
    
    ! Retrieve stdout, stderr
    if (want_stdout) stdout = getfile(redirect_file,delete=.true.)
    if (want_stderr) stderr = trim(iomsg)

    if (present(exit_state)) then
        exit_state = estat
    elseif (estat /= 0) then
        call error_stop('Cannot run: '//cmd)
    end if

    if (present(command_state)) then 
        command_state = cstat
    elseif (cstat /= 0) then 
        call error_stop('Command error: '//cmd)
    endif
    
    contains
    
        ! Simple timestamp-based temporary name generation
        function scratch_name() result(temp_filename)
            character(:), allocatable :: temp_filename
            character(len=10) :: timestamp,yyyymmdd

            call date_and_time(date=yyyymmdd,time=timestamp)

            temp_filename = 'tmp_' // yyyymmdd(1:8) //'_'// timestamp(1:6) // '_' // timestamp(8:10) // '.tmp'
        end function scratch_name    
    
end subroutine run

end module stdlib_system
