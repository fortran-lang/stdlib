module stdlib_system
use, intrinsic :: iso_c_binding, only : c_int, c_long
implicit none
private
public :: sleep

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
if (ierr/=0) error stop 'problem with usleep() system call'
#endif


end subroutine sleep

!> Determine the current OS type
integer function OS_TYPE() result(os)
    if (have_os) then 
        os = OS_CURRENT
    else
        OS_CURRENT = runtime_os()
        have_os = .true.
        os = OS_CURRENT
    end if
end function OS_TYPE

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

end module stdlib_system
