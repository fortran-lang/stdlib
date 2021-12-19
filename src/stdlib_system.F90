! SPDX-Identifier: MIT

module stdlib_system
  use stdlib_error, only : error_stop
  use stdlib_string_type, only : string_type, char
  use, intrinsic :: iso_c_binding, only : c_int, c_long, c_char, c_null_char
  implicit none
  private

  public :: sleep
  public :: get_argument, get_variable, set_variable


  !> Obtain the command line argument at a given index
  interface get_argument
    module procedure :: get_argument_char
    module procedure :: get_argument_string
  end interface get_argument

  !> Obtain the value of an environment variable
  interface get_variable
    module procedure :: get_variable_char_char
    module procedure :: get_variable_string_char
    module procedure :: get_variable_char_string
    module procedure :: get_variable_string_string
  end interface get_variable

  !> Set the value of an environment variable
  interface set_variable
    module procedure :: set_variable_char_char
    module procedure :: set_variable_string_char
    module procedure :: set_variable_char_string
    module procedure :: set_variable_string_string
  end interface set_variable

  interface
#ifdef _WIN32
    !> version: experimental
    !>
    !> void Sleep(DWORD dwMilliseconds)
    !> https://docs.microsoft.com/en-us/windows/win32/api/synchapi/nf-synchapi-sleep
    subroutine winsleep(dwMilliseconds) bind (C, name='Sleep')
      import c_long
      integer(c_long), value, intent(in) :: dwMilliseconds
    end subroutine winsleep
#else
    !> version: experimental
    !>
    !> int usleep(useconds_t usec);
    !> https://linux.die.net/man/3/usleep
    integer(c_int) function usleep(usec) bind (C)
      import c_int
      integer(c_int), value, intent(in) :: usec
    end function usleep
#endif
  end interface

  interface
#ifndef _WIN32
    function sys_setenv(name, value, overwrite) result(stat) &
        & bind(c, name="setenv")
      import :: c_char, c_int
      character(len=c_char), intent(in) :: name(*)
      character(len=c_char), intent(in) :: value(*)
      integer(c_int), value :: overwrite
      integer(c_int) :: stat
    end function
#else
    function win_setenv(name, value) result(stat) &
        & bind(c, name="SetEnvironmentVariable")
      import :: c_char, c_int
      character(len=c_char), intent(in) :: name(*)
      character(len=c_char), intent(in) :: value(*)
      integer(c_int) :: stat
    end function
#endif
  end interface

contains

  !> version: experimental
  subroutine sleep(millisec)
    integer, intent(in) :: millisec
    integer(c_int) :: ierr

#ifdef _WIN32
    ! PGI Windows, Ifort Windows, ....
    call winsleep(int(millisec, c_long))
#else
    ! Linux, Unix, MacOS, MSYS2, ...
    ierr = usleep(int(millisec * 1000, c_int))
    if (ierr/=0) error stop 'problem with usleep() system call'
#endif

  end subroutine sleep

  !> Obtain the command line argument at a given index
  subroutine get_argument_char(idx, arg, stat)
    !> Index of command line argument, range [0:command_argument_count()]
    integer, intent(in) :: idx
    !> Command line argument
    character(len=:), allocatable, intent(out) :: arg
    !> Status of operation
    integer, intent(out), optional :: stat

    integer :: length, istat

    istat = 0
    call get_command_argument(idx, length=length, status=istat)
    if (istat == 0) then
      allocate(character(len=length) :: arg, stat=istat)
    endif

    if (length > 0 .and. istat == 0) then
      call get_command_argument(idx, arg, status=istat)
    end if

    if (present(stat)) then
      stat = istat
    else if (istat /= 0) then
      call error_stop('get_argument: error getting argument')
    end if
  end subroutine get_argument_char

  !> Obtain the command line argument at a given index
  subroutine get_argument_string(idx, arg, stat)
    !> Index of command line argument, range [0:command_argument_count()]
    integer, intent(in) :: idx
    !> Command line argument
    type(string_type), intent(out) :: arg
    !> Status of operation
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: tmp

    call get_argument(idx, tmp, stat)
    arg = string_type(tmp)
  end subroutine get_argument_string


  !> Obtain the value of an environment variable
  subroutine get_variable_char_char(var, val, stat)
    !> Name of variable
    character(len=*), intent(in) :: var
    !> Value of variable
    character(len=:), allocatable, intent(out) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    integer :: length, istat

    istat = 0
    call get_environment_variable(var, length=length, status=istat)
    if (istat == 0) then
      allocate(character(len=length) :: val, stat=istat)
    end if

    if (length > 0 .and. istat == 0) then
      call get_environment_variable(var, val, status=istat)
    end if

    if (present(stat)) then
      stat = istat
    else if (istat /= 0) then
      call error_stop('get_variable: error getting variable')
    end if
  end subroutine get_variable_char_char

  !> Obtain the value of an environment variable
  subroutine get_variable_string_char(var, val, stat)
    !> Name of variable
    type(string_type), intent(in) :: var
    !> Value of variable
    character(len=:), allocatable, intent(out) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    call get_variable(char(var), val, stat)
  end subroutine get_variable_string_char

  !> Obtain the value of an environment variable
  subroutine get_variable_char_string(var, val, stat)
    !> Name of variable
    character(len=*), intent(in) :: var
    !> Value of variable
    type(string_type), intent(out) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: tmp

    call get_variable(var, tmp, stat)
    val = string_type(tmp)
  end subroutine get_variable_char_string

  !> Obtain the value of an environment variable
  subroutine get_variable_string_string(var, val, stat)
    !> Name of variable
    type(string_type), intent(in) :: var
    !> Value of variable
    type(string_type), intent(out) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    character(len=:), allocatable :: tmp

    call get_variable(char(var), tmp, stat)
    val = string_type(tmp)
  end subroutine get_variable_string_string


  !> Set the value of an environment variable
  subroutine set_variable_char_char(var, val, stat)
    !> Name of variable
    character(len=*), intent(in) :: var
    !> Value of variable
    character(len=*), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    integer(c_int), parameter :: overwrite = 1_c_int
    integer :: istat

    istat = sys_setenv(as_c_char(var), as_c_char(val), overwrite)

    if (present(stat)) then
      stat = istat
    else if (istat /= 0) then
      call error_stop('set_variable: error setting variable')
    end if
  end subroutine set_variable_char_char

  !> Set the value of an environment variable
  subroutine set_variable_string_char(var, val, stat)
    !> Name of variable
    type(string_type), intent(in) :: var
    !> Value of variable
    character(len=*), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    call set_variable(char(var), val, stat)
  end subroutine set_variable_string_char

  !> Set the value of an environment variable
  subroutine set_variable_char_string(var, val, stat)
    !> Name of variable
    character(len=*), intent(in) :: var
    !> Value of variable
    type(string_type), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    call set_variable(var, char(val), stat)
  end subroutine set_variable_char_string

  !> Set the value of an environment variable
  subroutine set_variable_string_string(var, val, stat)
    !> Name of variable
    type(string_type), intent(in) :: var
    !> Value of variable
    type(string_type), intent(in) :: val
    !> Status of operation
    integer, intent(out), optional :: stat

    call set_variable(char(var), char(val), stat)
  end subroutine set_variable_string_string


  pure function as_c_char(str) result(res)
    character(len=*), intent(in) :: str
    character(kind=c_char) :: res(len(str)+1)
    res = transfer(str // c_null_char, res)
  end function as_c_char

#ifdef _WIN32
  function sys_setenv(name, value, overwrite) result(stat)
    character(len=c_char), intent(in) :: name(*)
    character(len=c_char), intent(in) :: value(*)
    integer(c_int), value :: overwrite
    integer(c_int) :: stat

    stat = win_setenv(name, value)
  end function
#endif

end module stdlib_system
