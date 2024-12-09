! SPDX-Identifier: MIT

!> Interaction with the filesystem.
module stdlib_io_filesystem
    use stdlib_string_type, only: string_type
    use stdlib_error, only: state_type, STDLIB_FS_ERROR
    use stdlib_system, only: OS_TYPE
    implicit none
    private
    
    public :: delete_file

contains

    !> test if a name matches an existing directory path. 
    !> Cross-platform version that does not use C externals
    logical function is_directory(path)
        character(*), intent(in) :: path 
        
        integer :: ios
        

        select case (get_os_type())

            case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
                call execute_command_line("test -d " // dir,   &
                        & exitstat=stat,echo=.false.,verbose=.false.)

            case (OS_WINDOWS)
                call run('cmd /c "if not exist ' // windows_path(dir) // '\ exit /B 1"', &
                        & exitstat=stat,echo=.false.,verbose=.false.)

        end select

        is_directory = stat == 0
        
    end function is_directory


    subroutine delete_file(filename, err)
        character(*), intent(in) :: filename
        type(state_type), optional, intent(out) :: err
        
        !> Local variables
        integer :: file_unit, ios        
        type(state_type) :: err0
        character(len=512) :: msg
        logical :: file_exists

        ! Check if the filename is a file or a directory by inquiring about its existence
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            ! File does not exist, return error status
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',filename,': file does not exist')
            call err0%handle(err)
            return
        endif

        ! Try opening the file in "readwrite" mode to verify it is a file, not a directory
        ! Because we're trying to delete the file, we need write access anyways. This will 
        ! be forbidden if this is a directory
        open(newunit=file_unit, file=filename, status="old", action="readwrite", iostat=ios, iomsg=msg)
        
        print *, 'IOS ',ios,' IOMSG ',trim(msg)
        
        if (ios /= 0) then
            ! If unable to open, assume it's a directory or inaccessible
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',filename,'-',msg)
            call err0%handle(err)
            return            
        end if

        ! Close and delete the file
        close(unit=file_unit, status="delete", iostat=ios, iomsg=msg)
        if (ios /= 0) then
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',filename,'-',msg)
            call err0%handle(err)
            return              
        end if
    end subroutine delete_file

    ! Run a command
    subroutine run(cmd,exitstat,cmdstat,cmdmsg,screen_output)
        character(len=*), intent(in) :: cmd
        integer, intent(out), optional :: exitstat,cmdstat
        character(*), optional, intent(out) :: cmdmsg
        type(string_type), optional, intent(out) :: screen_output

        character(len=256) :: iomsg
        logical :: want_output
        character(:), allocatable :: redirect_str,redirect_file
        integer :: cstat, stat, fh, iostat
        
        want_output = present(screen_output)

        if (want_output) then
            
            ! Redirect output to a file
            redirect_file = scratch_name()
            redirect_str  =  ">"//redirect_file//" 2>&1"

        else
            ! No redirection and non-verbose output
            if (os_is_unix()) then
                redirect_str = " >/dev/null 2>&1"
            else
                redirect_str = " >NUL 2>&1"
            end if
                        
        end if

        call execute_command_line(cmd//redirect_str, exitstat=stat,cmdstat=cstat,cmdmsg=iomsg)
        
        if (want_output) then            
            call screen_output%read_ascii_file(redirect_file,iostat=iostat,iomsg=iomsg,delete=.true.)
        end if

        if (present(exitstat)) then
            exitstat = stat
        elseif (stat /= 0) then
            error stop 'Cannot run '//cmd
        end if

        if (present(cmdstat))  cmdstat  = cstat
        if (present(cmdmsg))   cmdmsg   = iomsg

    end subroutine run

    !> Replace file system separators for windows
    function windows_path(path) result(winpath)

        character(*), intent(in) :: path
        character(len(path)) :: winpath

        integer :: idx

        winpath = path
        idx = index(winpath,'/')
        do while(idx > 0)
            winpath(idx:idx) = '\'
            idx = index(winpath,'/')
        end do

    end function windows_path

end module stdlib_io_filesystem
