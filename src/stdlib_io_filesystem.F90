! SPDX-Identifier: MIT

!> Interaction with the filesystem.
module stdlib_io_filesystem
    use stdlib_string_type, only: string_type
    use stdlib_error, only: state_type, STDLIB_FS_ERROR
    use stdlib_system, only: run, OS_TYPE, OS_UNKNOWN, OS_MACOS, OS_LINUX, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD, OS_WINDOWS
    implicit none
    private
    
    !! version: experimental
    !!
    !! Deletes a specified file from the filesystem.
    !! ([Specification](../page/specs/stdlib_io.html#delete_file-delete-a-file))
    !!
    !!### Summary
    !! Subroutine to safely delete a file from the filesystem. It handles errors gracefully using the library's `state_type`.
    !!
    !!### Description
    !! 
    !! This subroutine deletes a specified file. If the file does not exist, or if it is a directory or inaccessible, 
    !! an error is raised. Errors are handled using the library's `state_type` mechanism. If the optional `err` argument 
    !! is not provided, exceptions trigger an `error stop`.
    !!
    public :: delete_file
    
    !! version: experimental
    !!
    !! Tests if a given path matches an existing directory.
    !! ([Specification](../page/specs/stdlib_io.html#is_directory-test-if-a-path-is-a-directory))
    !!
    !!### Summary
    !! Function to evaluate whether a specified path corresponds to an existing directory.
    !!
    !!### Description
    !! 
    !! This function checks if a given file system path is a directory. It is cross-platform and avoids reliance 
    !! on external C libraries by utilizing system calls. It supports common operating systems such as Linux, macOS, 
    !! Windows, and various UNIX-like environments. On unsupported operating systems, the function will return `.false.`.
    !!
    public :: is_directory

contains

    !! Tests if a given path matches an existing directory.
    !! Cross-platform implementation without using external C libraries.
    logical function is_directory(path)
        !> Input path to evaluate
        character(*), intent(in) :: path
        
        integer :: stat
        
        select case (OS_TYPE())

            case (OS_UNKNOWN, OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)
                
                call run("test -d " // path, exit_state=stat)

            case (OS_WINDOWS)
                
                call run('cmd /c "if not exist ' // windows_path(path) // '\ exit /B 1"', exit_state=stat)
                        
            case default
                
                ! Unknown/invalid OS
                stat = -1

        end select

        is_directory = stat == 0
        
    end function is_directory

    subroutine delete_file(path, err)
        character(*), intent(in) :: path
        type(state_type), optional, intent(out) :: err
        
        !> Local variables
        integer :: file_unit, ios        
        type(state_type) :: err0
        character(len=512) :: msg
        logical :: file_exists

        ! Check if the path exists
        inquire(file=path, exist=file_exists)
        if (.not. file_exists) then
            ! File does not exist, return error status
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',path,': file does not exist')
            call err0%handle(err)
            return
        endif

        ! Verify the file is not a directory
        if (is_directory(path)) then 
            ! If unable to open, assume it's a directory or inaccessible
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',path,'- is a directory')
            call err0%handle(err)
            return            
        end if

        ! Close and delete the file
        close(unit=file_unit, status="delete", iostat=ios, iomsg=msg)
        if (ios /= 0) then
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',path,'-',msg)
            call err0%handle(err)
            return              
        end if
    end subroutine delete_file

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
