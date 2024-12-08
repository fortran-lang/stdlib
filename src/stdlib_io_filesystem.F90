! SPDX-Identifier: MIT

!> Interaction with the filesystem.
module stdlib_io_filesystem
    use stdlib_string_type, only: string_type
    use stdlib_error, only: state_type, STDLIB_FS_ERROR
    implicit none
    private

contains

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

        ! Try opening the file in read-only mode to verify it is a file, not a directory
        open(newunit=file_unit, file=filename, status="old", action="read", iostat=ios, iomsg=msg)
        if (ios /= 0) then
            ! If unable to open, assume it's a directory or inaccessible
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',filename,':',msg)
            call err0%handle(err)
            return            
        end if

        ! Close and delete the file
        close(unit=file_unit, status="delete", iostat=ios, iomsg=msg)
        if (ios /= 0) then
            err0 = state_type(STDLIB_FS_ERROR,'Cannot delete',filename,':',msg)
            call err0%handle(err)
            return              
        end if
    end subroutine delete_file

end module stdlib_io_filesystem
