! SPDX-Identifier: MIT

!> Interaction with the filesystem.
module stdlib_io_filesystem
    use stdlib_string_type, only: string_type
    implicit none
    private

    public :: exists, list_dir, run, temp_dir

    character(*), parameter :: temp_dir = 'temp'
    character(*), parameter :: listed_contents = temp_dir//'/listed_contents.txt'

contains

    !> Version: experimental
    !>
    !> Whether a file or directory exists at the given path.
    !> [Specification](../page/specs/stdlib_io.html#exists)
    logical function exists(filename)
        !> Name of the file or directory.
        character(len=*), intent(in) :: filename

        inquire(file=filename, exist=exists)

#if defined(__INTEL_COMPILER)
        if (.not. exists) inquire(directory=filename, exist=exists)
#endif
    end

    !> Version: experimental
    !>
    !> List files and directories of a directory. Does not list hidden files.
    !> [Specification](../page/specs/stdlib_io.html#list_dir)
    subroutine list_dir(dir, files, iostat, iomsg)
        !> Directory to list.
        character(len=*), intent(in) :: dir
        !> List of files and directories.
        type(string_type), allocatable, intent(out) :: files(:)
        !> Status of listing.
        integer, optional, intent(out) :: iostat
        !> Error message.
        character(len=:), allocatable, optional, intent(out) :: iomsg

        integer :: unit, stat
        character(len=256) :: line

        stat = 0

        if (.not. exists(temp_dir)) then
            call run('mkdir '//temp_dir, stat)
            if (stat /= 0) then
                if (present(iostat)) iostat = stat
                if (present(iomsg)) iomsg = "Failed to create temporary directory '"//temp_dir//"'."
                return
            end if
        end if

        call run('ls '//dir//' > '//listed_contents, stat)
        if (stat /= 0) then
            if (present(iostat)) iostat = stat
            if (present(iomsg)) iomsg = "Failed to list files in directory '"//dir//"'."
            return
        end if

        open(newunit=unit, file=listed_contents, status='old', action='read', iostat=stat)
        if (stat /= 0) then
            if (present(iostat)) iostat = stat
            if (present(iomsg)) iomsg = "Failed to open file '"//listed_contents//"'."
            return
        end if

        allocate(files(0))
        do
            read(unit, '(A)', iostat=stat) line
            if (stat /= 0) exit
            files = [files, string_type(line)]
        end do
        close(unit, status="delete")
    end

    !> Version: experimental
    !>
    !> Run a command in the shell.
    !> [Specification](../page/specs/stdlib_io.html#run)
    subroutine run(command, iostat, iomsg)
        !> Command to run.
        character(len=*), intent(in) :: command
        !> Status of the operation.
        integer, intent(out), optional :: iostat
        !> Error message.
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: exitstat, cmdstat
        character(len=256) :: cmdmsg

        if (present(iostat)) iostat = 0
        exitstat = 0; cmdstat = 0

        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
        if (exitstat /= 0 .or. cmdstat /= 0) then
            if (present(iostat)) then
                if (exitstat /= 0) then
                    iostat = exitstat
                else
                    iostat = cmdstat
                end if
            end if
            if (present(iomsg) .and. trim(adjustl(cmdmsg)) /= '') iomsg = cmdmsg
        end if
    end
end
