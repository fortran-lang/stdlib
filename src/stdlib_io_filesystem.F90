! SPDX-Identifier: MIT

 !> Interaction with the filesystem.
module stdlib_io_filesystem
    use stdlib_string_type, only: string_type
    implicit none
    private

    public :: temp_dir, is_windows, exists, list_dir, mkdir, rmdir, run

    character(*), parameter :: temp_dir = 'temp'
    character(*), parameter :: listed_contents = temp_dir//'/listed_contents.txt'

contains

    !> Version: experimental
    !>
    !> Whether the operating system is Windows.
    !> [Specification](../page/specs/stdlib_io.html#is_windows)
    logical function is_windows()
        character(len=255) :: value
        integer :: length, stat

        call get_environment_variable('OSTYPE', value, length, stat)
        if (stat == 0 .and. length > 0 .and. (index(value, 'win') > 0 .or. index(value, 'msys') > 0)) then
            is_windows = .true.; return
        end if

        call get_environment_variable('OS', value, length, stat)
        if (stat == 0 .and. length > 0 .and. index(value, 'Windows_NT') > 0) then
            is_windows = .true.; return
        end if

        is_windows = .false.
    end

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
            call mkdir(temp_dir, stat)
            if (stat /= 0) then
                if (present(iostat)) iostat = stat
                if (present(iomsg)) iomsg = "Failed to create temporary directory '"//temp_dir//"'."
                return
            end if
        end if

        if (is_windows()) then
            call run('dir /b '//dir//' > '//listed_contents, stat)
        else
            call run('ls '//dir//' > '//listed_contents, stat)
        end if
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
    !> Create a directory.
    !> [Specification](../page/specs/stdlib_io.html#mkdir)
    subroutine mkdir(dir, iostat, iomsg)
        character(len=*), intent(in) :: dir
        integer, optional, intent(out) :: iostat
        character(len=:), allocatable, optional, intent(out) :: iomsg

        if (is_windows()) then
            call run('mkdir '//dir, iostat, iomsg)
        else
            call run('mkdir -p '//dir, iostat, iomsg)
        end if
    end

    !> Version: experimental
    !>
    !> Remove a directory including its contents.
    !> [Specification](../page/specs/stdlib_io.html#rmdir)
    subroutine rmdir(dir)
        character(len=*), intent(in) :: dir

        if (is_windows()) then
            call run('rmdir /s/q '//dir)
        else
            call run('rm -rf '//dir)
        end if
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
