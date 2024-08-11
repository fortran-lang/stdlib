module stdlib_filesystem
    use stdlib_string_type, only: string_type
    implicit none
    private

    public :: exists, list_dir, run, temp_dir

    character(*), parameter :: temp_dir = 'temp'
    character(*), parameter :: listed_contents = temp_dir//'/listed_contents.txt'

contains

    logical function exists(filename)
        character(len=*), intent(in) :: filename

        inquire(file=filename, exist=exists)

#if defined(__INTEL_COMPILER)
        if (.not. exists) inquire(directory=filename, exist=exists)
#endif
    end

    !> List files and directories of a directory. Does not list hidden files.
    subroutine list_dir(dir, files, stat, msg)
        character(len=*), intent(in) :: dir
        type(string_type), allocatable, intent(out) :: files(:)
        integer, intent(out) :: stat
        character(len=:), allocatable, optional, intent(out) :: msg

        integer :: unit, iostat
        character(len=256) :: line

        stat = 0

        if (.not. exists(temp_dir)) then
            call run('mkdir '//temp_dir, stat)
            if (stat /= 0) then
                if (present(msg)) msg = "Failed to create temporary directory '"//temp_dir//"'."; return
            end if
        end if

        call run('ls '//dir//' > '//listed_contents, stat)
        if (stat /= 0) then
            if (present(msg)) then
                msg = "Failed to list files in directory '"//dir//"'."; return
            end if
        end if

        open(newunit=unit, file=listed_contents, status='old', action='read', iostat=stat)
        if (stat /= 0) then
            if (present(msg)) msg = "Failed to open file '"//listed_contents//"'."; return
        end if

        allocate(files(0))
        do
            read(unit, '(A)', iostat=iostat) line
            if (iostat /= 0) exit
            files = [files, string_type(line)]
        end do
        close(unit, status="delete")
    end

    subroutine run(command, stat, msg)
        character(len=*), intent(in) :: command
        integer, intent(out), optional :: stat
        character(len=:), allocatable, intent(out), optional :: msg

        integer :: exitstat, cmdstat
        character(len=256) :: cmdmsg

        if (present(stat)) stat = 0
        exitstat = 0; cmdstat = 0

        call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
        if (exitstat /= 0 .or. cmdstat /= 0) then
            if (present(stat)) then
                if (exitstat /= 0) then
                    stat = exitstat
                else
                    stat = cmdstat
                end if
            end if
            if (present(msg) .and. trim(adjustl(cmdmsg)) /= '') msg = cmdmsg
        end if
    end
end
