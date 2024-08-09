module stdlib_filesystem
    implicit none
    private

    public :: exists, run, temp_folder

    character(*), parameter :: temp_folder = 'temp'

contains

    logical function exists(filename)
        character(len=*), intent(in) :: filename

        inquire(file=filename, exist=exists)

#if defined(__INTEL_COMPILER)
        if (.not. exists) inquire(directory=filename, exist=exists)
#endif
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
