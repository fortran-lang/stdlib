module stdlib_io_zip
    implicit none
    private

    public :: unzip

    character(*), parameter :: temp_folder = 'temp'
    character(*), parameter :: zip_contents = temp_folder//'/zip_contents'

    !> Contains extracted raw data from a zip file.
    type, public :: t_unzipped_bundle
        !> The raw data of the files within the zip file.
        type(t_unzipped_file), allocatable :: files(:)
    end type

    !> Contains the name of the file and its raw data.
    type, public :: t_unzipped_file
        !> The name of the file.
        character(:), allocatable :: name
        !> The raw data of the file.
        character(:), allocatable :: data
    end type

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

    subroutine unzip(filename, outputdir, stat, msg)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: outputdir
        integer, intent(out), optional :: stat
        character(len=:), allocatable, intent(out), optional :: msg

        integer :: run_stat
        character(:), allocatable :: err_msg
        character(:), allocatable :: output_dir

        if (present(outputdir)) then
            output_dir = outputdir
        else
            output_dir = zip_contents
        end if

        if (present(stat)) stat = 0
        run_stat = 0

        call run('rm -rf '//zip_contents, run_stat, err_msg)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) then
                if (allocated(err_msg)) then
                    msg = "Error removing folder '"//zip_contents//"': '"//err_msg//"'"
                else
                    msg = "Error removing folder '"//zip_contents//"'."
                end if
            end if
            return
        end if

        if (.not. exists(temp_folder)) then
            call run('mkdir '//temp_folder, run_stat, err_msg)
            if (run_stat /= 0) then
                if (present(stat)) stat = run_stat
                if (present(msg)) then
                    if (allocated(err_msg)) then
                        msg = "Error creating folder '"//temp_folder//"': '"//err_msg//"'"
                    else
                        msg = "Error creating folder '"//temp_folder//"'."
                    end if
                end if
                return
            end if
        end if

        call run('unzip '//filename//' -d '//zip_contents, run_stat, err_msg)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) then
                if (allocated(err_msg)) then
                    msg = "Error unzipping '"//filename//"': '"//err_msg//"'"
                else
                    msg = "Error unzipping '"//filename//"'."
                end if
            end if
        end if
    end
end
