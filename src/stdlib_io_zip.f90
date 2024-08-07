module stdlib_io_zip
    implicit none
    private

    public :: list_files_in_zip, unzip

    character(*), parameter :: temp_folder = 'temp'
    character(*), parameter :: zip_contents_file = temp_folder//'/zip_contents.txt'

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
        inquire (file=filename, exist=exists)

#if defined(__INTEL_COMPILER)
        if (.not.exists) inquire (directory=filename, exist=exists)
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

    subroutine list_files_in_zip(filename, stat, msg)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: stat
        character(len=:), allocatable, intent(out), optional :: msg

        integer :: run_stat
        character(:), allocatable :: err_msg

        if (present(stat)) stat = 0
        run_stat = 0

        call run('rm -f '//zip_contents_file, run_stat, err_msg)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) then
                if (allocated(err_msg)) then
                    msg = "Error removing file '"//zip_contents_file//"': '"//err_msg//"'"
                else
                    msg = "Error removing file '"//zip_contents_file//"'."
                end if
            end if
            return
        end if

        if (.not. exists(temp_folder)) call run('mkdir '//temp_folder, run_stat, err_msg)
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

        call run('unzip -l '//filename//' > '//zip_contents_file, run_stat, err_msg)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) then
                if (allocated(err_msg)) then
                    msg = "Error listing contents of '"//filename//"': '"//err_msg//"'"
                else
                    msg = "Error listing contents of '"//filename//"'."
                end if
            end if
        end if
    end

    subroutine unzip(filename, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: stat
        character(:), allocatable :: msg

        if (present(iostat)) iostat = 0
        stat = 0

        call run('unzip '//filename, stat, msg)
        if (stat /= 0) then
            if (present(iostat)) iostat = stat
            if (present(iomsg)) then
                if (allocated(msg)) then
                    iomsg = "Error unzipping '"//filename//"': '"//msg//"'"
                else
                    iomsg = "Error unzipping '"//filename//"'."
                end if
            end if
        end if
    end
end
