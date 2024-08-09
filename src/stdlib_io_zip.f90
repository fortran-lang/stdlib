module stdlib_io_zip
    use stdlib_filesystem, only: exists, run, temp_folder
    implicit none
    private

    public :: unzip, unzipped_folder

    character(*), parameter :: unzipped_folder = temp_folder//'/unzipped_files'

contains

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
            output_dir = unzipped_folder
        end if

        if (present(stat)) stat = 0
        run_stat = 0

        call run('rm -rf '//unzipped_folder, run_stat, err_msg)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) then
                if (allocated(err_msg)) then
                    msg = "Error removing folder '"//unzipped_folder//"': '"//err_msg//"'"
                else
                    msg = "Error removing folder '"//unzipped_folder//"'."
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

        call run('unzip '//filename//' -d '//unzipped_folder, run_stat, err_msg)
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
