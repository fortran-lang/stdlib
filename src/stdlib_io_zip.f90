module stdlib_io_zip
    use stdlib_filesystem, only: exists, run, temp_dir
    implicit none
    private

    public :: unzip, unzipped_folder, zip_contents

    character(*), parameter :: unzipped_folder = temp_dir//'/unzipped_files'
    character(*), parameter :: zip_contents = unzipped_folder//'/zip_contents.txt'

contains

    subroutine unzip(filename, outputdir, stat, msg)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: outputdir
        integer, intent(out), optional :: stat
        character(len=:), allocatable, intent(out), optional :: msg

        integer :: run_stat
        character(:), allocatable :: output_dir

        if (present(outputdir)) then
            output_dir = outputdir
        else
            output_dir = unzipped_folder
        end if

        if (present(stat)) stat = 0
        run_stat = 0

        call run('rm -rf '//unzipped_folder, run_stat)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) msg = "Error removing folder '"//unzipped_folder//"'."
            return
        end if

        if (.not. exists(temp_dir)) then
            call run('mkdir '//temp_dir, run_stat)
            if (run_stat /= 0) then
                if (present(stat)) stat = run_stat
                if (present(msg)) msg = "Error creating folder '"//temp_dir//"'."
                return
            end if
        end if

        call run('unzip '//filename//' -d '//unzipped_folder, run_stat)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) msg = "Error unzipping '"//filename//"'."
            return
        end if
    end
end
