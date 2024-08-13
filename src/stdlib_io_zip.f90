module stdlib_io_zip
    use stdlib_filesystem, only: exists, run, temp_dir
    implicit none
    private

    public :: unzip, default_unzip_dir, zip_contents

    character(*), parameter :: default_unzip_dir = temp_dir//'/unzipped_files'
    character(*), parameter :: zip_contents = default_unzip_dir//'/zip_contents.txt'

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
            output_dir = default_unzip_dir
        end if

        if (present(stat)) stat = 0
        run_stat = 0

        call run('rm -rf '//default_unzip_dir, run_stat)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) msg = "Error removing folder '"//default_unzip_dir//"'."
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

        call run('unzip '//filename//' -d '//output_dir, run_stat)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) msg = "Error unzipping '"//filename//"'."
            return
        end if
    end
end
