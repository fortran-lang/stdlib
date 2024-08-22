! SPDX-Identifier: MIT

!> Handling of zip files including creation and extraction.
module stdlib_io_zip
    use stdlib_io_filesystem, only: exists, run, temp_dir
    use stdlib_string_type, only: string_type, char
    use stdlib_strings, only: starts_with
    implicit none
    private

    public :: zip, unzip, default_unzip_dir, zip_contents

    character(*), parameter :: default_unzip_dir = temp_dir//'/unzipped_files'
    character(*), parameter :: zip_contents = default_unzip_dir//'/zip_contents.txt'

contains

    !> Version: experimental
    !>
    !> Create a zip file from a list of files.
    subroutine zip(output_file, files, stat, msg, compressed)
        !> Name of the zip file to create.
        character(*), intent(in) :: output_file
        !> List of files to include in the zip file.
        type(string_type), intent(in) :: files(:)
        !> Optional error status of zipping, zero on success.
        integer, intent(out), optional :: stat
        !> Optional error message.
        character(len=:), allocatable, intent(out), optional :: msg
        !> If true, the file is saved in compressed format. The default is true.
        logical, intent(in), optional :: compressed

        integer :: run_stat, i
        character(:), allocatable :: files_str, cmd
        logical :: is_compressed

        if (present(stat)) stat = 0
        run_stat = 0

        if (present(compressed)) then
            is_compressed = compressed
        else
            is_compressed = .true.
        end if

        if (trim(output_file) == '') then
            if (present(stat)) stat = 1
            if (present(msg)) msg = "Output file is empty."
            return
        end if

        files_str = ''
        do i = 1, size(files)
            files_str = files_str//' '//char(files(i))
        end do

        cmd = 'zip -q '//''//output_file//' '//files_str
        if (.not. is_compressed) cmd = cmd//' -0'

        call run(cmd, run_stat)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) msg = "Error creating zip file '"//output_file//"'."
            return
        end if
    end

    !> Version: experimental
    !>
    !> Extract a zip file to a directory.
    subroutine unzip(filename, outputdir, stat, msg)
        !> Name of the zip file to extract.
        character(len=*), intent(in) :: filename
        !> Directory to extract the zip file to.
        character(len=*), intent(in), optional :: outputdir
        !> Optional error status of unzipping, zero on success.
        integer, intent(out), optional :: stat
        !> Optional error message.
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

        call run('rm -rf '//output_dir, run_stat)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) msg = "Error removing folder '"//output_dir//"'."
            return
        end if

        if (starts_with(output_dir, temp_dir) .and. .not. exists(temp_dir)) then
            call run('mkdir '//temp_dir, run_stat)
            if (run_stat /= 0) then
                if (present(stat)) stat = run_stat
                if (present(msg)) msg = "Error creating folder '"//temp_dir//"'."
                return
            end if
        end if

        call run('unzip -q '//filename//' -d '//output_dir, run_stat)
        if (run_stat /= 0) then
            if (present(stat)) stat = run_stat
            if (present(msg)) msg = "Error unzipping '"//filename//"'."
            return
        end if
    end
end
