module stdlib_io_zip
    use stdlib_filesystem, only: exists, run, temp_dir
    use stdlib_string_type, only: string_type, char
    use stdlib_strings, only: starts_with
    implicit none
    private

    public :: zip, unzip, default_unzip_dir, zip_contents

    character(*), parameter :: default_unzip_dir = temp_dir//'/unzipped_files'
    character(*), parameter :: zip_contents = default_unzip_dir//'/zip_contents.txt'

contains

    subroutine zip(output_file, files, stat, msg, compressed)
        character(*), intent(in) :: output_file
        type(string_type), intent(in) :: files(:)
        integer, intent(out), optional :: stat
        character(len=:), allocatable, intent(out), optional :: msg
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

        if (.not. exists(filename)) then
            if (present(stat)) stat = 1
            if (present(msg)) msg = "File '"//filename//"' does not exist."
            return
        end if

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
