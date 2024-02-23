module stdlib_io_zip
    use stdlib_io_minizip
    use iso_c_binding, only: c_ptr, c_associated, c_int, c_long, c_char
    implicit none
    private

    public :: unzip, zip_prefix, zip_suffix

    character(*), parameter :: zip_prefix = 'PK'//achar(3)//achar(4)
    character(*), parameter :: zip_suffix = 'PK'//achar(5)//achar(6)
    integer(kind=c_int), parameter :: read_buffer_size = 1024
    integer(kind=c_long), parameter :: buffer_size = 1024

    interface unzip
        module procedure unzip_to_bundle
    end interface

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

    module subroutine unzip_to_bundle(filename, bundle, iostat, iomsg)
        character(len=*), intent(in) :: filename
        type(t_unzipped_bundle), intent(out) :: bundle
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        type(c_ptr) :: file_handle
        type(unz_global_info) :: global_info
        type(unz_file_info) :: file_info
        integer(kind=c_int) :: stat, bytes_read
        character(kind=c_char, len=read_buffer_size) :: read_buffer
        character(kind=c_char, len=buffer_size) :: file_name, extra_field, comment
        integer(kind=c_long) :: i

        if (present(iostat)) iostat = 0

        file_handle = unz_open(filename)
        if (.not. c_associated(file_handle)) then
            if (present(iostat)) iostat = 1
            if (present(iomsg)) iomsg = 'Failed to open file '//trim(filename)//'.'
            return
        end if

        stat = unz_get_global_info(file_handle, global_info)
        if (stat /= UNZ_OK) then
            if (present(iostat)) iostat = stat
            if (present(iomsg)) iomsg = 'Failed to get global info for '//trim(filename)//'.'
            return
        end if

        allocate (bundle%files(global_info%number_of_files))

        read_files: block
            if (size(bundle%files) == 0) exit read_files

            stat = unz_go_to_first_file(file_handle)
            if (stat /= UNZ_OK) then
                if (present(iostat)) iostat = stat
                if (present(iomsg)) iomsg = 'Failed to go to first file in '//trim(filename)//'.'
                stat = unz_close(file_handle); return
            end if

            do i = 1, global_info%number_of_files
                stat = unz_open_current_file(file_handle)
                if (stat /= UNZ_OK) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = 'Error opening file within '//trim(filename)//'.'
                    stat = unz_close(file_handle); return
                end if

                stat = unz_get_current_file_info(file_handle, file_info, file_name, buffer_size, &
                                                 extra_field, buffer_size, comment, buffer_size)
                if (stat /= UNZ_OK) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = 'Failed to get current file info in '//trim(filename)//'.'
                    stat = unz_close(file_handle); return
                end if

                bundle%files(i)%name = file_name(1:file_info%size_filename)
                bundle%files(i)%data = ''

                do
                    bytes_read = unz_read_current_file(file_handle, read_buffer, read_buffer_size)
                    if (bytes_read < 0) then
                        if (present(iostat)) iostat = bytes_read
                        if (present(iomsg)) iomsg = 'Error reading file within '//trim(filename)//'.'
                        stat = unz_close_current_file(file_handle); 
                        stat = unz_close(file_handle); 
                        return
                    else if (bytes_read == 0) then
                        stat = unz_close_current_file(file_handle)
                        if (stat /= UNZ_OK) then
                            if (present(iostat)) iostat = stat
                            if (present(iomsg)) iomsg = 'Error closing file within '//trim(filename)//'.'
                            stat = unz_close(file_handle); return
                        end if
                        exit
                    else
                        bundle%files(i)%data = bundle%files(i)%data//read_buffer(1:bytes_read)
                    end if
                end do

                if (i == global_info%number_of_files) exit
                stat = unz_go_to_next_file(file_handle)
                if (stat /= UNZ_OK) then
                    if (present(iostat)) iostat = stat
                    if (present(iomsg)) iomsg = 'Failed to go to next file within '//trim(filename)//'.'
                    stat = unz_close(file_handle); return
                end if
            end do
        end block read_files

        stat = unz_close(file_handle)
        if (stat /= UNZ_OK) then
            if (present(iostat)) iostat = stat
            if (present(iomsg)) iomsg = 'Failed to close file '//trim(filename)//'.'
            return
        end if
    end
end
