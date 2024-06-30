!> Interface to the minizip-ng library for creating and extracting zip files.
!>
!> https://github.com/zlib-ng/minizip-ng
module stdlib_io_minizip
    use, intrinsic :: iso_c_binding, only: c_char, c_ptr, c_int, c_long
    implicit none
    private

    integer, parameter, public :: UNZ_OK = 0
    integer, parameter, public :: UNZ_END_OF_LIST_OF_FILE = -100
    integer, parameter, public :: UNZ_ERRNO = -1
    integer, parameter, public :: UNZ_EOF = 0
    integer, parameter, public :: UNZ_PARAMERROR = -102
    integer, parameter, public :: UNZ_BADZIPFILE = -103
    integer, parameter, public :: UNZ_INTERNALERROR = -104
    integer, parameter, public :: UNZ_CRCERROR = -105
    integer, parameter, public :: UNZ_BADPASSWORD = -106

    public :: unz_get_global_info
    public :: unz_open
    public :: unz_go_to_first_file
    public :: unz_get_current_file_info
    public :: unz_open_current_file
    public :: unz_read_current_file
    public :: unz_close_current_file
    public :: unz_go_to_next_file
    public :: unz_close

    type, bind(c), public :: unz_global_info
        integer(kind=c_long) :: number_of_files
        integer(kind=c_long) :: comment_size
    end type

    type, bind(c), public :: unz_file_info
        integer(kind=c_long) :: version
        integer(kind=c_long) :: version_needed
        integer(kind=c_long) :: flag
        integer(kind=c_long) :: compression_method
        integer(kind=c_long) :: dos_date
        integer(kind=c_long) :: crc
        integer(kind=c_long) :: compressed_size
        integer(kind=c_long) :: uncompressed_size
        integer(kind=c_long) :: size_filename
        integer(kind=c_long) :: size_file_extra
        integer(kind=c_long) :: size_file_comment
        integer(kind=c_long) :: disk_num_start
        integer(kind=c_long) :: internal_file_attributes
        integer(kind=c_long) :: external_file_attributes
    end type

    interface
        function unz_open(path) bind(c, name='unzOpen')
            import :: c_char, c_ptr
            implicit none
            character(kind=c_char), intent(in) :: path
            type(c_ptr) :: unz_open
        end

        function unz_get_global_info(file, global_info) bind(c, name='unzGetGlobalInfo')
            import :: c_ptr, c_int, unz_global_info
            implicit none
            type(c_ptr), intent(in), value :: file
            type(unz_global_info), intent(out) :: global_info
            integer(kind=c_int) :: unz_get_global_info
        end

        function unz_go_to_first_file(file) bind(c, name='unzGoToFirstFile')
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: file
            integer(kind=c_int) :: unz_go_to_first_file
        end

        function unz_get_current_file_info(file, file_info, filename, filename_buffer_size, &
            & extra_field, extra_field_buffer_size, comment, comment_buffer_size) &
            & bind(c, name='unzGetCurrentFileInfo')
            import :: c_ptr, c_int, c_char, c_long, unz_file_info
            implicit none
            type(c_ptr), intent(in), value :: file
            type(unz_file_info), intent(out) :: file_info
            character(kind=c_char), intent(out) :: filename(*)
            integer(kind=c_long), intent(in), value :: filename_buffer_size
            character(kind=c_char), intent(out) :: extra_field(*)
            integer(kind=c_long), intent(in), value :: extra_field_buffer_size
            character(kind=c_char), intent(out) :: comment(*)
            integer(kind=c_long), intent(in), value :: comment_buffer_size
            integer(kind=c_int) :: unz_get_current_file_info
        end

        function unz_open_current_file(file) bind(c, name='unzOpenCurrentFile')
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: file
            integer(kind=c_int) :: unz_open_current_file
        end

        function unz_read_current_file(file, buffer, size) bind(c, name='unzReadCurrentFile')
            import :: c_ptr, c_int, c_char
            implicit none
            type(c_ptr), intent(in), value :: file
            character(kind=c_char), intent(out) :: buffer(*)
            integer(kind=c_int), intent(in), value :: size
            integer(kind=c_int) :: unz_read_current_file
        end

        function unz_go_to_next_file(file) bind(c, name='unzGoToNextFile')
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: file
            integer(kind=c_int) :: unz_go_to_next_file
        end

        function unz_close_current_file(file) bind(c, name='unzCloseCurrentFile')
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: file
            integer(kind=c_int) :: unz_close_current_file
        end

        function unz_close(file) bind(c, name='unzClose')
            import :: c_ptr, c_int
            implicit none
            type(c_ptr), intent(in), value :: file
            integer(kind=c_int) :: unz_close
        end
    end interface
end
