module stdlib_io_zip
    implicit none
    private

    public :: unzip, zip_prefix, zip_suffix

    character(*), parameter :: zip_prefix = 'PK'//achar(3)//achar(4)
    character(*), parameter :: zip_suffix = 'PK'//achar(5)//achar(6)

    interface unzip
        procedure unzip_to_bundle
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

    subroutine unzip_to_bundle(filename, bundle, iostat, iomsg)
        character(len=*), intent(in) :: filename
        type(t_unzipped_bundle), intent(out) :: bundle
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        if (present(iostat)) iostat = 0
    end
end
