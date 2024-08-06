module stdlib_io_zip
    implicit none
    private

    public :: unzip, zip_prefix, zip_suffix

    character(*), parameter :: zip_prefix = 'PK'//achar(3)//achar(4)
    character(*), parameter :: zip_suffix = 'PK'//achar(5)//achar(6)

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

    subroutine unzip(filename, output_dir, iostat, iomsg)
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: output_dir
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: exitstat, cmdstat
        character(:), allocatable :: cmdmsg

        exitstat = 0; cmdstat = 0

        ! call execute_command_line('unzip '//filename//' -d '//output_dir, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
        call execute_command_line('unzip '//filename, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
        if (exitstat /= 0 .or. cmdstat /= 0) then
            if (present(iostat)) then
                if (exitstat /= 0) then
                    iostat = exitstat
                else
                    iostat = cmdstat
                end if
            end if
            if (present(iomsg)) then
                if (allocated(cmdmsg)) then
                    iomsg = "Error unzipping '"//filename//"'"//": '"//cmdmsg//"'"
                else
                    iomsg = "Error unzipping '"//filename//"'."
                end if
            end if
        end if
    end
end
