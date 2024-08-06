module stdlib_io_zip
    implicit none
    private

    public :: unzip

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

    subroutine unzip(filename, iostat, iomsg)
        character(len=*), intent(in) :: filename
        integer, intent(out), optional :: iostat
        character(len=:), allocatable, intent(out), optional :: iomsg

        integer :: exitstat, cmdstat
        character(len=256) :: cmdmsg

        exitstat = 0; cmdstat = 0

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
                if (trim(adjustl(cmdmsg)) == '') then
                    iomsg = "Error unzipping '"//filename//"'."
                else
                    iomsg = "Error unzipping '"//filename//"'"//": '"//cmdmsg//"'"
                end if
            end if
        end if
    end
end
