! SPDX-Identifer: MIT


!> Description of the npy format taken from
!> https://numpy.org/doc/stable/reference/generated/numpy.lib.format.html
!>
!>## Format Version 1.0
!>
!> The first 6 bytes are a magic string: exactly \x93NUMPY.
!>
!> The next 1 byte is an unsigned byte:
!> the major version number of the file format, e.g. \x01.
!>
!> The next 1 byte is an unsigned byte:
!> the minor version number of the file format, e.g. \x00.
!> Note: the version of the file format is not tied to the version of the numpy package.
!>
!> The next 2 bytes form a little-endian unsigned short int:
!> the length of the header data HEADER_LEN.
!>
!> The next HEADER_LEN bytes form the header data describing the array’s format.
!> It is an ASCII string which contains a Python literal expression of a dictionary.
!> It is terminated by a newline (\n) and padded with spaces (\x20) to make the total
!> of len(magic string) + 2 + len(length) + HEADER_LEN be evenly divisible by 64 for
!> alignment purposes.
!>
!> The dictionary contains three keys:
!>
!> - “descr”: dtype.descr
!>   An object that can be passed as an argument to the numpy.dtype constructor
!>   to create the array’s dtype.
!>
!> - “fortran_order”: bool
!>   Whether the array data is Fortran-contiguous or not. Since Fortran-contiguous
!>   arrays are a common form of non-C-contiguity, we allow them to be written directly
!>   to disk for efficiency.
!>
!> - “shape”: tuple of int
!>   The shape of the array.
!>
!> For repeatability and readability, the dictionary keys are sorted in alphabetic order.
!> This is for convenience only. A writer SHOULD implement this if possible. A reader MUST
!> NOT depend on this.
!>
!> Following the header comes the array data. If the dtype contains Python objects
!> (i.e. dtype.hasobject is True), then the data is a Python pickle of the array.
!> Otherwise the data is the contiguous (either C- or Fortran-, depending on fortran_order)
!> bytes of the array. Consumers can figure out the number of bytes by multiplying the
!> number of elements given by the shape (noting that shape=() means there is 1 element)
!> by dtype.itemsize.
!>
!>## Format Version 2.0
!>
!> The version 1.0 format only allowed the array header to have a total size of 65535 bytes.
!> This can be exceeded by structured arrays with a large number of columns.
!> The version 2.0 format extends the header size to 4 GiB. numpy.save will automatically
!> save in 2.0 format if the data requires it, else it will always use the more compatible
!> 1.0 format.
!>
!> The description of the fourth element of the header therefore has become:
!> “The next 4 bytes form a little-endian unsigned int: the length of the header data
!> HEADER_LEN.”
!>
!>## Format Version 3.0
!>
!> This version replaces the ASCII string (which in practice was latin1) with a
!> utf8-encoded string, so supports structured types with any unicode field names.
module stdlib_io_npy
    use stdlib_kinds, only : int8, int16, int32, int64, sp, dp, xdp, qp
    implicit none
    private

    public :: save_npy, load_npy


    !> Version: experimental
    !>
    !> Save multidimensional array in npy format
    !> ([Specification](../page/specs/stdlib_io.html#save_npy))
    interface save_npy
        module subroutine save_npy_rsp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rsp_1
        module subroutine save_npy_rsp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rsp_2
        module subroutine save_npy_rsp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rsp_3
        module subroutine save_npy_rsp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rsp_4
        module subroutine save_npy_rdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rdp_1
        module subroutine save_npy_rdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rdp_2
        module subroutine save_npy_rdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rdp_3
        module subroutine save_npy_rdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_rdp_4
        module subroutine save_npy_iint8_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint8_1
        module subroutine save_npy_iint8_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint8_2
        module subroutine save_npy_iint8_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint8_3
        module subroutine save_npy_iint8_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint8_4
        module subroutine save_npy_iint16_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint16_1
        module subroutine save_npy_iint16_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint16_2
        module subroutine save_npy_iint16_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint16_3
        module subroutine save_npy_iint16_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint16_4
        module subroutine save_npy_iint32_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint32_1
        module subroutine save_npy_iint32_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint32_2
        module subroutine save_npy_iint32_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint32_3
        module subroutine save_npy_iint32_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint32_4
        module subroutine save_npy_iint64_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint64_1
        module subroutine save_npy_iint64_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint64_2
        module subroutine save_npy_iint64_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint64_3
        module subroutine save_npy_iint64_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_iint64_4
        module subroutine save_npy_csp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_csp_1
        module subroutine save_npy_csp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_csp_2
        module subroutine save_npy_csp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_csp_3
        module subroutine save_npy_csp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_csp_4
        module subroutine save_npy_cdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_cdp_1
        module subroutine save_npy_cdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_cdp_2
        module subroutine save_npy_cdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_cdp_3
        module subroutine save_npy_cdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), intent(in) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine save_npy_cdp_4
    end interface save_npy

    !> Version: experimental
    !>
    !> Load multidimensional array in npy format
    !> ([Specification](../page/specs/stdlib_io.html#load_npy))
    interface load_npy
        module subroutine load_npy_rsp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rsp_1
        module subroutine load_npy_rsp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rsp_2
        module subroutine load_npy_rsp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rsp_3
        module subroutine load_npy_rsp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(sp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rsp_4
        module subroutine load_npy_rdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rdp_1
        module subroutine load_npy_rdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rdp_2
        module subroutine load_npy_rdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rdp_3
        module subroutine load_npy_rdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            real(dp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_rdp_4
        module subroutine load_npy_iint8_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint8_1
        module subroutine load_npy_iint8_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint8_2
        module subroutine load_npy_iint8_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint8_3
        module subroutine load_npy_iint8_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int8), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint8_4
        module subroutine load_npy_iint16_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint16_1
        module subroutine load_npy_iint16_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint16_2
        module subroutine load_npy_iint16_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint16_3
        module subroutine load_npy_iint16_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int16), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint16_4
        module subroutine load_npy_iint32_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint32_1
        module subroutine load_npy_iint32_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint32_2
        module subroutine load_npy_iint32_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint32_3
        module subroutine load_npy_iint32_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int32), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint32_4
        module subroutine load_npy_iint64_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint64_1
        module subroutine load_npy_iint64_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint64_2
        module subroutine load_npy_iint64_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint64_3
        module subroutine load_npy_iint64_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            integer(int64), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_iint64_4
        module subroutine load_npy_csp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_csp_1
        module subroutine load_npy_csp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_csp_2
        module subroutine load_npy_csp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_csp_3
        module subroutine load_npy_csp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(sp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_csp_4
        module subroutine load_npy_cdp_1(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_cdp_1
        module subroutine load_npy_cdp_2(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_cdp_2
        module subroutine load_npy_cdp_3(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_cdp_3
        module subroutine load_npy_cdp_4(filename, array, iostat, iomsg)
            character(len=*), intent(in) :: filename
            complex(dp), allocatable, intent(out) :: array(:,:,:,:)
            integer, intent(out), optional :: iostat
            character(len=:), allocatable, intent(out), optional :: iomsg
        end subroutine load_npy_cdp_4
    end interface load_npy


    character(len=*), parameter :: nl = achar(10)

    character(len=*), parameter :: &
        type_iint8 = "<i1", type_iint16 = "<i2", type_iint32 = "<i4", type_iint64 = "<i8", &
        type_rsp = "<f4", type_rdp = "<f8", type_rxdp = "<f10", type_rqp = "<f16", &
        type_csp = "<c8", type_cdp = "<c16", type_cxdp = "<c20", type_cqp = "<c32"

    character(len=*), parameter :: &
        & magic_number = char(int(z"93")), &
        & magic_string = "NUMPY"


end module stdlib_io_npy
