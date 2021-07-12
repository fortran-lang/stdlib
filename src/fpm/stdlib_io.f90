

module stdlib_io
  !! Provides a support for file handling
  !! ([Specification](../page/specs/stdlib_io.html))

  use stdlib_kinds, only: sp, dp, qp, &
      int8, int16, int32, int64
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  use stdlib_ascii, only: is_blank
  implicit none
  private
  ! Public API
  public :: loadtxt, savetxt, open

  ! Private API that is exposed so that we can test it in tests
  public :: parse_mode

  interface loadtxt
    !! version: experimental
    !!
    !! Loads a 2D array from a text file
    !! ([Specification](../page/specs/stdlib_io.html#description))
      module procedure loadtxt_rsp
      module procedure loadtxt_rdp
      module procedure loadtxt_rqp
      module procedure loadtxt_iint8
      module procedure loadtxt_iint16
      module procedure loadtxt_iint32
      module procedure loadtxt_iint64
      module procedure loadtxt_csp
      module procedure loadtxt_cdp
      module procedure loadtxt_cqp
  end interface loadtxt

  interface savetxt
    !! version: experimental
    !!
    !! Saves a 2D array into a text file
    !! ([Specification](../page/specs/stdlib_io.html#description_2))
      module procedure savetxt_rsp
      module procedure savetxt_rdp
      module procedure savetxt_rqp
      module procedure savetxt_iint8
      module procedure savetxt_iint16
      module procedure savetxt_iint32
      module procedure savetxt_iint64
      module procedure savetxt_csp
      module procedure savetxt_cdp
      module procedure savetxt_cqp
  end interface

contains

    subroutine  loadtxt_rsp(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      real(sp), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(sp), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_rsp
    subroutine  loadtxt_rdp(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      real(dp), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(dp), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_rdp
    subroutine  loadtxt_rqp(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      real(qp), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(qp), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_rqp
    subroutine  loadtxt_iint8(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      integer(int8), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int8), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_iint8
    subroutine  loadtxt_iint16(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      integer(int16), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int16), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_iint16
    subroutine  loadtxt_iint32(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      integer(int32), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int32), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_iint32
    subroutine  loadtxt_iint64(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      integer(int64), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int64), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_iint64
    subroutine  loadtxt_csp(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      complex(sp), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(sp), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_csp
    subroutine  loadtxt_cdp(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      complex(dp), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(dp), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_cdp
    subroutine  loadtxt_cqp(filename, d)
      !! version: experimental
      !!
      !! Loads a 2D array from a text file.
      !!
      !! Arguments
      !! ---------
      !!
      !! Filename to load the array from
      character(len=*), intent(in) :: filename
      !! The array 'd' will be automatically allocated with the correct dimensions
      complex(qp), allocatable, intent(out) :: d(:,:)
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(qp), allocatable :: data(:, :)
      !! call loadtxt("log.txt", data)  ! 'data' will be automatically allocated
      !!```
      !!
      !! Where 'log.txt' contains for example::
      !!
      !!     1 2 3
      !!     2 4 6
      !!     8 9 10
      !!     11 12 13
      !!     ...
      !!
      integer :: s
      integer :: nrow, ncol, i

      s = open(filename)

      ! determine number of columns
      ncol = number_of_columns(s)

      ! determine number or rows
      nrow = number_of_rows_numeric(s)

      allocate(d(nrow, ncol))
      do i = 1, nrow
        read(s, *) d(i, :)
      end do
      close(s)

    end subroutine loadtxt_cqp


    subroutine savetxt_rsp(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      real(sp), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(sp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_rsp
    subroutine savetxt_rdp(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      real(dp), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(dp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_rdp
    subroutine savetxt_rqp(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      real(qp), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(qp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_rqp
    subroutine savetxt_iint8(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int8), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int8) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_iint8
    subroutine savetxt_iint16(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int16), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int16) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_iint16
    subroutine savetxt_iint32(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int32), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int32) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_iint32
    subroutine savetxt_iint64(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int64), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int64) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_iint64
    subroutine savetxt_csp(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      complex(sp), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(sp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_csp
    subroutine savetxt_cdp(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      complex(dp), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(dp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_cdp
    subroutine savetxt_cqp(filename, d)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      complex(qp), intent(in) :: d(:,:)           ! The 2D array to save
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(qp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!

      integer :: s, i
      s = open(filename, "w")
      do i = 1, size(d, 1)
        write(s, *) d(i, :)
      end do
      close(s)
    end subroutine savetxt_cqp


  integer function number_of_columns(s)
    !! version: experimental
    !!
    !! determine number of columns
    integer,intent(in) :: s

    integer :: ios
    character :: c
    logical :: lastblank

    rewind(s)
    number_of_columns = 0
    lastblank = .true.
    do
      read(s, '(a)', advance='no', iostat=ios) c
      if (ios /= 0) exit
      if (lastblank .and. .not. is_blank(c)) number_of_columns = number_of_columns + 1
      lastblank = is_blank(c)
    end do
    rewind(s)

  end function number_of_columns


  integer function number_of_rows_numeric(s) result(nrows)
    !! version: experimental
    !!
    !! determine number or rows
    integer,intent(in)::s
    integer :: ios

    real :: r
    complex :: z

    rewind(s)
    nrows = 0
    do
      read(s, *, iostat=ios) r
      if (ios /= 0) exit
      nrows = nrows + 1
    end do

    rewind(s)

    ! If there are no rows of real numbers, it may be that they are complex
    if( nrows == 0) then
      do
        read(s, *, iostat=ios) z
        if (ios /= 0) exit
        nrows = nrows + 1
      end do
      rewind(s)
    end if
  end function number_of_rows_numeric


  integer function open(filename, mode, iostat) result(u)
    !! version: experimental
    !!
    !! Opens a file
    !! ([Specification](../page/specs/stdlib_io.html#description_1))
    !!
    !!##### Behavior
    !!
    !!
    !! To open a file to read:
    !!
    !!```fortran
    !! u = open("somefile.txt")        ! The default `mode` is "rt"
    !! u = open("somefile.txt", "r")
    !!```
    !!
    !! To open a file to write:
    !!
    !!```fortran
    !! u = open("somefile.txt", "w")
    !!```
    !!
    !! To append to the end of the file if it exists:
    !!
    !!```fortran
    !! u = open("somefile.txt", "a")
    !!```

    character(*), intent(in) :: filename
    character(*), intent(in), optional :: mode
    integer, intent(out), optional :: iostat

    character(3) :: mode_
    character(:),allocatable :: action_, position_, status_, access_, form_


    mode_ = parse_mode(optval(mode, ""))

    select case (mode_(1:2))
    case('r')
      action_='read'
      position_='asis'
      status_='old'
    case('w')
      action_='write'
      position_='asis'
      status_='replace'
    case('a')
      action_='write'
      position_='append'
      status_='old'
    case('x')
      action_='write'
      position_='asis'
      status_='new'
    case('r+')
      action_='readwrite'
      position_='asis'
      status_='old'
    case('w+')
      action_='readwrite'
      position_='asis'
      status_='replace'
    case('a+')
      action_='readwrite'
      position_='append'
      status_='old'
    case('x+')
      action_='readwrite'
      position_='asis'
      status_='new'
    case default
      call error_stop("Unsupported mode: "//mode_(1:2))
    end select

    select case (mode_(3:3))
    case('t')
      form_='formatted'
    case('b')
      form_='unformatted'
    case default
      call error_stop("Unsupported mode: "//mode_(3:3))
    end select

    access_ = 'stream'

    if (present(iostat)) then
      open(newunit=u, file=filename, &
          action = action_, position = position_, status = status_, &
          access = access_, form = form_, &
          iostat = iostat)
    else
      open(newunit=u, file=filename, &
          action = action_, position = position_, status = status_, &
          access = access_, form = form_)
    end if

  end function open

  character(3) function parse_mode(mode) result(mode_)
    character(*), intent(in) :: mode

    integer :: i
    character(:),allocatable :: a
    logical :: lfirst(3)

    mode_ = 'r t'

    if (len_trim(mode) == 0) return
    a=trim(adjustl(mode))

    lfirst = .true.
    do i=1,len(a)
      if (lfirst(1) &
          .and. (a(i:i) == 'r' .or. a(i:i) == 'w' .or. a(i:i) == 'a' .or. a(i:i) == 'x') &
          ) then
        mode_(1:1) = a(i:i)
        lfirst(1)=.false.
      else if (lfirst(2) .and. a(i:i) == '+') then
        mode_(2:2) = a(i:i)
        lfirst(2)=.false.
      else if (lfirst(3) .and. (a(i:i) == 't' .or. a(i:i) == 'b')) then
        mode_(3:3) = a(i:i)
        lfirst(3)=.false.
      else if (a(i:i) == ' ') then
        cycle
      else if(any(.not.lfirst)) then
        call error_stop("Wrong mode: "//trim(a))
      else
        call error_stop("Wrong character: "//a(i:i))
      endif
    end do

  end function parse_mode

end module
