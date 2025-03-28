

module stdlib_io
  !! Provides a support for file handling
  !! ([Specification](../page/specs/stdlib_io.html))

  use, intrinsic :: iso_fortran_env, only : input_unit
  use stdlib_kinds, only: sp, dp, xdp, qp, &
      int8, int16, int32, int64
  use stdlib_error, only: error_stop, state_type, STDLIB_IO_ERROR
  use stdlib_optval, only: optval
  use stdlib_ascii, only: is_blank
  use stdlib_string_type, only : string_type, assignment(=), move
  implicit none
  private
  ! Public API
  public :: loadtxt, savetxt, open, get_line, get_file

  !! version: experimental 
  !!
  !! Reads a whole ASCII file and loads its contents into a string variable. 
  !! ([Specification](../page/specs/stdlib_io.html#get-file-read-a-whole-ascii-file-into-a-character-or-a-string-variable))
  !! 
  !!### Summary 
  !! Subroutine interface for reading the content of a file into a string.
  !!
  !!### Description
  !! 
  !! This subroutine reads the entirety of a specified ASCII file and returns it as a string. The optional 
  !! `err` argument allows for handling errors through the library's `state_type` class. 
  !! An optional `logical` flag can be passed to delete the file after reading.  
  !! 
  !!@note Handles errors using the library's `state_type` error-handling class. If not provided, 
  !! exceptions will trigger an `error stop`. 
  !!         
  interface get_file
    module procedure :: get_file_char
    module procedure :: get_file_string
  end interface get_file

  ! Private API that is exposed so that we can test it in tests
  public :: parse_mode

  !> Version: experimental
  !>
  !> Format strings with edit descriptors for each type and kind
  !> ([Specification](../page/specs/stdlib_io.html))
  character(*), parameter :: &
    !> Format string for integers
    FMT_INT = '(i0)', &
    !> Format string for single precision real numbers
    FMT_REAL_SP = '(es15.8e2)', &
    !> Format string for souble precision real numbers
    FMT_REAL_DP = '(es24.16e3)', &
    !> Format string for extended double precision real numbers
    FMT_REAL_XDP = '(es26.18e3)', &
    !> Format string for quadruple precision real numbers
    FMT_REAL_QP = '(es44.35e4)', &
    !> Format string for single precision complex numbers
    FMT_COMPLEX_SP = '(es15.08e2,1x,es15.08e2)', &
    !> Format string for double precision complex numbers
    FMT_COMPLEX_DP = '(es24.16e3,1x,es24.16e3)', &
    !> Format string for extended double precision complex numbers
    FMT_COMPLEX_XDP = '(es26.18e3,1x,es26.18e3)', &
    !> Format string for quadruple precision complex numbers
    FMT_COMPLEX_QP = '(es44.35e4,1x,es44.35e4)'
  !> Default delimiter for loadtxt, savetxt and number_of_columns
  character(len=1), parameter :: delimiter_default = " "

  public :: FMT_INT, FMT_REAL_SP, FMT_REAL_DP, FMT_REAL_XDP, FMT_REAL_QP
  public :: FMT_COMPLEX_SP, FMT_COMPLEX_DP, FMT_COMPLEX_XDP, FMT_COMPLEX_QP

  !> Version: experimental
  !>
  !> Read a whole line from a formatted unit into a string variable
  interface get_line
    module procedure :: get_line_char
    module procedure :: get_line_string
    module procedure :: get_line_input_char
    module procedure :: get_line_input_string
  end interface get_line

  interface loadtxt
    !! version: experimental
    !!
    !! Loads a 2D array from a text file
    !! ([Specification](../page/specs/stdlib_io.html#description))
      module procedure loadtxt_rsp
      module procedure loadtxt_rdp
      module procedure loadtxt_iint8
      module procedure loadtxt_iint16
      module procedure loadtxt_iint32
      module procedure loadtxt_iint64
      module procedure loadtxt_csp
      module procedure loadtxt_cdp
  end interface loadtxt

  interface savetxt
    !! version: experimental
    !!
    !! Saves a 2D array into a text file
    !! ([Specification](../page/specs/stdlib_io.html#description_2))
      module procedure savetxt_rsp
      module procedure savetxt_rdp
      module procedure savetxt_iint8
      module procedure savetxt_iint16
      module procedure savetxt_iint32
      module procedure savetxt_iint64
      module procedure savetxt_csp
      module procedure savetxt_cdp
  end interface

contains

    subroutine  loadtxt_rsp(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "(*"//FMT_REAL_sp(1:len(FMT_REAL_sp)-1)//",:,1x))")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_rsp
    subroutine  loadtxt_rdp(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "(*"//FMT_REAL_dp(1:len(FMT_REAL_dp)-1)//",:,1x))")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_rdp
    subroutine  loadtxt_iint8(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "*")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_iint8
    subroutine  loadtxt_iint16(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "*")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_iint16
    subroutine  loadtxt_iint32(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "*")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_iint32
    subroutine  loadtxt_iint64(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "*")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_iint64
    subroutine  loadtxt_csp(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)
      ncol = ncol / 2

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "(*"//FMT_COMPLEX_sp(1:len(FMT_COMPLEX_sp)-1)//",:,1x))")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_csp
    subroutine  loadtxt_cdp(filename, d, skiprows, max_rows, fmt, delimiter)
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
      !! Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.
      integer, intent(in), optional :: skiprows
      !! Read `max_rows` lines of content after `skiprows` lines.
      !! A negative value results in reading all lines.
      !! A value of zero results in no lines to be read.
      !! The default value is -1.
      integer, intent(in), optional :: max_rows
      character(len=*), intent(in), optional :: fmt
      character(len=1), intent(in), optional :: delimiter
      character(len=:), allocatable :: fmt_
      character(len=1) :: delimiter_
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
      integer :: nrow, ncol, i, j, ios, skiprows_, max_rows_, istart, iend
      character(len=:), allocatable :: line, iomsg_
      character(len=1024) :: iomsg, msgout

      skiprows_ = max(optval(skiprows, 0), 0)
      max_rows_ = optval(max_rows, -1)
      delimiter_ = optval(delimiter, delimiter_default)

      s = open(filename)

      ! determine number or rows
      nrow = number_of_rows(s)
      skiprows_ = min(skiprows_, nrow)
      if ( max_rows_ < 0 .or. max_rows_ > (nrow - skiprows_) ) max_rows_ = nrow - skiprows_

      ! determine number of columns
      ncol = 0
      if ( skiprows_ < nrow ) ncol = number_of_columns(s, skiprows=skiprows_, delimiter=delimiter_)
      ncol = ncol / 2

      allocate(d(max_rows_, ncol))
      if (max_rows_ == 0 .or. ncol == 0) return

      do i = 1, skiprows_
        read(s, *, iostat=ios, iomsg=iomsg)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),i,trim(filename) 
           1 format('loadtxt: error <',a,'> skipping line ',i0,' of ',a,'.')
           call error_stop(msg=trim(msgout))
        end if
        
      end do
      
      ! Default to format used for savetxt if fmt not specified.
      fmt_ = optval(fmt, "(*"//FMT_COMPLEX_dp(1:len(FMT_COMPLEX_dp)-1)//",:,1x))")

      if ( fmt_ == '*' ) then
        ! Use list directed read if user has specified fmt='*'
        if (is_blank(delimiter_) .or. delimiter_ == ",") then
          do i = 1, max_rows_
            read (s,*,iostat=ios,iomsg=iomsg) d(i, :)
            
            if (ios/=0) then 
              write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
              call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        ! Otherwise read each value separately
        else
          do i = 1, max_rows_
            call get_line(s, line, ios, iomsg_)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg_),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if
  
            istart = 0
            do j = 1, ncol - 1
              iend = index(line(istart+1:), delimiter_)
              read (line(istart+1:istart+iend-1),*,iostat=ios,iomsg=iomsg) d(i, j)
              if (ios/=0) then 
                 write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
                 call error_stop(msg=trim(msgout))
              end if
              istart = istart + iend
            end do
  
            read (line(istart+1:),*,iostat=ios,iomsg=iomsg) d(i, ncol)
            if (ios/=0) then 
               write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
               call error_stop(msg=trim(msgout))
            end if          
            
          enddo
        end if
      else
        ! Otherwise pass default or user specified fmt string.
        do i = 1, max_rows_
          read (s,fmt_,iostat=ios,iomsg=iomsg) d(i, :)
          
          if (ios/=0) then 
             write(msgout,2) trim(iomsg),size(d,2),i,trim(filename)
             call error_stop(msg=trim(msgout))
          end if             
          
        enddo
      endif

      close(s)
      
      2 format('loadtxt: error <',a,'> reading ',i0,' values from line ',i0,' of ',a,'.')

    end subroutine loadtxt_cdp


    subroutine savetxt_rsp(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      real(sp), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(sp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_REAL_sp(1:len(FMT_REAL_sp)-1)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_rsp
    subroutine savetxt_rdp(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      real(dp), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! real(dp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_REAL_dp(1:len(FMT_REAL_dp)-1)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_rdp
    subroutine savetxt_iint8(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int8), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int8) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_INT(1:len(FMT_INT)-1)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_iint8
    subroutine savetxt_iint16(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int16), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int16) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_INT(1:len(FMT_INT)-1)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_iint16
    subroutine savetxt_iint32(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int32), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int32) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_INT(1:len(FMT_INT)-1)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_iint32
    subroutine savetxt_iint64(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      integer(int64), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! integer(int64) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_INT(1:len(FMT_INT)-1)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_iint64
    subroutine savetxt_csp(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      complex(sp), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(sp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_COMPLEX_sp(1:11)//delim_str//FMT_COMPLEX_sp(14:23)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_csp
    subroutine savetxt_cdp(filename, d, delimiter)
      !! version: experimental
      !!
      !! Saves a 2D array into a text file.
      !!
      !! Arguments
      !! ---------
      !!
      character(len=*), intent(in) :: filename  ! File to save the array to
      complex(dp), intent(in) :: d(:,:)           ! The 2D array to save
      character(len=1), intent(in), optional :: delimiter  ! Column delimiter. Default is a space.
      !!
      !! Example
      !! -------
      !!
      !!```fortran
      !! complex(dp) :: data(3, 2)
      !! call savetxt("log.txt", data)
      !!```
      !!
      integer :: s, i, ios
      character(len=1) :: delimiter_
      character(len=3) :: delim_str
      character(len=:), allocatable :: fmt_
      character(len=1024) :: iomsg, msgout

      delimiter_ = optval(delimiter, delimiter_default)
      delim_str = "'"//delimiter_//"'"
        fmt_ = "(*"//FMT_COMPLEX_dp(1:11)//delim_str//FMT_COMPLEX_dp(14:23)//",:,"//delim_str//"))"

      s = open(filename, "w")
      do i = 1, size(d, 1)
          write(s, fmt_, &
                iostat=ios,iomsg=iomsg) d(i, :)
        
        if (ios/=0) then 
           write(msgout,1) trim(iomsg),size(d,2),i,trim(filename) 
           call error_stop(msg=trim(msgout))
        end if           
        
      end do
      close(s)
      
      1 format('savetxt: error <',a,'> writing ',i0,' values to line ',i0,' of ',a,'.')
      
    end subroutine savetxt_cdp


  integer function number_of_columns(s, skiprows, delimiter)
    !! version: experimental
    !!
    !! determine number of columns
    integer,intent(in) :: s
    integer, intent(in), optional :: skiprows
    character(len=1), intent(in), optional :: delimiter

    integer :: ios, skiprows_, i
    character :: c
    character(len=:), allocatable :: line
    character(len=1) :: delimiter_
    logical :: last_delim

    skiprows_ = optval(skiprows, 0)
    delimiter_ = optval(delimiter, delimiter_default)

    rewind(s)

    do i = 1, skiprows_
      read(s, *)
    end do
    number_of_columns = 0
    
    ! Read first non-skipped line as a whole
    call get_line(s, line, ios)
    if (ios/=0 .or. .not.allocated(line)) return

    last_delim = .true.
    if (delimiter_ == delimiter_default) then
      do i = 1,len(line)
        c = line(i:i)
        if (last_delim .and. .not. is_blank(c)) number_of_columns = number_of_columns + 1
        last_delim = is_blank(c)
      end do
    else
      do i = 1,len(line)
        if (line(i:i) == delimiter_) number_of_columns = number_of_columns + 1
      end do
      if (number_of_columns == 0) then
        if (len_trim(line) /= 0) number_of_columns = 1
      else
        number_of_columns = number_of_columns + 1
      end if
    end if
    rewind(s)

  end function number_of_columns


  integer function number_of_rows(s) result(nrows)
    !! version: experimental
    !!
    !! Determine the number or rows in a file
    integer, intent(in)::s
    integer :: ios

    rewind(s)
    nrows = 0
    do
      read(s, *, iostat=ios)
      if (ios /= 0) exit
      nrows = nrows + 1
    end do

    rewind(s)

  end function number_of_rows


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
      access_='sequential'
    case('b')
      form_='unformatted'
      access_ = 'stream'
    case default
      call error_stop("Unsupported mode: "//mode_(3:3))
    end select

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

  !> Version: experimental
  !>
  !> Read a whole line from a formatted unit into a deferred length character variable
  subroutine get_line_char(unit, line, iostat, iomsg)
    !> Formatted IO unit
    integer, intent(in) :: unit
    !> Line to read
    character(len=:), allocatable, intent(out) :: line
    !> Status of operation
    integer, intent(out), optional :: iostat
    !> Error message
    character(len=:), allocatable, optional :: iomsg

    integer, parameter :: bufsize = 4096
    character(len=bufsize) :: buffer, msg
    integer :: chunk, stat
    logical :: opened

    if (unit /= -1) then
      inquire(unit=unit, opened=opened)
    else
      opened = .false.
    end if

    if (opened) then
      open(unit=unit, pad="yes", iostat=stat, iomsg=msg)
    else
      stat = 1
      msg = "Unit is not connected"
    end if

    line = ""
    do while (stat == 0)
      read(unit, '(a)', advance='no', iostat=stat, iomsg=msg, size=chunk) buffer
      if (stat > 0) exit
      line = line // buffer(:chunk)
    end do
    if (is_iostat_eor(stat)) stat = 0

    if (stat /= 0 .and. present(iomsg)) iomsg = trim(msg)
    if (present(iostat)) then
      iostat = stat
    else if (stat /= 0) then
      call error_stop(trim(msg))
    end if
  end subroutine get_line_char

  !> Version: experimental
  !>
  !> Read a whole line from a formatted unit into a string variable
  subroutine get_line_string(unit, line, iostat, iomsg)
    !> Formatted IO unit
    integer, intent(in) :: unit
    !> Line to read
    type(string_type), intent(out) :: line
    !> Status of operation
    integer, intent(out), optional :: iostat
    !> Error message
    character(len=:), allocatable, optional :: iomsg

    character(len=:), allocatable :: buffer

    call get_line(unit, buffer, iostat, iomsg)
    line = string_type(buffer)
  end subroutine get_line_string

  !> Version: experimental
  !>
  !> Read a whole line from the standard input into a deferred length character variable
  subroutine get_line_input_char(line, iostat, iomsg)
    !> Line to read
    character(len=:), allocatable, intent(out) :: line
    !> Status of operation
    integer, intent(out), optional :: iostat
    !> Error message
    character(len=:), allocatable, optional :: iomsg

    call get_line(input_unit, line, iostat, iomsg)
  end subroutine get_line_input_char

  !> Version: experimental
  !>
  !> Read a whole line from the standard input into a string variable
  subroutine get_line_input_string(line, iostat, iomsg)
    !> Line to read
    type(string_type), intent(out) :: line
    !> Status of operation
    integer, intent(out), optional :: iostat
    !> Error message
    character(len=:), allocatable, optional :: iomsg

    call get_line(input_unit, line, iostat, iomsg)
  end subroutine get_line_input_string

  !> Version: experimental
  !> 
  !> Reads a whole ASCII file and loads its contents into a string variable.
  !> The function handles error states and optionally deletes the file after reading.
  subroutine get_file_string(filename,file,err,delete) 
      !> Input file name
      character(*), intent(in) :: filename
      !> Output string variable
      type(string_type), intent(out) :: file
      !> [optional] State return flag. On error, if not requested, the code will stop.
      type(state_type), optional, intent(out) :: err
      !> [optional] Delete file after reading? Default: do not delete
      logical, optional, intent(in) :: delete
        
      ! Local variables
      character(len=:), allocatable :: filestring
      
      ! Process output
      call get_file_char(filename,filestring,err,delete)
      call move(from=fileString,to=file)

  end subroutine get_file_string

  !> Version: experimental
  !> 
  !> Reads a whole ASCII file and loads its contents into an allocatable `character` variable.
  !> The function handles error states and optionally deletes the file after reading.
  subroutine get_file_char(filename,file,err,delete) 
      !> Input file name
      character(*), intent(in) :: filename
      !> Output string variable
      character(len=:), allocatable, intent(out) :: file
      !> [optional] State return flag. On error, if not requested, the code will stop.
      type(state_type), optional, intent(out) :: err
      !> [optional] Delete file after reading? Default: do not delete
      logical, optional, intent(in) :: delete
        
      ! Local variables
      type(state_type) :: err0
      character(len=512) :: iomsg
      integer :: lun,iostat
      integer(int64) :: errpos,file_size
      logical :: is_present,want_deleted

      !> Check if the file should be deleted after reading
      if (present(delete)) then 
         want_deleted = delete
      else
         want_deleted = .false.   
      end if

      !> Check file existing
      inquire(file=filename, exist=is_present)
      if (.not.is_present) then
         allocate(character(len=0) :: file)
         err0 = state_type('get_file',STDLIB_IO_ERROR,'File not present:',filename)
         call err0%handle(err)
         return
      end if
      
      !> Retrieve file size
      inquire(file=filename,size=file_size)
      
      invalid_size: if (file_size<0) then 

          allocate(character(len=0) :: file)
          err0 = state_type('get_file',STDLIB_IO_ERROR,filename,'has invalid size=',file_size)
          call err0%handle(err)
          return            
            
      endif invalid_size  
            
      ! Read file
      open(newunit=lun,file=filename, &
           form='unformatted',action='read',access='stream',status='old', &
           iostat=iostat,iomsg=iomsg)
             
      if (iostat/=0) then 
         allocate(character(len=0) :: file)
         err0 = state_type('get_file',STDLIB_IO_ERROR,'Cannot open',filename,'for read:',iomsg)
         call err0%handle(err)
         return
      end if     
        
      allocate(character(len=file_size) :: file)
        
      read_data: if (file_size>0) then 
            
          read(lun, pos=1, iostat=iostat, iomsg=iomsg) file
            
          ! Read error
          if (iostat/=0) then 
                
              inquire(unit=lun,pos=errpos)                    
              err0 = state_type('get_file',STDLIB_IO_ERROR,iomsg,'(',filename,'at byte',errpos,')')
              call err0%handle(err)
              return

          endif
            
      end if read_data
                   
      if (want_deleted) then 
         close(lun,iostat=iostat,status='delete')
         if (iostat/=0) err0 = state_type('get_file',STDLIB_IO_ERROR,'Cannot delete',filename,'after reading')
      else
         close(lun,iostat=iostat)
         if (iostat/=0) err0 = state_type('get_file',STDLIB_IO_ERROR,'Cannot close',filename,'after reading')
      endif 
      
      ! Process output
      call err0%handle(err)

  end subroutine get_file_char

end module stdlib_io
