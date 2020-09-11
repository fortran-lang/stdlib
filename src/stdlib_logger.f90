module stdlib_logger
!!### Module STDLIB_LOGGER
!!
!! This module defines a derived type, procedures, a variable, and
!! constants to be used for reporting errors by the Fortran Standard
!! Library.
!!
!! The derived type, LOGGER_T, is to be used to define variables to
!! serve as both local and global loggers. A logger directs its messages
!! to selected I/O units so the user has a record (a log) of major events.
!! For each entity of LOGGER_T the reports go to a list of I/O units
!! represented by the private internal array, LOG_UNITS. If LOG_UNITS is
!! empty then output by default goes to OUTPUT_UNIT. Otherwise reports
!! go to OUTPUT_UNIT only if it has been explicitly added to LOG_UNITS.
!! Each entity of type LOGGER_T also maintains an internal state
!! controlling the formatting of output.
!!
!! The procedures are as follows. The logical function
!! LOG_UNITS_ASSIGNED returns the number of I/O units in LOG_UNITS. The
!! subroutines ADD_LOG_FILE and ADD_LOG_UNIT include the specified file
!! in LOG_UNITS. REMOVE_LOG_UNIT removes the specified logical unit from
!! the LOG_UNITS array and optionally closes the file. CONFIGURE
!! configures the details of the logging process. CONFIGURATION
!! reports the details of that configuration. The subroutines
!! LOG_ERROR, LOG_INFORMATION, LOG_IO_ERROR, LOG_MESSAGE,
!! LOG_TEXT_ERRROR, and LOG_WARNING send messages to the log units.
!!
!! The variable is the entity GLOBAL_LOGGER of type LOGGER_T, to serve
!! as its name suggests, as a global logger to be used as a default
!! anywhere in the source code.
!!
!! The constants are used to report errors by some of the subroutines
!! in their optional STAT arguments. The constants are as follows.
!! SUCCESS indicates that no error has occurred. CLOSE_FAILURE
!! indicates that a `CLOSE` statement for an I/O unit failed.
!! INVALID_INDEX_ERROR` indicates that `COLUMN` was invalid for
!! the given `LINE`. OPEN_FAILURE indicates that an `OPEN` statement
!! failed. READ_ONLY_ERROR indicates that an output unit did not have a
!! `WRITE` or `READWRITE` action. SEQUENTIAL_ACCESS_ERROR indicates
!! that the unit did not have `SEQUENTIAL` access. UNFORMATTED_IN_ERROR
!! indicates that the unit did not have a `FORM` of `FORMATTED`.
!! UNOPENED_IN_ERROR indicates that the unit was not opened. WRITE_FAILURE
!! indicates that at least one of the writes to `LOG_UNITS` failed.

    use, intrinsic ::           &
        iso_fortran_env, only : &
        error_unit,             &
        input_unit,             &
        output_unit

    use stdlib_ascii, only : to_lower
    use stdlib_optval, only : optval

    implicit none

    private
    public :: global_logger, logger_t

    !! public constants used as error flags
    integer, parameter, public :: &
        success = 0,              &
        close_failure = 1,        &
        invalid_index_error = 2,  &
        non_sequential_error = 3, &
        open_failure = 4,         &
        read_only_error = 5,      &
        unformatted_in_error = 6, &
        unopened_in_error = 7,    &
        write_failure = 8

    character(*), parameter :: module_name = 'stdlib_logger'

    !! Public derived type
    type :: logger_t
        !! version: experimental
        
        private

        logical              :: add_line = .TRUE.
        logical              :: indent_lines = .TRUE.
        integer, allocatable :: log_units(:)
        integer              :: max_width = 80
        logical              :: time_stamp = .TRUE.
        integer              :: units = 0

    contains

!        procedure, pass(self) :: assert
        procedure, pass(self) :: add_log_file
        procedure, pass(self) :: add_log_unit
        procedure, pass(self) :: configuration
        procedure, pass(self) :: configure
        final                 :: final_logger
        procedure, pass(self) :: log_error
        procedure, pass(self) :: log_information
        procedure, pass(self) :: log_io_error
        procedure, pass(self) :: log_message
        procedure, pass(self) :: log_text_error
        procedure, pass(self) :: log_units_assigned
        procedure, pass(self) :: log_warning
        procedure, pass(self) :: remove_log_unit
    end type logger_t

    !! Variable of type LOGGER_T to be used as a global logger
    type(logger_t) :: global_logger

    character(*), parameter :: &
        invalid_column = 'COLUMN is not a valid index to LINE.'

contains

    subroutine add_log_file( self, filename, unit, action, position, status, &
                             stat )
!! version: experimental

!! Opens a formatted sequential access output file, `filename` using
!! `newunit` and adds the resulting unit number to the logger's `log_units`
!! array. `ACTION`, if present, is the `ACTION` specifier of the `OPEN`
!! statement, and has the default value of 'WRITE'. `POSITION`, if present,
!! is the `POSITION` specifier, and has the default value of 'REWIND'.
!! `STATUS`, if present, is the `STATUS` specifier of the OPEN statement, and
!! has the default value of 'REPLACE'. `STAT`, if present, has the value
!! `SUCCESS` if `FILENAME` could be opened, `READ_ONLY_ERROR` if `ACTION` is
!! 'READ", and `OPEN_FAILURE` otherwise.

        class(logger_t), intent(inout)     :: self
!! The logger variable to which the file is to be added
        character(*), intent(in)           :: filename
!! The name of the file to be  added to the logger
        integer, intent(out)               :: unit
!! The resulting I/O unit number
        character(*), intent(in), optional :: action
!! The `ACTION` specifier for the `OPEN` statement
        character(*), intent(in), optional :: position
!! The `POSITION` specifier for the `OPEN` statement
        character(*), intent(in), optional :: status
!! The `STATUS` specifier for the  `OPEN`  statement
        integer, intent(out), optional     :: stat
!! The error status on exit with the possible values
!! * `SUCCESS` - no errors found
!! * `READ_ONLY_ERROR` - file unopened as ACTION was 'READ' for an output file
!! * `OPEN_FAILURE` - the OPEN statement failed


!!##### Example
!!
!!     program main
!!         use stdlib_logger
!!         ...
!!         integer :: unit, stat
!!         ...
!!         call global_logger % add_log_file( 'error_log.txt', unit,      &
!!                                            position='asis', stat=stat )
!!         if ( stat /= success ) then
!!             error stop 'Unable to open "error_log.txt".'
!!         end if
!!         ...
!!     end program main

        character(128) :: iomsg
        integer        :: iostat
        character(16)  :: aaction, aposition, astatus
        character(*), parameter :: procedure_name = 'add_log_file'
        integer, allocatable :: dummy(:)
        integer        :: lun
        integer        :: i

        aaction = optval(action, 'write')
        aposition = optval(position, 'rewind')
        astatus = optval(status, 'replace')

        if ( len_trim(aaction) == 4 ) then
            do i=1, 4
                aaction(i:i) = to_lower(aaction(i:i))
            end do
            if ( aaction == 'read' ) then
                if ( present( stat ) ) then
                    stat = read_only_error
                    return

                else
                    error stop 'In ' // module_name // ' % ' //         &
                        procedure_name // ' ACTION is "READ" which ' // &
                        'does not allow writes to the file.'

                end if

            end if

        end if

        open( newunit=unit, file=filename, form='formatted', action=aaction,  &
              position=aposition, status=astatus, iostat=iostat, iomsg=iomsg, &
              err=999 )

        if ( allocated( self % log_units ) ) then
            if ( size(self % log_units) == self % units ) then
                allocate( dummy(2*self % units) )
                do lun=1, self % units
                    dummy(lun) = self % log_units(lun)

                end do
                dummy(self % units+1:) = 0

                call move_alloc( dummy, self % log_units )

            end if

        else
            allocate( self % log_units(16) )

        end if

        self % log_units(self % units + 1 ) = unit
        self % units = self % units + 1
        if ( present(stat) ) stat = success

        return

999     if (present(stat) ) then
            stat = open_failure
            return

        else
            call self % log_io_error( 'Unable to open ' // trim(filename), &
                                      module = module_name,                &
                                      procedure = procedure_name,          &
                                      iostat = iostat,                     &
                                      iomsg = iomsg )

            error stop module_name // ' % ' // procedure_name // &
                ': Unable to open file'

        end if

    end subroutine add_log_file


    subroutine add_log_unit( self, unit, stat )
!! version: experimental

!! Adds UNIT to the log file units in LOG_UNITS. UNIT must be an OPEN
!! file, of FORM FORMATTED, with SEQUENTIAL ACCESS, and an ACTION of
!! 'WRITE' or 'READWRITE', otherwise either STAT, if preseent, has a
!! value other than SUCCESS and UNIT is not entered into LOG_UNITS,
!! or, if STAT is not presecn, processing stops.
        class(logger_t), intent(inout) :: self
!! The logger variable to which the I/O unit is to be added
        integer, intent(in)            :: unit
!! The input logical unit number
        integer, intent(out), optional :: stat
!! An error code with the possible values
!! * SUCCESS - no problems were found
!! * NON_SEQUENTIAL_ERROR - UNIT did not have sequential access
!! * READ_ONLY_ERROR - UNIT was not writeable
!! * UNFORMATTED_IN_ERROR - UNIT was an UNFORMATTED file
!! * UNOPENED_IN_ERROR - UNIT was not opened

!!##### Example
!!
!!     program main
!!         use stdlib_logger
!!         ...
!!         character(256) :: iomsg
!!         integer :: iostat, unit, stat
!!         ...
!!         open( newunit=unit, 'error_log.txt', form='formatted', &
!!               status='replace', position='rewind', err=999,  &
!!               action='read', iostat=iostat, iomsg=iomsg )
!!         ...
!!         call global_logger % add_log_unit( unit, stat )
!!         select case ( stat )
!!         ...
!!         case ( read_only_error )
!!             error stop 'Unable to write to "error_log.txt".'
!!         ...
!!         end select
!!         ...
!!     999 error stop 'Unable to open "error_log.txt".
!!         ...
!!     end program main

        integer, allocatable :: dummy(:)
        character(*), parameter :: procedure_name = 'SET_LOG_UNIT'
        integer :: lun
        character(12) :: specifier
        logical :: question

        call validate_unit()
        if ( present(stat) ) then
            if ( stat /= success ) return
        end if

        do lun = 1, self % units
! Check that unit is not already registered
            if (self % log_units(lun) == unit ) return

        end do
        if ( allocated( self % log_units ) ) then
            if ( size(self % log_units) == self % units ) then
                allocate( dummy(2*self % units) )
                do lun=1, self % units
                    dummy(lun) = self % log_units(lun)

                end do

                call move_alloc( dummy, self % log_units )

            end if

        else
            allocate( self % log_units(16) )

        end if

        self % log_units(self % units + 1 ) = unit
        self % units = self % units + 1

    contains

        subroutine validate_unit()

! Check that UNIT is not INPUT_UNIT
            if ( unit == input_unit ) then
                if ( present(stat) ) then
                    stat = read_only_error
                    return

                else
                    error stop 'UNIT in ' // module_name // ' % ' // &
                        procedure_name // ' must not be INPUT_UNIT.'

                end if

            end if

! Check that UNIT is opened
            inquire( unit, opened=question )
            if ( .not. question ) then
                if ( present(stat) ) then
                    stat = unopened_in_error
                    return

                else
                    error stop 'UNIT in ' // module_name // ' % ' // &
                        procedure_name // ' is not OPEN.'

                end if

            end if

! Check that UNIT is writeable
            inquire( unit, write=specifier )
            if ( specifier(1:1) /= 'Y' .and. specifier(1:1) /= 'y' ) then
                if ( present(stat) ) then
                    stat = read_only_error
                    return

                else
                    error stop 'UNIT in ' // module_name // ' % ' // &
                        procedure_name // ' is not writeable.'

                end if

            end if

            inquire( unit, sequential=specifier )
            if ( specifier(1:1) /= 'Y' .and. specifier(1:1) /= 'y' ) then
                if ( present(stat) ) then
                    stat = non_sequential_error
                    return

                else
                    error stop 'UNIT in ' // module_name // ' % ' // &
                        procedure_name // ' is not "SEQUENTIAL".'

                end if

            end if

            inquire( unit, formatted=specifier )
            if ( specifier(1:1) /= 'Y' .and. specifier(1:1) /= 'y' ) then
                if ( present(stat) ) then
                    stat = unformatted_in_error
                    return

                else
                    error stop 'UNIT in ' // module_name // ' % ' // &
                        procedure_name // ' is not "FORMATTED".'

                end if

            end if

            if ( present(stat) ) stat = success

        end subroutine validate_unit

    end subroutine add_log_unit


!    subroutine assert( self, test, message, module, procedure )
! Checks the value of TEST and if TEST is .FALSE. writes output to the
! I/O units in SELF % LOG_UNITS and stops processing, otherwise it returns
! with no effect.
!
! ##### Behavior
! If TEST is .FALSE. ASSERT will write to the files, otherwise
! nothing is written. If time stamps are actiVe then the time stamp will
! be written first. Then if MODULE and PROCEDURE are present then they will
! be written.Finally MESSAGE, will be written prepended by the
! string 'ASSERTION FAILURE: '.
!
!        class(logger_t), intent(in)            :: self
!! The logger variabl to report the error
!        logical, intent(in)                    :: test
!! A logical condition whose failure indicates an error has occurred.
!        character(len=*), intent(in)           :: message
!! Typically the textual representation of TEST
!        character(len=*), intent(in), optional :: module
!! The name of the module containing the call of ASSERT
!        character(len=*), intent(in), optional :: procedure
!! The name of the procedure containing the call of ASSERT
!
!!##### Example
!!
!!     function factorial( i )
!!       use stdlib_logger
!!       real                :: factorial
!!       integer, intent(in) :: i
!!       integer             :: j
!!       call assert( i >= 0,                  &
!!                    'i >= 0.',               &
!!                    procedure = "FACTORIAL" )
!!       factorial = 1.0
!!       do j=1, i
!!           factorial = factorial * j
!!       end do
!!
!!       return
!!     end function factorial
!
!
!        integer :: status_code
!
!        if ( test ) then
!            return
!
!        end if
!
!        call self % log_message( 'ASSERTION FAILURE: ' // message, &
!                                 module = module,                  &
!                                 procedure = procedure )
!
!        error stop 'Failed assertion'
!
!        return
!
!    end subroutine assert

    pure subroutine configuration( self, add_line, indent, max_width, &
        time_stamp, log_units )
!! version: experimental

!! Reports the logging configuration of SELF. The following attributes are
!! reported:
!! 1. ADD_LINE is a logical flag with .TRUE. implying that output starts
!!    with a blank line, and .FALSE. implying no blank line.
!! 2. INDENT is a logical flag with .TRUE. implying that subsequent columns
!!    will be indented 4 spaces and .FALSE. implying no indentation.
!! 3. MAX_WIDTH is the maximum number of columns of output text with
!!    MAX_WIDTH == 0 => no bounds on output width.
!! 4. TIME_STAMP is a logical flag with .TRUE. implying that the output
!!    will have a time stamp, and .FALSE. implying that there will be no
!!    time stamp.
!! 5. LOG_UNITS is an array of the logical unit numbers to which log output
!!    will be written
        class(logger_t), intent(in)                 :: self
!! The logger variable whose configuration is being reported
        logical, intent(out), optional              :: add_line
!! A logical flag to add a preceding blank line
        logical, intent(out), optional              :: indent
!! A logical flag to indent subsequent lines
        integer, intent(out), optional              :: max_width
!! The maximum number of columns for most outputs
        logical, intent(out), optional              :: time_stamp
!! A logical flag to add a time stamp
        integer, intent(out), allocatable, optional :: log_units(:)
!! The I/O units used in output

!!##### Example
!!
!!     module example_mod
!!       use stdlib_logger
!!       ...
!!     contains
!!       ...
!!       subroutine example_sub(unit, ...)
!!         integer, intent(in) :: unit
!!         ...
!!         integer, allocatable :: log_units(:)
!!         ...
!!         call global_logger % configuration( log_units=log_units )
!!         if ( size(log_units) == 0 ) then
!!            call add_logger_unit( unit )
!!         end if
!!         ..
!!       end subroutine example_sub
!!       ...
!!     end module example_mod


        if ( present(add_line) ) add_line = self % add_line
        if ( present(indent) ) indent = self % indent_lines
        if ( present(max_width) ) max_width = self % max_width
        if ( present(time_stamp) ) time_stamp = self % time_stamp
        if ( present(log_units) ) log_units = self % log_units(1:self % units)

    end subroutine configuration


    pure subroutine configure( self, add_line, indent, max_width, time_stamp )
!! version: experimental

!! Configures the logging process for SELF. The following attributes are
!! configured:
!! 1. ADD_LINE is a logical flag with .TRUE. implying that output starts
!!    with a blank line, and .FALSE. implying no blank line. ADD_LINE has a
!!    default value of .TRUE..
!! 2. INDENT is a logical flag with .TRUE. implying that subsequent lines
!!    will be indented 4 spaces and .FALSE. implying no indentation. INDENT
!!    has a default value of .TRUE..
!! 3. MAX_WIDTH is the maximum number of columns of output text with
!!    MAX_WIDTH == 0 => no bounds on output width. MAX_WIDTH has a default
!!    value of 80.
!! 4. TIME_STAMP is a logical flag with .TRUE. implying that the output
!!    will have a time stamp, and .FALSE. implying that there will be no
!!    time stamp. TIME_STAMP has a default value of .TRUE..

!!##### Example
!!
!!     program main
!!         use stdlib_logger
!!         ...
!!         call global_logger % configure( indent=.false., max_width=72 )
!!         ...

        class(logger_t), intent(inout) :: self
        logical, intent(in), optional  :: add_line
        logical, intent(in), optional  :: indent
        integer, intent(in), optional  :: max_width
        logical, intent(in), optional  :: time_stamp

        if ( present(add_line) ) self % add_line = add_line
        if ( present(indent) ) self % indent_lines = indent
        if ( present(max_width) ) then
            if ( max_width <= 4 ) then
                self % max_width = 0

            else
                self % max_width = max_width

            end if

        end if
        if ( present(time_stamp) ) self % time_stamp = time_stamp

    end subroutine configure


    subroutine final_logger( self )
!! finalizes the logger_t entity by flushing the units
        type(logger_t), intent(in) :: self

        integer        :: iostat
        character(256) :: message
        integer        :: unit

        do unit=1, self % units
            flush( self % log_units(unit), iomsg=message, iostat=iostat )
            if ( iostat /= 0 ) then
                write(error_unit, '(a, i0)' ) 'In the logger_t finalizer ' // &
                    'an error occurred in flushing UNIT = ',                  &
                    self % log_units(unit)
                write(error_unit, '(a, i0)') 'With IOSTAT = ', iostat
                write(error_unit, '(a)') 'With IOMSG = ' // trim(message)

            end if

        end do

        return
    end subroutine final_logger


    subroutine format_output_string( self, unit, string, procedure_name, &
                                     col_indent )
!! version: experimental

!! Writes the STRING to UNIT ensuring that the number of characters
!! does not exceed MAX_WIDTH and that the lines after the first
!! one are indented four characters.
        class(logger_t), intent(in) :: self
        integer, intent(in)         :: unit
        character(*), intent(in)    :: string
        character(*), intent(in)    :: procedure_name
        character(*), intent(in)    :: col_indent

        integer :: count, indent_len, index, iostat, length, remain
        character(256) :: iomsg

        length = len_trim(string)
        indent_len = len(col_indent)
        call format_first_line()

        if ( self % indent_lines ) then
            do while( remain > 0 )
                call indent_format_subsequent_line()

            end do

        else
            do while( remain > 0 )
                call format_subsequent_line()

            end do

        end if

    contains

        subroutine format_first_line()

            if ( length <= self % max_width .or. self % max_width == 0 ) then
                write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                    string(1:length)
                remain = 0
                return

            else
                do index=self % max_width, 1, -1
                    if ( string(index:index) == ' ' ) exit

                end do
                if ( index == 0 ) then
                    write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                        string(1:self % max_width)
                    count = self % max_width
                    remain = length - count
                    return

                else
                    write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                        string(1:index-1)
                    count = index
                    remain = length - count
                    return

                end if

            end if

999         call handle_write_failure( unit, procedure_name, iostat, iomsg )

        end subroutine format_first_line

        subroutine format_subsequent_line()

            if ( remain <= self % max_width ) then
                write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                    string(count+1:length)
                count = length
                remain = 0
                return

            else
                do index=count+self % max_width, count+1, -1
                    if ( string(index:index) == ' ' ) exit
                end do
                if ( index == count ) then
                    write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                        string(count+1:count+self % max_width)
                    count = count + self % max_width
                    remain = length - count
                    return

                else
                    write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                        string(count+1:index)
                    count = index
                    remain = length - count
                    return

                end if

            end if

999         call handle_write_failure( unit, procedure_name, iostat, iomsg )

        end subroutine format_subsequent_line

        subroutine indent_format_subsequent_line()

            if ( remain <= self % max_width - indent_len ) then
                write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                    col_indent // string(count+1:length)
                count = length
                remain = 0
                return

            else
                do index=count+self % max_width-indent_len, count+1, -1
                    if ( string(index:index) == ' ' ) exit
                end do
                if ( index == count ) then
                    write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                        col_indent // &
                        string(count+1:count+self % max_width-indent_len)
                    count = count + self % max_width - indent_len
                    remain = length - count
                    return

                else
                    write( unit, '(a)', err=999, iostat=iostat, iomsg=iomsg ) &
                        col_indent // string(count+1:index)
                    count = index
                    remain = length - count
                    return

                end if

            end if

999         call handle_write_failure( unit, procedure_name, iostat, iomsg )

        end subroutine indent_format_subsequent_line

    end subroutine format_output_string


    subroutine handle_write_failure( unit, procedure_name, iostat, iomsg )
!! version: experimental

!! Handles a failure to write to UNIT in PROCEDURE_NAME with IOSTAT and
!! IOMSG by writing a description of the failure to OUTPUT_UNIT and
!! stopping.
        integer, intent(in)      :: unit
        character(*), intent(in) :: procedure_name
        integer, intent(in)      :: iostat
        character(*), intent(in) :: iomsg

        character(256) :: name
        logical :: named
        character(10) :: action

         write( output_unit, '(a)' ) 'WRITE failure in ' // module_name // &
             ' % ' // trim(procedure_name) // '.'
         write( output_unit, '(a, i0)' ) 'UNIT = ', unit
         inquire( unit, named=named )
         if ( named ) then
             inquire( unit, name=name )
             write( output_unit, '(a, a)' ) 'NAME = ', trim(name)

         else
             write( output_unit, '(a)' ) 'UNIT is UNNAMED'

         end if
         inquire( unit, action=action )
         write( output_unit, '(a, a)' ) 'ACTION = ', trim(action)
         write( output_unit, '(a, i0)' ) 'IOSTAT = ', iostat
         write( output_unit, '(a, a )' ) 'IOMSG = ', trim(iomsg)
         error stop 'WRITE failure in ' // module_name // '.'

     end subroutine handle_write_failure


    subroutine log_error( self, message, module, procedure, stat, errmsg )
!! Writes the string MESSAGE to SELF %LOG_UNITS with optional additional
!! text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written first. Then if
!! MODULE or PROCEDURE are present, they are written. Then MESSAGE is
!! written with the prefix 'ERROR: '. Then if STAT or ERRMSG
!! are present they are written.
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       real, allocatable :: a(:)
!!       ...
!!       type(logger_t) :: alogger
!!       ...
!!     contains
!!       ...
!!       subroutine example_sub( size )
!!         integer, intent(in) :: size
!!         character(128) :: errmsg, message
!!         integer        :: stat
!!         allocate( a(size), stat=stat, errmsg=errmsg )
!!         if ( stat /= 0 ) then
!!           write( message, `(a, i0)' )                    &
!!               "Allocation of A failed with SIZE = ", size
!!           alogger % call log_error( message,                   &
!!                                     module = 'EXAMPLE_MOD',    &
!!                                     procedure = 'EXAMPLE_SUB', &
!!                                     stat = stat,               &
!!                                     errmsg = errmsg )
!!         end if
!!       end subroutine example_sub
!!       ...
!!     end module example_mod
!!

        class(logger_t), intent(in)             :: self
!! The logger to be used in logging the message
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of REPORT_ERR
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of REPORT_ERR
        integer, intent(in), optional           :: stat
!! The value of the STAT specifier returned by a Fortran statement
        character(len=*), intent(in), optional  :: errmsg
!! The value of the ERRMSG specifier returned by a Fortran statement

        integer :: unit
        integer :: iostat
        character(*), parameter :: procedure_name = 'LOG_ERROR'
        character(256) :: iomsg

        call self % log_message( 'ERROR: ' // message, &
                                 module = module,      &
                                 procedure = procedure )

        if ( self % units == 0 ) then
            call write_log_error( output_unit )

        else
            do unit=1, self % units
                call write_log_error( self % log_units(unit) )
            end do

        end if

    contains

        subroutine write_log_error( unit )
            integer, intent(in) :: unit

            if ( present(stat) ) then
                write( unit, '("With STAT = ", i0)', err=999, &
                    iostat=iostat, iomsg=iomsg ) stat
            end if

            if ( present(errmsg) ) then
                if ( len_trim(errmsg) > 0 ) then
                    call format_output_string( self, unit,          &
                                               'With ERRMSG = "' // &
                                               trim(errmsg) // '"', &
                                               procedure_name,      &
                                               '    ' )
                end if

            end if

            return

999         call handle_write_failure( unit, procedure_name, iostat, iomsg )

        end subroutine write_log_error

    end subroutine log_error


    subroutine log_information( self, message, module, procedure )
!! Writes the string MESSAGE to SELF % LOG_UNITS with optional additional
!! text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written first. Then if
!! MODULE or PROCEDURE are present, they are written. Then MESSAGE is
!! written with the prefix 'INFORMATION: '.
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       real, allocatable :: a(:)
!!       ...
!!       type(logger_t) :: alogger
!!       ...
!!     contains
!!       ...
!!       subroutine example_sub( selection )
!!         integer, intent(out) :: selection
!!         integer        :: stat
!!         write(*,'(a)') "Enter an integer to select a widget"
!!         read(*,'(i0)') selection
!!         write( message, `(a, i0)' )           &
!!               "The user selected ", selection
!!         call alogger % log_information( message,                   &
!!                                         module = 'EXAMPLE_MOD',    &
!!                                         procedure = 'EXAMPLE_SUB' )
!!         ...
!!       end subroutine example_sub
!!       ...
!!     end module example_mod
!!

        class(logger_t), intent(in)             :: self
!! The logger used to send the message
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of REPORT_ERR
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of REPORT_ERR

        call self % log_message( 'INFORMATION: ' // message, &
                                 module = module,      &
                                 procedure = procedure )

    end subroutine log_information


    subroutine log_io_error( self, message, module, procedure, iostat, &
                             iomsg )
!! Writes the string MESSAGE to the SELF  % LOG_UNITS with optional
!! additional text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written first. Then if
!! MODULE or PROCEDURE are present, they are written. Then MESSAGE is
!! written with a prefix 'I/O ERROR: '. Then if IOSTAT or IOMSG
!! are present they are also written.
!!
!!##### Example
!!
!!    program example
!!      use stdlib_logger
!!      ...
!!      character(*), parameter :: filename = 'dummy.txt'
!!      integer                 :: iostat, lun
!!      character(128)          :: iomsg
!!      character(*), parameter :: message = 'Failure in opening "dummy.txt".'
!!
!!      open( newunit=lun, file = filename, form='formatted', &
!!            status='old', iostat=iostat, iomsg=iomsg )
!!      if ( iostat /= 0 ) then
!!        call global_logger % log_io_error( message, procedure = 'EXAMPLE', &
!!            iostat=iostat, iomsg = iomsg )
!!        error stop 'Error on opening ' // filename
!!      end if
!!      ...
!!    end program example

        class(logger_t), intent(in)             :: self
!! The logger variable to receivee the message
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of REPORT_ERROR
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of REPORT_ERROR
        integer, intent(in), optional           :: iostat
!! The value of the IOSTAT specifier returned by a Fortran I/O statement
        character(len=*), intent(in), optional  :: iomsg
!! The value of the IOMSG specifier returned by a Fortran I/O statement

        integer :: unit
        integer :: iostat2
        character(*), parameter :: procedure_name = 'LOG_ERROR'
        character(256) :: iomsg2

        call self % log_message( 'I/O ERROR: ' // message, &
                                 module = module,          &
                                 procedure = procedure )

        if ( self % units == 0 ) then
            call write_log_io_error( output_unit )

        else
            do unit=1, self % units
                call write_log_io_error( self % log_units(unit) )
            end do

        end if

    contains

        subroutine write_log_io_error( unit )
            integer, intent(in) :: unit

            if ( present(iostat) ) then
                write( unit, '("With IOSTAT = ", i0)', err=999, &
                    iostat=iostat2, iomsg=iomsg2 ) iostat
            end if

            if ( present(iomsg) ) then
                if ( len_trim(iomsg) > 0 ) then
                    call format_output_string( self,  unit,        &
                                               'With IOMSG = "' // &
                                               trim(iomsg) // '"', &
                                               procedure_name,     &
                                               '    ' )
                end if

            end if

            return

999         call handle_write_failure( unit, procedure_name, iostat, iomsg )

        end subroutine write_log_io_error

    end subroutine log_io_error


    subroutine log_message( self, message, module, procedure )
!! version: experimental

!! Writes the string MESSAGE to the SELF % LOG_UNITS with optional
!! additional text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written first. Then if
!! MODULE or PROCEDURE are present, they are written. Finally MESSAGE is
!! written
!!
!!##### Example
!!
!!    module  example_mod
!!      use stdlib_logger
!!      ...
!!      real, allocatable :: a(:)
!!      ...
!!    contains
!!      ...
!!      subroutine example_sub( selection )
!!        integer, intent(out) :: selection
!!        integer        :: stat
!!        write(*,'(a)') "Enter an integer to select a widget"
!!        read(*,'(i0)') selection
!!        write( message, `(a, i0)' )          &
!!              "The user selected ", selection
!!        call global_logger % log_message( message,                   &
!!                                          module = 'EXAMPLE_MOD',    &
!!                                          procedure = 'EXAMPLE_SUB' )
!!      end subroutine example_sub
!!      ...
!!    end module example_mod
!!

        class(logger_t), intent(in)             :: self
!! The logger variable to receive the message
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of LOG_MESSAGE
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of LOG_MESSAGE

        integer :: unit
        integer :: iostat
        character(*), parameter :: procedure_name = 'LOG_MESSAGE'
        character(256) :: iomsg

        if ( self % units == 0 ) then
            call write_log_message( output_unit )

        else
            do unit=1, self % units
                call write_log_message( self % log_units(unit) )

            end do

        end if

    contains

        subroutine write_log_message( unit )
            integer, intent(in) :: unit

            if ( self % add_line ) write( unit, *, err=999, iostat=iostat, &
                iomsg=iomsg )

            if ( self % time_stamp ) write( unit, '(a)', err=999, &
                iostat=iostat, iomsg=iomsg ) time_stamp()

            if ( present(module) ) then
                if ( present(procedure) ) then
                    write( unit,                                     &
                           "('Module % Procedure: ', a, ' % ', a)",  &
                           err=999, iostat=iostat, iomsg=iomsg)      &
                        trim( module ), trim( procedure )

                else
                    write( unit, "( 'Module: ', a)", err=999, iostat=iostat, &
                        iomsg=iomsg ) trim( module )

                end if

            else if ( present(procedure) ) then
                write( unit, "( 'Procedure: ', a)", err=999, iostat=iostat, &
                    iomsg=iomsg ) trim( procedure )

            end if

            call format_output_string( self, unit, trim( message ), &
                                       procedure_name, '    ' )

            return

999         call handle_write_failure( unit, procedure_name, iostat, iomsg )

        end subroutine write_log_message

    end subroutine log_message


    subroutine log_text_error( self, line, column, summary, filename,  &
                               line_number, caret, stat )
!! LOG_TEXT_ERROR sends a message to SELF % LOG_UNITS describing an error found
!! in a line of text.
!!
!!##### Behavior
!!
!! If time stamps are active first a time stamp is written. Then if
!! FILENAME or LINE_NUMBER or column are present they are written.
!! Then LINE is written. Then a caret, '^', is written below LINE at the
!! column indicated by COLUMN. Then SUMMARY is written.
!
!!##### Example
!!
!!    program example
!!      ...
!!      character(*), parameter :: filename = 'dummy.txt'
!!      integer                 :: col_num, line_num, lun
!!      character(128)          :: line
!!      character(*), parameter :: message = 'Bad text found.'
!!
!!      open( newunit=lun, file = filename, statu='old', form='formatted' )
!!      line_num = 0
!!      do
!!        read( lun, fmt='(a)', end=900 ) line
!!        line_num = line_num + 1
!!        call check_line( line, status, col_num )
!!        if ( status /= 0 )
!!          call global_logger % log_text_error( line, col_num, message, &
!!                                               filename, line_num )
!!          error stop 'Error in reading ' // filename
!!        end if
!!        ...
!!      end do
!!900   continue
!!      ...
!!    end program example
!!
        class(logger_t), intent(in)           :: self
!! The logger variable to receive the message
        character(*), intent(in)              :: line
!! The line of text in which the error was found.
        integer, intent(in)                   :: column
!! The one's based column in LINE at which the error starts.
        character(*), intent(in)              :: summary
!! A brief description of the error.
        character(*), intent(in), optional    :: filename
!! The name of the file, if any, in which the error was found.
        integer, intent(in), optional         :: line_number
!! The one's based line number in the file where LINE was found.
        character(1), intent(in), optional    :: caret
!! The symbol used to mark the column wher the error was first detected
        integer, intent(out), optional        :: stat
!! Integer flag that an error has occurred. Has the value SUCCESS if no
!! error hass occurred, INVALID_INDEX if COLUMN is less than zero or
!! greater than LEN(LINE), and WRITE_FAILURE if any of the WRITE statements
!! has failed.

        character(1)              :: acaret
        character(5)              :: num
        character(:), allocatable :: fmt
        character(128)            :: iomsg
        integer                   :: iostat
        integer                   :: lun
        character(*), parameter   :: procedure_name = 'LOG_TEXT_ERROR'

        acaret = optval(caret, '^')

        if ( column < 0 .or. column > len( line ) + 1 ) then
            if ( present(stat) ) then
                stat = invalid_index_error
                return

            else
                call self % log_error( invalid_column,           &
                                       module = module_name,     &
                                       procedure = procedure_name )
                error stop module_name // ' % ' // procedure_name // ': ' // &
                    invalid_column

            end if

        end if

        write(num, '(i0)') column-1
        fmt = '(' // trim(num) // 'x, a)'

        if ( self % units == 0 ) then
            call write_log_text_error( output_unit )

        else
            do lun=1, self % units
                call write_log_text_error( self % log_units(lun) )

            end do

        end if

    contains

        subroutine write_log_text_error( unit )
            integer, intent(in) :: unit

            if ( self % add_line ) write( unit, * )

            if ( self % time_stamp ) write( unit, '(a)' ) time_stamp()

            if ( present(filename) ) then
                if ( present(line_number) ) then
                    write( unit, '(a,":", i0, ":", i0)', err=999, &
                           iomsg=iomsg, iostat=iostat )           &
                           trim(filename) , line_number, column

                else
                    write( unit, '(a, i0)', err=999, iomsg=iomsg, &
                           iostat=iostat ) &
                           "Error found in file: '" // trim(filename) // "'" &
                           // ', at column: ', column

                end if

            else
                if ( present(line_number) ) then
                    write( unit, '(a, i0, a, i0)', err=999, iomsg=iomsg, &
                           iostat=iostat ) &
                        'Error found at line number: ', line_number, &
                        ', and column: ', column
                else
                    write( unit, '("Error found in line at column:", i0)' ) &
                        column

                end if

            end if

            write( unit, * )
            write( unit, '(a)', err=999, iomsg=iomsg, iostat=iostat ) line
            write( unit, fmt, err=999, iomsg=iomsg, iostat=iostat ) &
                acaret
            write( unit, '(a)', err=999, iomsg=iomsg, iostat=iostat ) &
                'Error: ' // trim(summary)

            if ( present(stat) ) stat = success

            return

999         if ( present( stat ) ) then
                stat = write_failure
                return

            else
                call handle_write_failure( unit, procedure_name, iostat, &
                                           iomsg )

            end if

        end subroutine write_log_text_error

    end subroutine log_text_error


    elemental function log_units_assigned(self)
!! Returns the number of units assigned to SELF % LOG_UNITS
        class(logger_t), intent(in) :: self
!! The logger subject to the inquiry
        integer                     :: log_units_assigned
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       type(logger_t) :: alogger
!!       ...
!!     contains
!!       ...
!!       subroutine example_sub(unit, ...)
!!         integer, intent(in) :: unit
!!         ...
!!         integer, allocatable :: log_units(:)
!!         ...
!!         if ( alogger % log_units_assigned() == 0 ) then
!!            call alogger % add_log_unit( unit )
!!         end if
!!         ...
!!       end subroutine example_sub
!!       ...
!!     end module example_mod

        log_units_assigned = self % units

    end function log_units_assigned


    subroutine log_warning( self, message, module, procedure )
!! Writes the string MESSAGE to SELF % LOG_UNITS with optional additional text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written first. Then if
!! MODULE or PROCEDURE are present, they are written. Then MESSAGE is
!! written with the prefix 'WARNING: '.
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       real, allocatable :: a(:)
!!       ...
!!       type(logger_t) :: alogger
!!       ...
!!     contains
!!       ...
!!       subroutine example_sub( size, stat )
!!         integer, intent(in)  :: size
!!         integer, intent(out) :: stat
!!         allocate( a(size) )
!!         if ( stat /= 0 ) then
!!           write( message, `(a, i0)' )                    &
!!               "Allocation of A failed with SIZE = ", size
!!           call alogger % log_warning( message,                   &
!!                                       module = 'EXAMPLE_MOD',    &
!!                                       procedure = 'EXAMPLE_SUB' )
!!         end if
!!       end subroutine example_sub
!!       ...
!!     end module example_mod
!!
        class(logger_t), intent(in)             :: self
!! The logger to which the message is written
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of REPORT_ERR
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of REPORT_ERR

        call self % log_message( 'WARNING: ' // message, &
                                 module = module,        &
                                 procedure = procedure )

    end subroutine log_warning


    subroutine remove_log_unit( self, unit, close_unit, stat )
!! Remove the I/O UNIT from the SELF % LOG_UNITS list. If CLOSE_UNIT is
!! present and .TRUE. then the corresponding file is closed. If UNIT is
!! not in LOG_UNITS then nothing is done. If STAT is present it, by default,
!! has the value SUCCESS. If closing the UNIT fails, then if STAT is
!! present it has the value CLOSE_FAILURE, otherwise processing stops
!! with an informative message.
        class(logger_t), intent(inout) :: self
!! The logger variable whose unit is to be removed
        integer, intent(in)            :: unit
!! The I/O unit to be removed from SELF
        logical, intent(in), optional  :: close_unit
!! A logical flag to close the unit while removing it from the SELF list
        integer, intent(out), optional :: stat
!! An error status with the values
!! * SUCCESS - no problems found
!! * CLOSE_FAILURE - the CLOSE statement for UNIT failed
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       type(logger_t) ::  alogger
!!     contains
!!       ...
!!       subroutine example_sub(unit, ...)
!!         integer, intent(in) :: unit
!!         ...
!!         call alogger % remove_log_unit( unit )
!!         ...
!!       end subroutine example_sub
!!       ...
!!     end module example_mod

        character(128) :: errmsg
        integer :: lun, lun_old
        character(*), parameter :: procedure_name = 'REMOVE_LOG_UNIT'

        if ( present(stat) ) stat = success
        do lun=1, self % units
            if ( unit == self % log_units(lun) ) exit

        end do

        if ( lun == self % units + 1 ) return

        if ( present(close_unit) ) then
            if ( close_unit ) close( unit, err=999, iomsg=errmsg )

        end if

        do lun_old=lun+1, self % units
            self % log_units(lun_old-1) = self % log_units(lun_old)

        end do
        self % units = self % units - 1

        return

999     if ( present(stat) ) then
            stat = close_failure
            return

        else
            write(*, '(a, i0)') 'In ' // module_name // ' % ' // &
                procedure_name // ' CLOSE_UNIT failed for UNIT = ', unit
            write(*, '(a)' ) 'With IOMSG = ' // trim(errmsg)
            error stop 'CLOSE_UNIT failed in ' // module_name // ' % ' // &
                procedure_name // '.'

        end if

    end subroutine remove_log_unit


    function time_stamp()
!! Creates a time stamp in the format 'yyyy-mm-dd hh:mm:ss.sss'
        character(23) :: time_stamp
        character(8)  :: date
        character(10) :: time

        call date_and_time( date, time )

        time_stamp(1:4)   = date(1:4)
        time_stamp(5:5)   = '-'
        time_stamp(6:7)   = date(5:6)
        time_stamp(8:8)   = '-'
        time_stamp(9:10)  = date(7:8)
        time_stamp(11:11) = ' '
        time_stamp(12:13) = time(1:2)
        time_stamp(14:14) = ':'
        time_stamp(15:16) = time(3:4)
        time_stamp(17:17) = ':'
        time_stamp(18:23) = time(5:10)

    end function time_stamp

end module stdlib_logger
