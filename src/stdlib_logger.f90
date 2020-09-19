module stdlib_logger
!!### Module STDLIB_LOGGER
!!
!! This module defines a derived type, procedures, a variable, and
!! constants to be used for reporting errors by the Fortran Standard
!! Library.
!!
!! The derived type, `logger_type`, is to be used to define variables to
!! serve as both local and global loggers. A logger directs its messages
!! to selected I/O units so the user has a record (a log) of major events.
!! For each entity of `logger_type` the reports go to a list of I/O units
!! represented by the private internal array, `log_units`. If `log_units` is
!! empty then output by default goes to `output_unit`. Otherwise reports
!! go to `output_unit` only if it has been explicitly added to `log_units`.
!! Each entity of type `logger_type` also maintains an internal state
!! controlling the formatting of output.
!!
!! The procedures are as follows. The logical function
!! `log_units_assigned` returns the number of I/O units in `log_units`. The
!! subroutines `add_log_file` and `add_log_unit` include the specified file
!! in `log_units`. `remove_log_units` removes the specified logical unit from
!! the `log_units` array and optionally closes the file. `configure`
!! configures the details of the logging process. `configuration`
!! reports the details of that configuration. The subroutines
!! `log_error`, `log_information`, `log_io_error`, `log_message`,
!! `log_text_error`, and `log_warning` send messages to the log units.
!!
!! The variable is the entity `global_logger` of type `logger_type`, to serve
!! as its name suggests, as a global logger to be used as a default
!! anywhere in the source code.
!!
!! The constants are used to report errors by some of the subroutines
!! in their optional `stat` arguments. The constants are as follows.
!! `success` indicates that no error has occurred. `close_failure`
!! indicates that a `CLOSE` statement for an I/O unit failed.
!! `invalid_index_error` indicates that `column` was invalid for
!! the given `line`. `open_failure` indicates that an `OPEN` statement
!! failed. `read_only_error` indicates that an output unit did not have a
!! `"WRITE"` or `"READWRITE"` action. `non_sequential_error` indicates
!! that the unit did not have `SEQUENTIAL` access. `unformatted_in_error`
!! indicates that the unit did not have a `FORM` of `"FORMATTED"`.
!! `unopened_in_error` indicates that the unit was not opened. `write_failure`
!! indicates that at least one of the writes to `log_units` failed.

    use, intrinsic ::           &
        iso_fortran_env, only : &
        error_unit,             &
        input_unit,             &
        output_unit

    use stdlib_ascii, only : to_lower
    use stdlib_optval, only : optval

    implicit none

    private
    public :: global_logger, logger_type

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
    type :: logger_type
        !! version: experimental
        
        private

        logical              :: add_blank_line = .FALSE.
        logical              :: indent_lines = .TRUE.
        integer, allocatable :: log_units(:)
        integer              :: max_width = 0
        logical              :: time_stamp = .TRUE.
        integer              :: units = 0

    contains

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
    end type logger_type

    !! Variable of type `logger_type` to be used as a global logger
    type(logger_type) :: global_logger

    character(*), parameter :: &
        invalid_column = 'COLUMN is not a valid index to LINE.'

contains

    subroutine add_log_file( self, filename, unit, action, position, status, &
                             stat )
!! version: experimental

!! Opens a formatted sequential access output file, `filename` using
!! `newunit` and adds the resulting unit number to `self`'s `log_units`
!! array. `action`, if present, is the `ACTION` specifier of the `OPEN`
!! statement, and has the default value of `"WRITE"`. `position`, if present,
!! is the `POSITION` specifier, and has the default value of `"REWIND"`.
!! `status`, if present, is the `STATUS` specifier of the `OPEN` statement, and
!! has the default value of `"REPLACE"`. `stat`, if present, has the value
!! `success` if `filename` could be opened, `read_only_error` if `ACTION` is
!! `"READ"`, and `open_failure` otherwise.

        class(logger_type), intent(inout)  :: self
!! The logger variable to which the file is to be added
        character(*), intent(in)           :: filename
!! The name of the file to be  added to the logger
        integer, intent(out), optional     :: unit
!! The resulting I/O unit number
        character(*), intent(in), optional :: action
!! The `ACTION` specifier for the `OPEN`` statement
        character(*), intent(in), optional :: position
!! The `POSITION` specifier for the `OPEN` statement
        character(*), intent(in), optional :: status
!! The `STATUS` specifier for the  `OPEN`  statement
        integer, intent(out), optional     :: stat
!! The error status on exit with the possible values
!! * `success` - no errors found
!! * `Rrea_only_error` - file unopened as `action1 was `"READ"` for an output file
!! * `open_failure` - the `OPEN` statement failed


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

        character(16)  :: aaction, aposition, astatus
        integer        :: aunit
        character(128) :: iomsg
        integer        :: iostat
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

        open( newunit=aunit, file=filename, form='formatted', action=aaction, &
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

        self % log_units(self % units + 1 ) = aunit
        self % units = self % units + 1
        if ( present(unit) ) unit = aunit
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

!! Adds `unit` to the log file units in `log_units`. `unit` must be an `OPEN`
!! file, of `FORM` `"FORMATTED"`, with `"SEQUENTIAL"` `ACCESS`, and an `ACTION` of
!! `"WRITE"` or `"READWRITE"`, otherwise either `stat`, if preseent, has a
!! value other than `success` and `unit` is not entered into L`log_units`,
!! or, if `stat` is not presecn, processing stops.
        class(logger_type), intent(inout) :: self
!! The logger variable to which the I/O unit is to be added
        integer, intent(in)               :: unit
!! The input logical unit number
        integer, intent(out), optional    :: stat
!! An error code with the possible values
!! * `success` - no problems were found
!! * `non_sequential_error` - `unit` did not have sequential access
!! * `read_only_error` - `unit` was not writeable
!! * `unformatted_in_error` - `unit` was an `"UNFORMATTED'` file
!! * `unopened_in_error` - `unit` was not opened

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


    pure subroutine configuration( self, add_blank_line, indent, &
        max_width, time_stamp, log_units )
!! version: experimental

!! Reports the logging configuration of `self`. The following attributes are
!! reported:
!! 1. `add_blank_line` is a logical flag with `.true.` implying that output
!!    starts with a blank line, and `.false.` implying no blank line.
!! 2. `indent` is a logical flag with `.true.` implying that subsequent columns
!!    will be indented 4 spaces and `.false.` implying no indentation.
!! 3. `max_width` is the maximum number of columns of output text with
!!    `max_width` == 0 => no bounds on output width.
!! 4. `time_stamp` is a logical flag with `.true.` implying that the output
!!    will have a time stamp, and `.false.` implying that there will be no
!!    time stamp.
!! 5. `log_units` is an array of the I/O unit numbers to which log output
!!    will be written
        class(logger_type), intent(in)              :: self
!! The logger variable whose configuration is being reported
        logical, intent(out), optional              :: add_blank_line
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


        if ( present(add_blank_line) ) &
            add_blank_line = self % add_blank_line
        if ( present(indent) ) indent = self % indent_lines
        if ( present(max_width) ) max_width = self % max_width
        if ( present(time_stamp) ) time_stamp = self % time_stamp
        if ( present(log_units) ) log_units = self % log_units(1:self % units)

    end subroutine configuration


    pure subroutine configure( self, add_blank_line, indent, max_width, &
        time_stamp )
!! version: experimental

!! Configures the logging process for SELF. The following attributes are
!! configured:
!! 1. `add_blank_line` is a logical flag with `.true.` implying that output
!!    starts with a blank line, and `.false.` implying no blank line.
!!    `add_blank_line` has a startup value of `.false.`.
!! 2. `indent` is a logical flag with `.true.` implying that subsequent lines
!!    will be indented 4 spaces and `.false.` implying no indentation. `indent`
!!    has a startup value of `.true.`.
!! 3. `max_width` is the maximum number of columns of output text with
!!    `max_wodth == 0` => no bounds on output width. `max_width` has a startup
!!    value of 0.
!! 4. `time_stamp` is a logical flag with `.true.` implying that the output
!!    will have a time stamp, and `.false.` implying that there will be no
!!    time stamp. `time_stamp` has a startup value of `.true.`.

!!##### Example
!!
!!     program main
!!         use stdlib_logger
!!         ...
!!         call global_logger % configure( indent=.false., max_width=72 )
!!         ...

        class(logger_type), intent(inout) :: self
        logical, intent(in), optional     :: add_blank_line
        logical, intent(in), optional     :: indent
        integer, intent(in), optional     :: max_width
        logical, intent(in), optional     :: time_stamp

        if ( present(add_blank_line) ) &
            self % add_blank_line = add_blank_line
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
!! version: experimental

!! finalizes the `logger_type` entity `self` by flushing the units
        type(logger_type), intent(in) :: self

        integer        :: iostat
        character(256) :: message
        integer        :: unit

        do unit=1, self % units
            flush( self % log_units(unit), iomsg=message, iostat=iostat )
            if ( iostat /= 0 ) then
                write(error_unit, '(a, i0)' ) 'In the logger_type finalizer ' // &
                    'an error occurred in flushing UNIT = ',                     &
                    self % log_units(unit)
                write(error_unit, '(a, i0)') 'With IOSTAT = ', iostat
                write(error_unit, '(a)') 'With IOMSG = ' // trim(message)

            end if

        end do

    end subroutine final_logger


    subroutine format_output_string( self, unit, string, procedure_name, &
                                     col_indent )
!! version: experimental

!! Writes the STRING to UNIT ensuring that the number of characters
!! does not exceed MAX_WIDTH and that the lines after the first
!! one are indented four characters.
        class(logger_type), intent(in) :: self
        integer, intent(in)            :: unit
        character(*), intent(in)       :: string
        character(*), intent(in)       :: procedure_name
        character(*), intent(in)       :: col_indent

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

!! Handles a failure to write to `unit` in `procedure_name` with `iostat` and
!! `iomsg` by writing a description of the failure to `output_unit` and
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
!! Writes the string `message` to `self % log_units` with optional additional
!! text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written, followed by
!! `module` and `procedure` if present, then `message` is
!! written with the prefix 'ERROR: ', and then if `stat` or `errmsg`
!! are present they are written.
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       real, allocatable :: a(:)
!!       ...
!!       type(logger_type) :: alogger
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

        class(logger_type), intent(in)          :: self
!! The logger to be used in logging the message
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of `log_error`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of `log_error`
        integer, intent(in), optional           :: stat
!! The value of the `STAT` specifier returned by a Fortran statement
        character(len=*), intent(in), optional  :: errmsg
!! The value of the `ERRMSG` specifier returned by a Fortran statement

        integer :: unit
        integer :: iostat
        character(*), parameter :: procedure_name = 'LOG_ERROR'
        character(256) :: iomsg

        call self % log_message( message,               &
                                 module = module,       &
                                 procedure = procedure, &
                                 prefix = 'ERROR')

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
!! Writes the string `message` to `self % log_units` with optional additional
!! text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written, followed by
!! `module` and `procedure` if present, and then `message` is
!! written with the prefix 'INFO: '.
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       real, allocatable :: a(:)
!!       ...
!!       type(logger_type) :: alogger
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

        class(logger_type), intent(in)          :: self
!! The logger used to send the message
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of `log_information`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of `log_information`

        call self % log_message( message,               &
                                 module = module,       &
                                 procedure = procedure, &
                                 prefix = 'INFO' )

    end subroutine log_information


    subroutine log_io_error( self, message, module, procedure, iostat, &
                             iomsg )
!! Writes the string `message to the `self % log_units` with optional
!! additional text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written, followed by
!! `module` and `procedure` if present, then `message` is
!! written with a prefix 'I/O ERROR: ', and then if `iostat` or `iomsg`
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

        class(logger_type), intent(in)          :: self
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

        call self % log_message( message,               &
                                 module = module,       &
                                 procedure = procedure, &
                                 prefix = 'I/O ERROR' )

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

    subroutine log_message( self, message, module, procedure, prefix )
!! version: experimental

!! Writes the string `message` to the `self % log_units` with optional
!! additional text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written, followed by `module`
!! and `procedure` if present, followed by `prefix // ': '` if present,
!! and then `message`.
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
!!                                          procedure = 'EXAMPLE_SUB', &
!!                                          prefix = 'INFO' )
!!      end subroutine example_sub
!!      ...
!!    end module example_mod
!!

        class(logger_type), intent(in)          :: self
!! The logger variable to receive the message
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of `log_message`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of `log_message`
        character(len=*), intent(in), optional  :: prefix
!! To be prepended to message as `prefix // ': ' // message`.

        integer :: unit
        integer :: iostat
        character(*), parameter :: procedure_name = 'LOG_MESSAGE'
        character(256) :: iomsg
        character(:), allocatable :: d_and_t, m_and_p, pref

        if ( present(prefix) ) then
            pref = prefix // ': '

        else
            pref = ''

        end if

        if ( self % time_stamp ) then
            d_and_t = time_stamp() // ': '

        else
            d_and_t = ''

        end if

        if ( present(module) ) then
            if ( present(procedure) ) then
                m_and_p = trim(module) // ' % ' // trim(procedure) // ': '

            else
                m_and_p = trim(module) // ': '

            end if

        else if ( present(procedure) ) then
            m_and_p = trim(procedure) // ': '

        else
            m_and_p = ''

        end if

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

            if ( self % add_blank_line ) write( unit, *, err=999, &
                iostat=iostat, iomsg=iomsg )

            call format_output_string( self, unit,                   &
                                       d_and_t // m_and_p // pref // &
                                       trim( message ),              &
                                       procedure_name, '    ' )

            return

999         call handle_write_failure( unit, procedure_name, iostat, iomsg )

        end subroutine write_log_message

    end subroutine log_message

    subroutine log_text_error( self, line, column, summary, filename,  &
                               line_number, caret, stat )
!! version: experimental

!! `log_text_error` sends a message to `self % log_units` describing an error found
!! in a line of text.
!!
!!##### Behavior
!!
!! If time stamps are active first a time stamp is written. Then if
!! `filename` or `line_number` or `column` are present they are written.
!! Then `line` is written. Then the symbol `caret` is written below `line` at the
!! column indicated by `column`. Then `summary` is written.
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
        class(logger_type), intent(in)        :: self
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
!! The one's based line number in the file where `line` was found.
        character(1), intent(in), optional    :: caret
!! The symbol used to mark the column wher the error was first detected
        integer, intent(out), optional        :: stat
!! Integer flag that an error has occurred. Has the value `success` if no
!! error hass occurred, `invalid_index` if `column` is less than zero or
!! greater than `len(line)`, and `write_failure` if any of the `WRITE` statements
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

            if ( self % add_blank_line ) write( unit, * )

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
!! Returns the number of units assigned to `self % log_units`
        class(logger_type), intent(in) :: self
!! The logger subject to the inquiry
        integer                        :: log_units_assigned
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       type(logger_type) :: alogger
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
!! Writes the string `message` to `self % log_units` with optional additional text.
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written, followed by
!! `module` and `procedure` if present, then `message` is
!! written with the prefix 'WARN: '.
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       real, allocatable :: a(:)
!!       ...
!!       type(logger_type) :: alogger
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
        class(logger_type), intent(in)          :: self
!! The logger to which the message is written
        character(len=*), intent(in)            :: message
!! A string to be written to LOG_UNIT
        character(len=*), intent(in), optional  :: module
!! The name of the module contining the current invocation of `log_warning`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure contining the current invocation of `log_warning`

        call self % log_message( message,               &
                                 module = module,       &
                                 procedure = procedure, &
                                 prefix = 'WARN' )

    end subroutine log_warning


    subroutine remove_log_unit( self, unit, close_unit, stat )
!! Remove the I/O UNIT from the SELF % LOG_UNITS list. If CLOSE_UNIT is
!! present and .TRUE. then the corresponding file is closed. If UNIT is
!! not in LOG_UNITS then nothing is done. If STAT is present it, by default,
!! has the value SUCCESS. If closing the UNIT fails, then if STAT is
!! present it has the value CLOSE_FAILURE, otherwise processing stops
!! with an informative message.
        class(logger_type), intent(inout) :: self
!! The logger variable whose unit is to be removed
        integer, intent(in)               :: unit
!! The I/O unit to be removed from SELF
        logical, intent(in), optional     :: close_unit
!! A logical flag to close the unit while removing it from the SELF list
        integer, intent(out), optional    :: stat
!! An error status with the values
!! * SUCCESS - no problems found
!! * CLOSE_FAILURE - the CLOSE statement for UNIT failed
!!
!!##### Example
!!
!!     module  example_mod
!!       use stdlib_logger
!!       ...
!!       type(logger_type) ::  alogger
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
