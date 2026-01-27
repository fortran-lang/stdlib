module stdlib_logger
!!### Module stdlib_logger
!!
!! This module defines a derived type, procedures, a variable, and
!! constants to be used for logging information and reporting errors
!! in Fortran applications.
!!([Specification](../page/specs/stdlib_logger.html))

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
!! The variable `global_logger` of type `logger_type` can be used
!! as a default global logger anywhere in the source code.
!!
!! The constants are used to report errors by some of the subroutines
!! in their optional `stat` arguments. The constants are as follows.
!! `success` indicates that no error has occurred. `close_failure`
!! indicates that a `close` statement for an I/O unit failed.
!! `index_invalid_error` indicates that `column` was invalid for
!! the given `line`. `open_failure` indicates that an `open` statement
!! failed. `read_only_error` indicates that an output unit did not have a
!! `"write"` or `"readwrite"` action. `non_sequential_error` indicates
!! that the unit did not have `sequential` access. `unformatted_in_error`
!! indicates that the unit did not have a `form` of `"formatted"`.
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
        index_invalid_error = 2,  &
        non_sequential_error = 3, &
        open_failure = 4,         &
        read_only_error = 5,      &
        unformatted_in_error = 6, &
        unopened_in_error = 7,    &
        write_failure = 8

    integer, parameter, public ::      &
        debug_level = 10,       &
        information_level = 20, &
        warning_level = 30,     &
        error_level = 40,       &
        io_error_level = 40,    &
        text_error_level = 50,  &
        all_level = -10 + min(  &
            debug_level,        &
            information_level,  &
            warning_level,      &
            error_level,        &
            io_error_level,     &
            text_error_level),  &
        none_level = 10 + max(  &
            debug_level,        &
            information_level,  &
            warning_level,      &
            error_level,        &
            io_error_level,     &
            text_error_level)

    character(*), parameter :: module_name = 'stdlib_logger'

    type :: logger_type
        !! version: experimental
        
        !! Public derived type ([Specification](../page/specs/stdlib_logger.html#the-derived-type-logger_type))
        private

        logical                   :: add_blank_line = .false.
        logical                   :: indent_lines = .true.
        integer                   :: level = information_level
        integer, allocatable      :: log_units(:)
        integer                   :: max_width = 0
        logical                   :: time_stamp = .true.
        integer                   :: units = 0

    contains

        private

        procedure, public, pass(self) :: add_log_file
        procedure, public, pass(self) :: add_log_unit
        procedure, public, pass(self) :: configuration
        procedure, public, pass(self) :: configure
        procedure, public, pass(self) :: log_debug
        procedure, public, pass(self) :: log_error
        procedure, public, pass(self) :: log_information
        procedure, public, pass(self) :: log_io_error
        procedure, public, pass(self) :: log_message
        procedure, public, pass(self) :: log_text_error
        procedure, public, pass(self) :: log_units_assigned
        procedure, public, pass(self) :: log_warning
        procedure, public, pass(self) :: remove_log_unit

        final :: final_logger

    end type logger_type

    !! Variable of type `logger_type` to be used as a global logger
    type(logger_type) :: global_logger

    character(*), parameter :: &
        invalid_column = 'column is not a valid index to line.'

contains

    subroutine add_log_file( self, filename, unit, action, position, status, &
                             stat )
!! version: experimental

!! Opens a formatted sequential access output file, `filename` using
!! `newunit` and adds the resulting unit number to `self`'s `log_units`
!! array. `action`, if present, is the `action` specifier of the `open`
!! statement, and has the default value of `"write"`. `position`, if present,
!! is the `position` specifier, and has the default value of `"REWIND"`.
!! `status`, if present, is the `status` specifier of the `open` statement,
!! and has the default value of `"REPLACE"`. `stat`, if present, has the value
!! `success` if `filename` could be opened, `read_only_error` if `action` is
!! `"read"`, and `open_failure` otherwise.
!!([Specification](../page/specs/stdlib_logger.html#add_log_file-open-a-file-and-add-its-unit-to-self-log_units))
        class(logger_type), intent(inout)  :: self
!! The logger variable to which the file is to be added
        character(*), intent(in)           :: filename
!! The name of the file to be  added to the logger
        integer, intent(out), optional     :: unit
!! The resulting I/O unit number
        character(*), intent(in), optional :: action
!! The `action` specifier for the `open`` statement
        character(*), intent(in), optional :: position
!! The `position` specifier for the `open` statement
        character(*), intent(in), optional :: status
!! The `status` specifier for the  `open`  statement
        integer, intent(out), optional     :: stat
!! The error status on exit with the possible values
!! * `success` - no errors found
!! * `read_only_error` - file unopened as `action1 was `"read"` for an output
!!   file
!! * `open_failure` - the `open` statement failed


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
                        procedure_name // ' action is "read" which ' // &
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

!! Adds `unit` to the log file units in `log_units`. `unit` must be an `open`
!! file, of `form` `"formatted"`, with `"sequential"` `access`, and an `action`
!! of `"write"` or `"readwrite"`, otherwise either `stat`, if present, has a
!! value other than `success` and `unit` is not entered into `log_units`,
!! or, if `stat` is not presecn, processing stops.
!!([Specification](../page/specs/stdlib_logger.html#add_log_unit-add-a-unit-to-the-array-self-log_units))

        class(logger_type), intent(inout) :: self
!! The logger variable to which the I/O unit is to be added
        integer, intent(in)               :: unit
!! The input logical unit number
        integer, intent(out), optional    :: stat
!! An error code with the possible values
!! * `success` - no problems were found
!! * `non_sequential_error` - `unit` did not have sequential access
!! * `read_only_error` - `unit` was not writeable
!! * `unformatted_in_error` - `unit` was an `'unformatted'` file
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
!!               status='replace', position='rewind', err=999,    &
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
        character(*), parameter :: procedure_name = 'set_log_unit'
        integer :: lun
        character(12) :: specifier
        logical :: question
        integer :: istat

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

! Check that unit is not input_unit
            if ( unit == input_unit ) then
                if ( present(stat) ) then
                    stat = read_only_error
                    return
                else
                    error stop 'unit in ' // module_name // ' % ' // &
                        procedure_name // ' must not be input_unit.'
                end if
            end if

! Check that unit is opened
            inquire( unit, opened=question, iostat=istat )
            if(istat /= 0) question = .false.
            if ( .not. question ) then
                if ( present(stat) ) then
                    stat = unopened_in_error
                    return
                else
                    error stop 'unit in ' // module_name // ' % ' // &
                        procedure_name // ' is not open.'
                end if
            end if

! Check that unit is writeable
            inquire( unit, write=specifier )
            if ( specifier(1:1) /= 'Y' .and. specifier(1:1) /= 'y' ) then
                if ( present(stat) ) then
                    stat = read_only_error
                    return
                else
                    error stop 'unit in ' // module_name // ' % ' // &
                        procedure_name // ' is not writeable.'
                end if
            end if

            inquire( unit, sequential=specifier )
            if ( specifier(1:1) /= 'Y' .and. specifier(1:1) /= 'y' ) then
                if ( present(stat) ) then
                    stat = non_sequential_error
                    return
                else
                    error stop 'unit in ' // module_name // ' % ' // &
                        procedure_name // ' is not "sequential".'
                end if
            end if

            inquire( unit, formatted=specifier )
            if ( specifier(1:1) /= 'Y' .and. specifier(1:1) /= 'y' ) then
                if ( present(stat) ) then
                    stat = unformatted_in_error
                    return
                else
                    error stop 'unit in ' // module_name // ' % ' // &
                        procedure_name // ' is not "formatted".'
                end if
            end if

            if ( present(stat) ) stat = success

        end subroutine validate_unit

    end subroutine add_log_unit


    pure subroutine configuration( self, add_blank_line, indent, level, &
        max_width, time_stamp, log_units )
!! version: experimental

!! Reports the logging configuration of `self`. The following attributes are
!! reported:
!! 1. `add_blank_line` is a logical flag with `.true.` implying that output
!!    starts with a blank line, and `.false.` implying no blank line.
!! 2. `indent` is a logical flag with `.true.` implying that subsequent columns
!!    will be indented 4 spaces and `.false.` implying no indentation.
!! 3. `level` is the lowest level for printing a message
!! 4. `max_width` is the maximum number of columns of output text with
!!    `max_width` == 0 => no bounds on output width.
!! 5. `time_stamp` is a logical flag with `.true.` implying that the output
!!    will have a time stamp, and `.false.` implying that there will be no
!!    time stamp.
!! 6. `log_units` is an array of the I/O unit numbers to which log output
!!    will be written.
!!([Specification](../page/specs/stdlib_logger.html#configuration-report-a-loggers-configuration))

        class(logger_type), intent(in)              :: self
!! The logger variable whose configuration is being reported
        logical, intent(out), optional              :: add_blank_line
!! A logical flag to add a preceding blank line
        logical, intent(out), optional              :: indent
!! A logical flag to indent subsequent lines
        integer, intent(out), optional              :: level
!! The minimum level for printing a message
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

        if ( present(add_blank_line) ) add_blank_line = self % add_blank_line
        if ( present(indent) ) indent = self % indent_lines
        if ( present(level) ) level = self % level
        if ( present(max_width) ) max_width = self % max_width
        if ( present(time_stamp) ) time_stamp = self % time_stamp
        if ( present(log_units) ) then
            if ( self % units .gt. 0 ) then
                log_units = self % log_units(1:self % units)
            else
                allocate(log_units(0))
            end if
        end if

    end subroutine configuration


    pure subroutine configure( self, add_blank_line, indent, level, max_width, &
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
!! 3. `level` is the lowest level for printing a message
!! 4. `max_width` is the maximum number of columns of output text with
!!    `max_width == 0` => no bounds on output width. `max_width` has a startup
!!    value of 0.
!! 5. `time_stamp` is a logical flag with `.true.` implying that the output
!!    will have a time stamp, and `.false.` implying that there will be no
!!    time stamp. `time_stamp` has a startup value of `.true.`.
!!([Specification](../page/specs/stdlib_logger.html#configure-configure-the-logging-process))
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
        integer, intent(in), optional     :: level
        integer, intent(in), optional     :: max_width
        logical, intent(in), optional     :: time_stamp

        if ( present(add_blank_line) ) self % add_blank_line = add_blank_line
        if ( present(level) ) self % level = level
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

!! Finalizes the `logger_type` entity `self` by flushing the units
        type(logger_type), intent(in) :: self

        integer        :: iostat
        character(256) :: message
        integer        :: unit

        do unit=1, self % units
            flush( self % log_units(unit), iomsg=message, iostat=iostat )
            if ( iostat /= 0 ) then
                write(error_unit, '(a, i0)' ) 'In the logger_type ' // &
                    'finalizer an error occurred in flushing unit = ', &
                    self % log_units(unit)
                write(error_unit, '(a, i0)') 'With iostat = ', iostat
                write(error_unit, '(a)') 'With iomsg = ' // trim(message)
            end if
        end do

    end subroutine final_logger


    subroutine format_output_string( self, string, col_indent, len_buffer, buffer )
!! version: experimental

!! Writes the STRING to UNIT ensuring that the number of characters
!! does not exceed MAX_WIDTH and that the lines after the first
!! one are indented four characters.
        class(logger_type), intent(in)             :: self
        character(*), intent(in)                   :: string
        character(*), intent(in)                   :: col_indent
        integer, intent(out)                       :: len_buffer
        character(len=:), allocatable, intent(out) :: buffer

        integer :: count, indent_len, index_, length, remain
        integer, parameter :: new_len = len(new_line('a'))

        length = len_trim(string)
        allocate( character(2*length) :: buffer )
        len_buffer = 0
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

            if ( self % max_width == 0 .or.                     &
                ( length <= self % max_width .and.              &
                index( string(1:length), new_line('a')) == 0 ) ) then
                buffer(1:length) = string(1:length)
                len_buffer = length
                remain = 0
                return
            else

                index_ = index( string(1:min(length, self % max_width)), &
                                new_line('a') )
                if ( index_ == 0 ) then
                    do index_=self % max_width, 1, -1
                        if ( string(index_:index_) == ' ' ) exit
                    end do
                end if

                if ( index_ == 0 ) then
                    buffer(1:self % max_width) = &
                        string(1:self % max_width)
                    len_buffer = self % max_width
                    count = self % max_width
                    remain = length - count
                    return
                else
                    buffer(1:index_-1) = string(1:index_-1)
                    len_buffer = index_-1
                    count = index_
                    remain = length - count
                    return
                end if

            end if

        end subroutine format_first_line

        subroutine format_subsequent_line()
            integer :: new_len_buffer
            character(:), allocatable :: dummy

            if ( remain <= self % max_width ) then
                new_len_buffer = len_buffer + length - count + new_len
                if ( new_len_buffer > len( buffer ) ) then
                    allocate( character( 2*len( buffer ) ) :: dummy )
                    dummy = buffer
                    call move_alloc( dummy, buffer )
                end if
                buffer( len_buffer+1:new_len_buffer ) = &
                    new_line('a') // string(count+1:length)
                len_buffer = new_len_buffer
                count = length
                remain = 0
                return
            else

                index_ = count + index(string(count+1:count+self % max_width),&
                    new_line('a'))
                if(index_ == count) then
                    do index_=count+self % max_width, count+1, -1
                        if ( string(index_:index_) == ' ' ) exit
                    end do
                end if

                if ( index_ == count ) then
                    new_len_buffer = len_buffer + self % max_width + &
                        new_len
                    if ( new_len_buffer > len( buffer ) ) then
                        allocate( character( 2*len( buffer ) ) :: dummy )
                        dummy = buffer
                        call move_alloc( dummy, buffer )
                    end if
                    buffer( len_buffer+1:new_len_buffer ) = &
                        new_line('a') // string(count+1:count+self % max_width)
                    len_buffer = new_len_buffer
                    count = count + self % max_width
                    remain = length - count
                    return
                else
                    new_len_buffer = len_buffer + index_ - 1 &
                        - count + new_len
                    if ( new_len_buffer > len( buffer ) ) then
                        allocate( character( 2*len( buffer ) ) :: dummy )
                        dummy = buffer
                        call move_alloc( dummy, buffer )
                    end if
                    buffer( len_buffer+1:new_len_buffer ) = &
                        new_line('a') // string(count+1:index_-1)
                    len_buffer = new_len_buffer
                    count = index_
                    remain = length - count
                    return
                end if

            end if

        end subroutine format_subsequent_line

        subroutine indent_format_subsequent_line()
            integer :: new_len_buffer
            character(:), allocatable :: dummy

            if ( index( string(count+1:length), new_line('a')) == 0 .and. &
                remain <= self % max_width - indent_len ) then
                new_len_buffer = len_buffer + length &
                    - count + new_len + indent_len
                if ( new_len_buffer > len( buffer ) ) then
                    allocate( character( 2*len( buffer ) ) :: dummy )
                    dummy = buffer
                    call move_alloc( dummy, buffer )
                end if
                buffer( len_buffer+1:new_len_buffer ) = &
                    new_line('a') // col_indent // string(count+1:length)
                len_buffer = new_len_buffer
                count = length
                remain = 0
                return
            else

                index_ = count + index( string(count+1:                   &
                    min ( length, count+self % max_width - indent_len) ), &
                    new_line('a'))
                if(index_ == count) then
                    do index_=count+self % max_width-indent_len, count+1, -1
                        if ( string(index_:index_) == ' ' ) exit
                    end do
                end if

                if ( index_ == count ) then
                    new_len_buffer = len_buffer + self % max_width &
                        + new_len 
                    if ( new_len_buffer > len( buffer ) ) then
                        allocate( character( 2*len( buffer ) ) :: dummy )
                        dummy = buffer
                        call move_alloc( dummy, buffer )
                    end if
                    buffer( len_buffer+1: new_len_buffer ) = &
                        new_line('a') // col_indent // &
                        string(count+1:count+self % max_width-indent_len)
                    len_buffer = new_len_buffer
                    count = count + self % max_width - indent_len
                    remain = length - count
                    return
                else
                    new_len_buffer = len_buffer + index_ - count - 1 &
                        + new_len + indent_len
                    if ( new_len_buffer > len( buffer ) ) then
                        allocate( character( 2*len( buffer ) ) :: dummy )
                        dummy = buffer
                        call move_alloc( dummy, buffer )
                    end if
                    buffer( len_buffer+1: new_len_buffer ) = &
                        new_line('a') // col_indent // string(count+1:index_-1)
                    len_buffer = new_len_buffer
                    count = index_
                    remain = length - count
                    return
                end if

            end if

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

         write( output_unit, '(a)' ) 'write failure in ' // module_name // &
             ' % ' // trim(procedure_name) // '.'
         if ( unit == -999 ) then
             write( output_unit, '(a, i0)' ) 'unit = internal file'
         else
             write( output_unit, '(a, i0)' ) 'unit = ', unit
             inquire( unit, named=named )

             if ( named ) then
                 inquire( unit, name=name )
                 write( output_unit, '(a, a)' ) 'name = ', trim(name)
             else
                 write( output_unit, '(a)' ) 'unit is unnamed'
             end if
             inquire( unit, action=action )
             write( output_unit, '(a, a)' ) 'action = ', trim(action)
         end if

         write( output_unit, '(a, i0)' ) 'iostat = ', iostat
         write( output_unit, '(a, a )' ) 'iomsg = ', trim(iomsg)
         error stop 'write failure in ' // module_name // '.'

     end subroutine handle_write_failure


    subroutine log_debug( self, message, module, procedure )
!! version: experimental

!! Writes the string `message` to `self % log_units` with optional additional
!! text.
!!([Specification](../page/specs/stdlib_logger.html#log_debug-writes-the-string-message-to-self-log_units))
!!
!!##### Behavior
!!
!! If time stamps are active, a time stamp is written, followed by
!! `module` and `procedure` if present, and then `message` is
!! written with the prefix 'DEBUG: '.
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
!!         call alogger % log_debug( message,                   &
!!                                   module = 'EXAMPLE_MOD',    &
!!                                   procedure = 'EXAMPLE_SUB' )
!!         ...
!!       end subroutine example_sub
!!       ...
!!     end module example_mod
!!

        class(logger_type), intent(in)          :: self
!! The logger used to send the message
        character(len=*), intent(in)            :: message
!! A string to be written to log_unit
        character(len=*), intent(in), optional  :: module
!! The name of the module containing the current invocation of `log_information`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure containing the current invocation of
!! `log_information`

        if ( self % level > debug_level ) return

        call self % log_message( message,               &
                                 module = module,       &
                                 procedure = procedure, &
                                 prefix = 'DEBUG' )

    end subroutine log_debug


    subroutine log_error( self, message, module, procedure, stat, errmsg )
!! version: experimental

!! Writes the string `message` to `self % log_units` with optional additional
!! text.
!! ([Specification](../specs/stdlib_logger.html#log_error-writes-the-string-message-to-self-log_units))

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
!! A string to be written to log_unit
        character(len=*), intent(in), optional  :: module
!! The name of the module containing the current invocation of `log_error`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure containing the current invocation of `log_error`
        integer, intent(in), optional           :: stat
!! The value of the `stat` specifier returned by a Fortran statement
        character(len=*), intent(in), optional  :: errmsg
!! The value of the `errmsg` specifier returned by a Fortran statement

        integer :: iostat
        character(28) :: dummy
        character(256) :: iomsg
        character(*), parameter :: procedure_name = 'log_error'
        character(:), allocatable :: suffix

        if ( self % level > error_level ) return

        if ( present(stat) ) then
            write( dummy, '(a, i0)', err=999, iostat=iostat, iomsg=iomsg ) &
                new_line('a') // "With stat = ", stat
        else
            dummy = ' '
        end if

        if ( present(errmsg) ) then
            if ( len_trim(errmsg) > 0 ) then
                suffix = trim(dummy) // &
                    new_line('a') // 'With errmsg = "' // trim(errmsg) // '"'
            else
                suffix = dummy
            end if
        else
            suffix = dummy
        end if

        call self % log_message( trim(message) // suffix, &
                                 module = module,         &
                                 procedure = procedure,   &
                                 prefix = 'ERROR')

        return

999     call handle_write_failure( -999, procedure_name, iostat, iomsg )

    end subroutine log_error


    subroutine log_information( self, message, module, procedure )
!! version: experimental

!! Writes the string `message` to `self % log_units` with optional additional
!! text.
!!([Specification](../page/specs/stdlib_logger.html#log_information-writes-the-string-message-to-self-log_units))
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
!! A string to be written to log_unit
        character(len=*), intent(in), optional  :: module
!! The name of the module containing the current invocation of `log_information`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure containing the current invocation of
!! `log_information`

        if ( self % level > information_level ) return

        call self % log_message( message,               &
                                 module = module,       &
                                 procedure = procedure, &
                                 prefix = 'INFO' )

    end subroutine log_information


    subroutine log_io_error( self, message, module, procedure, iostat, &
                             iomsg )
!! version: experimental

!! Writes the string `message` to the `self % log_units` with optional
!! additional text.
!!([Specification](../page/specs/stdlib_logger.html#log_io_error-write-the-string-message-to-self-log_units))
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
!! The name of the module containing the current invocation of REPORT_ERROR
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure containing the current invocation of REPORT_ERROR
        integer, intent(in), optional           :: iostat
!! The value of the IOSTAT specifier returned by a Fortran I/O statement
        character(len=*), intent(in), optional  :: iomsg
!! The value of the IOMSG specifier returned by a Fortran I/O statement

        character(28) :: dummy
        character(256) :: iomsg2
        integer :: iostat2
        character(*), parameter :: procedure_name = 'log_io_error'
        character(:), allocatable :: suffix

        if ( self % level > io_error_level ) return

        if ( present(iostat) ) then
            write( dummy, '(a, i0)', err=999, iostat=iostat2, iomsg=iomsg2 ) &
                new_line('a') // "With iostat = ", iostat
        else
            dummy = ' '
        end if

        if ( present(iomsg) ) then
            if ( len_trim(iomsg) > 0 ) then
                suffix = trim(dummy) // &
                    new_line('a') // 'With iomsg = "' // trim(iomsg) // '"'
            else
                suffix = trim(dummy)
            end if
        else
            suffix = trim(dummy)
        end if

        call self % log_message( trim(message) // suffix, &
                                 module = module,         &
                                 procedure = procedure,   &
                                 prefix = 'I/O ERROR' )

        return

999     call handle_write_failure( -999, procedure_name, iostat2, iomsg2 )

    end subroutine log_io_error

    subroutine log_message( self, message, module, procedure, prefix )
!! version: experimental

!! Writes the string `message` to the `self % log_units` with optional
!! additional text.
!!([Specification](../page/specs/stdlib_logger.html#log_message-write-the-string-message-to-self-log_units))
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
!!                                          module = 'example_mod',    &
!!                                          procedure = 'example_sub', &
!!                                          prefix = 'info' )
!!      end subroutine example_sub
!!      ...
!!    end module example_mod
!!

        class(logger_type), intent(in)          :: self
!! The logger variable to receive the message
        character(len=*), intent(in)            :: message
!! A string to be written to log_unit
        character(len=*), intent(in), optional  :: module
!! The name of the module containing the current invocation of `log_message`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure containing the current invocation of `log_message`
        character(len=*), intent(in), optional  :: prefix
!! To be prepended to message as `prefix // ': ' // message`.

        integer :: unit
        integer :: iostat
        integer :: len_buffer
        character(*), parameter :: procedure_name = 'log_message'
        character(256) :: iomsg
        character(:), allocatable :: d_and_t, m_and_p, pref
        character(:), allocatable :: buffer

        pref = optval(prefix, '')
        if ( len(pref) > 0 ) pref = pref // ': '

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

        call format_output_string( self,                         &
                                   d_and_t // m_and_p // pref // &
                                   trim( message ),              &
                                   '    ',                       &
                                   len_buffer,                   &
                                   buffer)

        if ( self % units == 0 ) then
            if ( self % add_blank_line ) then
                write( output_unit, '(a)', err=999, iostat=iostat, &
                        iomsg=iomsg) &
                    new_line('a') // buffer(1:len_buffer)
            else
                write( output_unit, '(a)', err=999, iostat=iostat, &
                        iomsg=iomsg ) &
                    buffer(1:len_buffer)
            end if
        else
            if ( self % add_blank_line ) then
                do unit=1, self % units
                    write( self % log_units(unit), '(a)', err=999, iostat=iostat, &
                        iomsg=iomsg ) new_line('a') // &
                        buffer(1:len_buffer)
                end do
            else
                do unit=1, self % units
                    write( self % log_units(unit), '(a)', err=999, iostat=iostat, &
                        iomsg=iomsg ) &
                        buffer(1:len_buffer)
                end do
            end if
        end if


        return

999     call handle_write_failure( unit, procedure_name, iostat, iomsg )

    end subroutine log_message

    subroutine log_text_error( self, line, column, summary, filename,  &
                               line_number, caret, stat )
!! version: experimental

!! Sends a message to `self % log_units` describing an error found
!! in a line of text.
!!([Specification](../page/specs/stdlib_logger.html#log_text_error-send-a-message-to-self-log_units-describing-an-error))

!!##### Behavior
!!
!! If time stamps are active first a time stamp is written. Then if
!! `filename` or `line_number` or `column` are present they are written.
!! Then `line` is written. Then the symbol `caret` is written below `line`
!! at the column indicated by `column`. Then `summary` is written.
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
!! error hass occurred, `index_invalid_error` if `column` is less than zero or
!! greater than `len(line)`, and `write_failure` if any of the `write`
!! statements has failed.

        character(1)                  :: acaret
        character(128)                :: iomsg
        integer                       :: iostat
        integer                       :: lun
        character(*), parameter       :: procedure_name = 'LOG_TEXT_ERROR'
        character(len=:), allocatable :: buffer

        if ( self % level > text_error_level ) return

        acaret = optval(caret, '^')

        if ( column < 0 .or. column > len( line ) + 1 ) then
            if ( present(stat) ) then
                stat = index_invalid_error
                return
            else
                call self % log_error( invalid_column,           &
                                       module = module_name,     &
                                       procedure = procedure_name )
                error stop module_name // ' % ' // procedure_name // ': ' // &
                    invalid_column
            end if
        end if

        call write_log_text_error_buffer( )
        if ( self % units == 0 ) then
            write( output_unit, '(a)' ) buffer
        else
            do lun=1, self % units
                write( self % log_units(lun), '(a)' ) buffer
            end do
        end if

    contains

        subroutine write_log_text_error_buffer( )
            integer                   :: i
            character(:), allocatable :: location, marker

            if ( present(filename) ) then
                if ( present(line_number) ) then
                    allocate( character(len_trim(filename)+15) :: location )
                    write( location, fmt='(a, ":", i0, ":", i0)', err=999, &
                           iomsg=iomsg, iostat=iostat )           &
                           trim(filename) , line_number, column
                else
                    allocate( character(len_trim(filename)+45) :: location )
                    write( location, fmt='(a, i0)', err=999, iomsg=iomsg, &
                           iostat=iostat ) &
                           "Error found in file: '" // trim(filename) // &
                           "', at column: ", column
                end if

            else
                if ( present(line_number) ) then
                    allocate( character(54) :: location )
                    write( location, fmt='(a, i0, a, i0)', err=999, &
                           iomsg=iomsg, iostat=iostat ) &
                        'Error found at line number: ', line_number, &
                        ', and column: ', column
                else
                    allocate( character(36) :: location )
                    write( location, &
                           fmt='("Error found in line at column:", i0)' ) &
                        column
                end if
            end if

            allocate( character(column) :: marker )
            do i=1, column-1
                marker(i:i) = ' '
            end do
            marker(column:column) = acaret
            if ( self % add_blank_line ) then
                if ( self % time_stamp ) then
                    buffer = new_line('a') // time_stamp() // &
                        new_line('a') // trim(location) // &
                        new_line('a') // new_line('a') // trim(line) // &
                        new_line('a') // marker // &
                        new_line('a') // 'Error: ' // trim(summary)
                else
                    buffer = new_line('a') // trim(location) // &
                        new_line('a') // new_line('a') // trim(line) // &
                        new_line('a') // marker // &
                        new_line('a') // 'Error: ' // trim(summary)
                end if
            else
                if ( self % time_stamp ) then
                    buffer = time_stamp() // &
                        new_line('a') // trim(location) // &
                        new_line('a') // new_line('a') // trim(line) // &
                        new_line('a') // marker // &
                        new_line('a') // 'Error: ' // trim(summary)
                else
                    buffer = trim(location) // &
                        new_line('a') // new_line('a') // trim(line) // &
                        new_line('a') // marker // &
                        new_line('a') // 'Error: ' // trim(summary)
                end if
            end if

            if ( present(stat) ) stat = success

            return

999         if ( present( stat ) ) then
                stat = write_failure
                return
            else
                call handle_write_failure( -999, procedure_name, iostat, &
                                           iomsg )
            end if

        end subroutine write_log_text_error_buffer

    end subroutine log_text_error


    elemental function log_units_assigned(self)
!! version: experimental

!! Returns the number of units assigned to `self % log_units`
!!([Specification](../page/specs/stdlib_logger.html#log_units_assigned-returns-the-number-of-active-io-units))

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
!! version: experimental

!! Writes the string `message` to `self % log_units` with optional additional
!! text.
!!([Specification](../page/specs/stdlib_logger.html#log_warning-write-the-string-message-to-log_units))

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
!! The name of the module containing the current invocation of `log_warning`
        character(len=*), intent(in), optional  :: procedure
!! The name of the procedure containing the current invocation of `log_warning`

        if ( self % level > warning_level ) return

        call self % log_message( message,               &
                                 module = module,       &
                                 procedure = procedure, &
                                 prefix = 'WARN' )

    end subroutine log_warning


    subroutine remove_log_unit( self, unit, close_unit, stat )
!! version: experimental

!! Remove the I/O unit from the self % log_units list. If `close_unit` is
!! present and `.true.` then the corresponding file is closed. If `unit` is
!! not in `log_units` then nothing is done. If `stat` is present it, by
!! default, has the value `success`. If closing the `unit` fails, then if
!! `stat` is present it has the value `close_failure`, otherwise processing
!! stops with an informative message.
!!([Specification](../page/specs/stdlib_logger.html#remove_log_unit-remove-unit-from-self-log_units))

        class(logger_type), intent(inout) :: self
!! The logger variable whose unit is to be removed
        integer, intent(in)               :: unit
!! The I/O unit to be removed from self
        logical, intent(in), optional     :: close_unit
!! A logical flag to close the unit while removing it from the SELF list
        integer, intent(out), optional    :: stat
!! An error status with the values
!! * success - no problems found
!! * close_failure - the close statement for unit failed
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
                procedure_name // ' close_unit failed for unit = ', unit
            write(*, '(a)' ) 'With iomsg = ' // trim(errmsg)
            error stop 'close_unit failed in ' // module_name // ' % ' // &
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
