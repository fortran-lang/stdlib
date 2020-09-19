---
title: logger
---
# Loggers

[TOC]

## Introduction

This module defines a derived type, its methods, a variable, and
constants to be used for the reporting of errors and other
information. The derived type, `logger_type`, is to be used to define
both global and local logger variables. The `logger_type` methods serve
to configure the loggers and use the logger variables to report
messages to a variable specific list of I/O units termed
`log_units`. The variable, `global_logger`, of type `logger_type`, is
intended to serve as the default global logger. The constants serve as
error flags returned by the optional integer `stat` argument.

The logger variables have the option to:

* change which units receive the log messages;
* report which units receive the log messages;
* precede messages by a blank line;
* precede messages by a time stamp of the form
  `yyyy-mm-dd hh:mm:ss.sss`;
* precede messages with the names of a module and procedure;
* follow a message with the `stat` and `errmsg` of the error report
  that prompted the log message;
* follow a message with the `iostat` and `iomsg` of the I/O error
  report that prompted the log message;
* label a message with one of `'INFO: '`, `'WARN: '`,
  `'ERROR: '`, or `'I/O ERROR: '`;
* indent subsequent lines of the messages; and
* format the text to fit within a maximum column width.

Note: Loggers of type `logger_type` normally report their messages to I/O
units in the internal list termed `log_units`. However if `log_units`
is empty then the messages go to the `output_unit` of the intrinsic
module `iso_fortran_env`.


## The `stdlib_logger` constants

The module defines nine distinct public integer constants for
reporting errors in the `stat` arguments of some of the module's
procedures. The constants, termed error codes, are as follows:

Error Code             | Description
-----------------------|------------
`success`              | no error was detected
`close_failure`        | a `close` statement for an I/O unit failed
`invalid_index_error`  | the `column` was invalid for the given `line`
`non_sequential_error` | the I/O unit did not have `SEQUENTIAL` access
`open_failure`         | an `open` statement failed
`read_only_error`      | an output unit did not have an `access` specifier of `'WRITE'` or `'READWRITE'`
`unformatted_in_error` | the unit did not have a `form` of `'FORMATTED'`
`unopened_in_error`    | the unit was not opened
`write_fault`          | one of the writes to `log_units` failed

## The derived type: logger_type

### Status

Experimental

### Description

Serves to define 'logger' variables to be used in reporting
significant events encountered during the execution of a program.

### Syntax

type(logger_type) :: variable

### Private attributes

| Attribute        | Type          | Description                                     | Initial value
|------------------|---------------|-------------------------------------------------|--------------
| `add_blank_line` | Logical       | Flag to precede output with a blank line        | `.false.`
| `indent_lines`   | Logical       | Flag to indent subsequent lines by four columns | `.true.`
| `log_units`      | Integer array | List of I/O units used for output               | empty
| `max_width`      | Integer       | Maximum column width of output                  | 0
| `time_stamp`     | Logical       | Flag to precede output by a time stamp          | `.true.`
| `units`          | Integer       | Count of the number of active output units      | 0

## The `stdlib_logger` variable

The module defines one public variable, `global_logger`, of type
`logger_type`. As might be guessed from its name, `global_logger` is
intended to serve as the default logger for use throughout an
application.


### Public `logger_type` methods

The module defines twelve public procedures: one function and eleven
subroutines.  The
procedures are:

Procedure            | Class      | Description
---------------------|------------|------------
`add_log_file`       | Subroutine | Opens a file using `newunit`, and adds the resulting unit to the `log_units` list
`add_log_unit`       | Subroutine | Adds an existing unit to the `log_units` list
`configure`          | Subroutine | Configures the details of the logging process
`configuration`      | Subroutine | Reports the details of the logging configuration
`log_error`          | Subroutine | Sends a message prepended by `'ERROR: '` optionally followed by a `stat` or `errmsg`
`log_information`    | Subroutine | Sends a message prepended by `'INFO: '`
`log_io_error`       | Subroutine | Sends a message prepended by `'I/O ERROR: '` optionally followed by an `iostat` or `iomsg`
`log_message`        | Subroutine | Sends a message
`log_text_error`     | Subroutine | Sends a message describing an error found in a line of text
`log_units_assigned` | Function   | Returns the number of active I/O units in `log_units`
`log_warning`        | Subroutine | Sends a message prepended by `'WARN: '`
`remove_log_unit`    | Subroutine | Removes the `unit` number from the `log_units` array

## Specification of the `logger_type` methods

### `add_log_file` - open a file and add its unit to `self % log_units`

#### Status

Experimental

#### Description

Opens a formatted, sequential access, output file, `filename` using
`newunit` and adds the resulting unit number to the logger's
`log_units` array.

#### Syntax

`call self % [[logger_type(type):add_log_file(bound)]]( filename [, unit, action, position, status, stat ] )`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar variable of type `logger_type`. It is an
`intent(inout)` argument. It shall be the logger to add the file to its `log_units`.

`filename`: shall be a scalar default character expression. It is
an `intent(in)` argument. It shall be the name of the file to be opened.

`unit` (optional): shall be a scalar default integer variable. It is an
`intent(out)` argument. It will be the unit number returned by the
`newunit` specifier of the `open` statement for `filename`.

`action` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It shall be the `action`
  specifier of the `open` statement and must have one of the values
  `'WRITE'` or `'READWRITE'`. It has the default value of `'WRITE'`.

`position` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It shall be the
  `position` specifier of the `open` statement and must have one of
  the values `'ASIS'`, `'REWIND'`, or `'APPEND'`. It has the default
  value of `'REWIND'`.

`status` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It shall be the
  `status` specifier of the `open` statement and must have one of
  the values `'OLD'`, `'NEW'`, `'REPLACE'`, or `'UNKNOWN'`. It has the
  default value of `'REPLACE'`.

`stat` (optional): shall be a scalar default integer variable. It
  is an `intent(out)` argument. If present, on return it will have the
  value `success` if `filename` could be opened, the value
  `read_only_error` if the `action` specifier is `"READ"`, or the value
  `open_failure` if `filename` could not be opened. If absent and `filename`
  could not be opened then processing will stop with an informative message as the stop code.

#### Example

```fortran
program demo_global_logger
    use stdlib_logger, global => global_logger
    ...
    integer :: unit, stat
    ...
    call global % add_log_file( 'error_log.txt', unit, &
        position='asis', stat=stat )
    if ( stat /= success ) then
         error stop 'Unable to open "error_log.txt".'
    end if
     ...
end program demo_global_logger
```

### `add_log_unit` - add a unit to the array `self % log_units`

#### Status

Experimental

#### Description

Adds `unit` to the array of `self % log_units`. The `unit` shall
be the unit number for an opened, sequential, formatted file with an
`action` specifier of `'WRITE'` or `'READWRITE'`. Failure of `unit` to meet
those requirements will cause `stat`, if present, to not be
`success` and `unit` will not be added to `log_units`. In this case, if `stat` is
not present, cause processing to stop with an informative string as
the stop code.

#### Syntax

`call self % [[logger_type(type):add_log_unit(bound)]]( unit [, stat ] )`

#### Class. 

Subroutine.

#### Arguments

`self`: shall be a scalar variable of type `logger_type`. It is an
`intent(inout)` argument. It shall be the logger to direct its output
to `unit`.

`unit`: shall be a scalar default integer expression. It is an
  `intent(in)` argument. It shall be the unit number for an opened,
  sequential, formatted file with an action specifier of `'WRITE'` or
  `'READWRITE'`.

`stat` (optional): shall be a scalar default integer variable. It is
  an `intent(out)` argument. If absent  and `unit` could not be added
  to self's `log_units` processing will stop with an informative
  message as the stop code. If present it shall have the value of one
  of the module's error codes indicating any errors found with
  `unit`. The codes are
  * `success` - no problem found
  * `non_sequential_error` - `unit` did not have an `access` specifier of
    `'SEQUENTIAL'`
  * `read_only_error` - `unit` had an `action` specifier of `'READ'`
    when it needs a specifier of `'WRITE'` or `'READWRITE'`
  * `unformatted_in_error` - `unit` did not have a `form` specifier of
    `'FORMATTED'`
  * `unopened_in_error` - `unit` was not opened

#### Example

```fortran
program demo_add_log_unit
    use stdlib_logger, only: global_logger, read_only_error
     ...
     character(256) :: iomsg
     integer :: iostat, unit, stat
     ...
     open( newunit=unit, 'error_log.txt',      &
          form='formatted', status='replace', &
          position='rewind', err=999,         &
          action='read', iostat=iostat, iomsg=iomsg )
     ...
     call global_logger % add_log_unit( unit, stat )
     select case ( stat )
     ...
     case ( read_only_error )
         error stop 'Unable to write to "error_log.txt".'
     ...
     end select
     ...
     999 error stop 'Unable to open "error_log.txt".
     ...
end program demo_add_log_unit
```

### `configuration` - report a logger's configuration

#### Status

Experimental

#### Description

Reports the configuration of a logger.

#### Syntax

`call [[stdlib_logger(module):self % configuration(interface)]]( [ add_blankline, indent, max_width, time_stamp, log_units ] )`

#### Class

Pure subroutine

#### Arguments

`self`: shall be a scalar variable of type `logger_type`. It is an
`intent(in)` argument. It shall be the logger whose configuration is reported.

`add_blank_line` (optional): shall be a scalar default logical
  variable. It is an `intent(out)` argument. A value of `.true.`
  starts output with a blank line, and `.false.` otherwise.

`indent` (optional): shall be a scalar default logical variable. It
  is an `intent(out)` argument. A value of `.true.` indents subsequent
  lines by four spaces, and `.false.` otherwise.

`max_width` (optional): shall be a scalar default integer
  variable. It is an `intent(out)` argument. A positive value bigger
  than four defines the maximum width of the output, otherwise there
  is no maximum width.

`time_stamp` (optional): shall be a scalar default logical
  variable. It is an `intent(out)` argument. A value of `.true.`
  precedes output with a time stamp of the form 'yyyy-mm-dd
  hh:mm:ss.sss', and `.false.` otherwise.

`log_units` (optional): shall be a rank one allocatable array
  variable of type default integer. It is an `intent(out)`
  argument. On return it shall be the elements of the `self`'s `log_units`
  array.

#### Example

```fortran
    module example_mod
      use stdlib_logger
      ...
      type(logger_type) :: logger
    contains
      ...
      subroutine example_sub(unit, ...)
        integer, intent(in) :: unit
        ...
        integer, allocatable :: log_units(:)
        ...
        call logger % configuration( log_units=log_units )
        if ( size(log_units) == 0 ) then
           call add_logger_unit( unit )
        end if
        ..
      end subroutine example_sub
      ...
    end module example_mod
```

### `configure` - configure the logging process

#### Status

Experimental

#### Description

Configures the logging process for self.

#### Syntax

`call [[stdlib_logger(module):self % configure(interface)]]( [ add_blank_line, indent, max_width, time_stamp ] )`

#### Class

Pure subroutine

#### Arguments

`self`: shall be a scalar variable of type `logger_type`. It is an
`intent(inout)` argument. It shall be the logger to be configured.

`add_blank_line` (optional): shall be a scalar default logical
  expression. It is an `intent(in)` argument. Set to `.true.` to start
  output with a blank line, and to `.false.` otherwise.
  
`indent` (optional): shall be a scalar default logical
  expression. It is an `intent(in)` argument. Set to `.true.` to
  indent subsequent lines by four spaces, and to `.false.` to
  not indent.

`max_width` (optional): shall be a scalar default integer
  expression. It is an `intent(in)` argument. Set to a positive value
  bigger than four to define the maximum width of the output,
  otherwise there is no maximum width.
  
`time_stamp` (optional): shall be a scalar default logical
  expression. It is an `intent(in)` argument. Set to `.true.` to
  precede output with a time stamp of the form 'yyyy-mm-dd
  hh:mm:ss.sss', and to `.false.` otherwise.
  
#### Example

```fortran
program demo_configure
      use stdlib_logger, only: global => global_logger
      
      call global % configure( indent=.false., max_width=72 )
      
end program demo_configure
```

### `log_error` - Writes the string `message` to `self % log_units`

#### Status

Experimental

#### Description

Writes the string `message` to `self % log_units` with optional additional text.

#### Syntax

`call [[stdlib_logger(module):self % log_error(interface)]]( message [, module, procedure, stat, errmsg ] )`

#### Behavior

If time stamps are active for `self`, a time stamp is written,
followed by `module` and `procedure` if present, then
`message` is written with the prefix `'ERROR: '`, and then
if `stat` or `errmsg` are present they are written.

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(in)` argument. It is the logger used to send the message.

`message`: shall be a scalar default character expression. It is an
  `intent(in)` argument.

`module` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the module containing the `log_error` call.

`procedure` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the procedure containing the `log_error` call.

`stat` (optional): shall be a scalar default integer expression. It
  is an `intent(in)` argument. It should be the `stat` specifier of
  the subroutine call or intrinsic statement that prompted the
  `log_error` call.

`errmsg` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the
  `errmsg` specifier of the subroutine call or intrinsic statement
  that prompted the `log_error` call.

#### Example

```fortran
    module  example_mod
	    use stdlib_logger
      ...
      real, allocatable :: a(:)
      ...
      type(logger_type) :: logger
      ...
    contains
      ...
      subroutine example_sub( size )
        integer, intent(in) :: size
        character(128) :: errmsg, message
        integer        :: stat
        allocate( a(size), stat=stat, errmsg=errmsg )
        if ( stat /= 0 ) then
          write( message, '(a, i0)' )                    &
              "Allocation of A failed with SIZE = ", size
          call logger % log_error( message,                   &
                                   module = 'EXAMPLE_MOD',    &
                                   procedure = 'EXAMPLE_SUB', &
                                   stat = stat,               &
                                   errmsg = errmsg )
        end if
      end subroutine example_sub
      ...
    end module example_mod
```

### `log_information` - Writes the string `message` to `self % log_units`

#### Status

Experimental

#### Description

Writes the string `message` to `self % log_units` with optional additional text.

#### Syntax

`call [[stdlib_logger(module):self % log_information(interface)]]( message [, module, procedure ] )`

#### Behavior

If time stamps are active, a time stamp is written, followed
by `module` and `procedure` if present, and then
`message` is written with the prefix `'INFO: '`.

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(in)` argument. It is the logger used to send the message.

`message`: shall be a scalar default character expression. It is an
  `intent(in)` argument.

`module` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the module containing the `log_information` call.

`procedure` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the procedure containing the `log_information` call.

#### Example

```fortran
    module  example_mod
      use stdlib_logger
      ...
      real, allocatable :: a(:)
      ...
      type(logger_type) :: logger
    contains
      ...
      subroutine example_sub( selection )
        integer, intent(out) :: selection
        character(128) :: errmsg, message
        integer        :: stat
        write(*,'(a)') "Enter an integer to select a widget"
        read(*,'(i0)') selection
        write( message, '(a, i0)' )                    &
              "The user selected ", selection
        call logger % log_information( message,               &
            module = 'EXAMPLE_MOD', procedure = 'EXAMPLE_SUB' )
        ...
      end subroutine example_sub
      ...
    end module example_mod
```

### `log_io_error` - Write the string `message` to `self % log_units`

#### Status

Experimental

#### Description

Writes the string `message` to `self % log_units` with
optional additional text.

#### Behavior

If time stamps are active, a time stamp is written
first. Then if `module` or `procedure` are present, they are
written. Then `message` is written with the prefix
`'I/O ERROR: '`. Then if `iostat` or `iomsg` are present they are
written.

#### Syntax

`call [[stdlib_logger(module):self % log_io_error(interface)]]( message [, module, procedure, iostat, iomsg ] )`

#### Class

Subroutine

#### Arguments
`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(in)` argument. It is the logger used to send the message.

`message`: shall be a scalar default character expression. It is an
  `intent(in)` argument.

`module` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the module containing the `log_io_error` call.

`procedure` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the procedure containing the `log_io_error` call.

`iostat` (optional): shall be a scalar default integer
  expression. It is an `intent(in)` argument. It should be the
  `iostat` specifier of the subroutine call or intrinsic statement
  that prompted the `log_io_error` call.

`iomsg` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the
  `iomsg` specifier of the subroutine call or intrinsic statement
  that prompted the `log_io_error` call.

#### Example

```fortran
    program example
      use stdlib_logger, global=>global_logger
      ...
      character(*), parameter :: filename = 'dummy.txt'
      integer                 :: iostat, lun
      character(128)          :: iomsg
      character(*), parameter :: message = &
	    'Failure in opening "dummy.txt".'

      open( newunit=lun, file = filename, form='formatted', &
            status='old', iostat=iostat, iomsg=iomsg )
      if ( iostat /= 0 ) then
        call global % log_io_error( message,               &
                                    procedure = 'EXAMPLE', &
                                    iostat=iostat,         &
                                    iomsg = iomsg )
        error stop 'Error on opening a file'
      end if
      ...
    end program example
```

### `log_message` - write the string `message` to `self % log_units` 

#### Status

Experimental

#### Description

Writes the string `message` to `self % log_units` with
  optional additional text.

#### Behavior

If time stamps are active, a time stamp is written,
then `module` and `procedure` are written if present,
followed by `prefix \\ ': '`, if present, and finally `message`.

#### Syntax

`call [[stdlib_logger(module):self % log_message(interface)]]( message [, module, procedure, prefix ] )`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(in)` argument. It is the logger used to send the message.

`message`: shall be a scalar default character expression. It is an
  `intent(in)` argument.

`module` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the module containing the `log_message` call.

`procedure` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the procedure containing the `log_message` call.
  
`prefix` (optional): shall be a scalar default character expression.
It is an `intent(in)` argument. It will precede `message` with an
`': '` appended.

#### Example

```fortran
    module  example_mod
      use stdlib_logger
      ...
      real, allocatable :: a(:)
      ...
      type(logger_type) :: logger
    contains
      ...
      subroutine example_sub( selection )
        integer, intent(out) :: selection
        integer        :: stat
        write(*,'(a)') "Enter an integer to select a widget"
        read(*,'(i0)') selection
        write( message, '(a, i0)' )          &
              "The user selected ", selection
        call logger % log_message( message,                   &
                                   module = 'EXAMPLE_MOD',    &
                                   procedure = 'EXAMPLE_SUB', &
                                   prefix = `INFO' )
      end subroutine example_sub
      ...
    end module example_mod
```

### `log_text_error` - send a message to `self % log_units` describing an error

#### Status

Experimental

#### Description

`log_text_error` sends a message to `self % log_units`
describing an error found in a line of text.

#### Behavior

If time stamps are active first a time stamp is
written. Then if `filename` or `line_number` are present they are
written with `column`. Then `line` is written. Then a caret, '^', is
written below `line` at the column indicated by `column`. Then
`summary` is written below the caret.

#### Syntax

`call [[stdlib_logger(module):self % log_text_error(interface)]]( line, column, summary [, filename, line_number, caret, stat ] )`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(in)` argument. It is the logger used to send the message.

`line`: shall be a scalar default character expression. It is an
  `intent(in)` argument. It should be the line of text in which the
  error was found.

`column`: shall be a scalar default integer expression. It is an
  `intent(in)` argument. It should be the one's based column at which
  the error begins.

`summary`: shall be a scalar default character expression. It is an
  `intent(in)` argument. It should be a description of the error in
  `line`.

`filename` (optional): shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the file, if any, in which `line` was found.

`line_number` (optional): shall be a scalar default integer
  expression. It is an `intent(in)` argument. It should be the line
  number in `filename` associated with `line`.

`caret` (optional): shall be a scalar default single character
  expression. It is an `intent(in)` argument. If present it will be
  placed below `line` on output to indicate the starting location of
  the error. It has a default value of '^'.

`stat` (optional): shall be a scalar default integer variable. It
  is an `intent(out)` argument. If present it will have the value of
  `success` if no errors were encountered, the value
  `index_invalid_error` if `column` is less than one or greater than
  `len(line)+1`, or the value `write_fault` if the writes to any of
  `log_units` failed. If `stat` is absent and would not have the value
  `success` then processing will stop with an informative stop code.

#### Example

```fortran
    program example
        use stdlib_logger
        ...
        character(*), parameter :: filename = 'dummy.txt'
        integer                 :: col_no, line_no, lun
        character(128)          :: line
        character(*), parameter :: message = 'Bad text found.'

        open( newunit=lun, file = filename, statu='old', &
		    form='formatted' )
        line_no = 0
        do
            read( lun, fmt='(a)', end=900 ) line
            line_no = line_no + 1
            call check_line( line, status, col_no )
            if ( status /= 0 )
              call global_logger % log_text_error( line, &
                  col_no, message, filename, line_no )
              error stop 'Error in reading ' // filename
            end if
            ...
        end do
    900 continue
        ...
    end program example
```

### `log_units_assigned` - returns the number of active I/O units

#### Status

Experimental

#### Description

Returns the number of active I/O units in `self % log_units`

#### Syntax

`Result = [[stdlib_logger(module):self % log_units_assigned(function)]]()`

#### Class

Elemental function

#### Argument

`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(in)` argument. It is the logger whose state is queried.

#### Result character

The result shall be a scalar of type default integer.

#### Result value
The result is the number of I/O units in
  `self % log_units`.

#### Example

```fortran
    module  example_mod
      use stdlib_logger
      ...
      type(logger_type) :: logger
    contains
      ...
      subroutine example_sub(unit, ...)
        integer, intent(in) :: unit
        ...
        integer, allocatable :: log_units(:)
        ...
        if ( logger % log_units_assigned() == 0 ) then
            call logger % add_log_unit( unit )
        end if
        ...
      end subroutine example_sub
      ...
    end module example_mod
```

### `log_warning` - write the string `message` to `log_units`

#### Status

Experimental

#### Description

Writes the string `message` to `log_units` with
  optional additional text.

#### Behavior

If time stamps are active, a time stamp is written,
then `module` and `procedure` if present, then
`message` is written with the prefix `WARN: '`.

#### Syntax

`call self % [[logger_type(type):log_warning(bound)]]( message [, module, procedure ] )`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(in)` argument. It is the logger used to send the message.

`message`: shall be a scalar default character expression. It is an
  `intent(in)` argument.

`module`: (optional) shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the module containing the `log_warning` call.

`procedure`: (optional) shall be a scalar default character
  expression. It is an `intent(in)` argument. It should be the name of
  the procedure containing the `log_warning` call.

#### Example

```fortran
    module  example_mod
      use stdlib_logger
      ...
      real, allocatable :: a(:)
      type(logger_type) :: logger
      ...
    contains
      ...
      subroutine example_sub( size, stat )
        integer, intent(in)  :: size
        integer, intent(out) :: stat
        allocate( a(size) )
        if ( stat /= 0 ) then
          write( message, '(a, i0)' )                    &
              "Allocation of A failed with SIZE = ", size
          call logger % log_warning( message,                 &
                                     module = 'EXAMPLE_MOD',  &
                                     procedure = 'EXAMPLE_SUB' )
        end if
      end subroutine example_sub
      ...
    end module example_mod
```

### `remove_log_unit` - remove `unit` from `self % log_units`

#### Status

Experimental

#### Description

Remove `unit` from the `self % log_units` list. If
`close_unit` is present and `.true.` then the corresponding file is
closed. If `unit` is not in `self % log_units` then nothing is done.

#### Syntax

`call self % [[logger_type(type):remove_log_unit(bound)]]( unit [, close_unit, stat ] )`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of type `logger_type`. It is an
`intent(inout)` argument. It is the logger whose `log_units` is to be
modified.

`unit`: shall be a scalar default integer expression. It is an
  `intent(in)` argument. It should be one of the I/O `unit` numbers
  in `self % log_units`. If it is not, then nothing is done.

`close_unit` (optional): shall be a scalar default logical
  expression. It is an `intent(in)` argument. If `.true` and `unit` is
  in `self % log_units` then `unit` will be closed, otherwise the I/O unit
  will be unaffected.

`stat` (optional): shall be a scalar default integer variable. It is
  an `intent(out)` argument. If present it has the default value of
  `success`, but has the value `close_failure` if `close_unit` is
  present with the value `.true.`, and `unit` is initially in
  `log_units`, and closing `unit` fails. If `stat` is absent and
  closing the `unit` fails then processing stops with an informative
  stop code.

#### Example

```fortran
    module  example_mod
      use stdlib_logger, global => global_logger
      ...
    contains
      ...
      subroutine example_sub(unit, ...)
        integer, intent(in) :: unit
        ...
        call global % remove_log_unit( unit )
        ...
      end subroutine example_sub
      ...
    end module example_mod
```
