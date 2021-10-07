program test_stdlib_logger
!! A test code for most of stdlib_logger.f90.

    use, intrinsic ::           &
        iso_fortran_env, only : &
        error_unit,             &
        input_unit,             &
        output_unit

    use stdlib_logger, global => global_logger

    implicit none

    integer, allocatable :: log_units(:)
    integer              :: level, max_width, stat
    integer              :: unit1, unit2, unit3, unit4, unit5, unit6
    logical              :: add_blank_line, exist, indent, time_stamp

    if ( global % log_units_assigned() == 0 ) then
        write(*,*) 'Start off with 0 LOG_UNITS as expected.'

    else
        error stop 'Unexpected start off with non_zero LOG_UNITS.'

    end if

    call test_logging_configuration()

    call test_adding_log_files()

    print *
    print *, 'running test of log_error'
    call global % log_error( 'This message should be output to five ' //  &
        'files and not to OUTPUT_UNIT, limited to 72 columns width, ' //  &
        'preceded by no blank line, then by a time stamp, then by ' //    &
        'MODULE % PROCEDURE, be prefixed by ERROR and be indented on ' // &
        'subsequent lines by 4 columns, and finish with STAT and.' //     &
        'ERRMSG lines.',                                                  &
        module = 'N/A',                                                   &
        procedure = 'TEST_STDLIB_LOGGER',                                 &
        stat = 0,                                                         &
        errmsg = 'This is a long ERRMSG intended to test formatting ' //  &
        'of the ERRMSG when it is more than 72 columns wide.' )

    call test_removing_log_units()

    print *
    print *, 'running log_text_error'
    call global % log_text_error( 'This text should be written to UNIT1' // &
                                  ' and UNIT3 and not to OUTPUT_UNIT.',     &
                                  column = 25,                              &
                                  summary = 'There is no real error here.', &
                                  filename = 'dummy.txt',                   &
                                  line_number = 0,                          &
                                  caret = '1',                              &
                                  stat = stat )

!    call global % assert( 1 < 0, '1 < 0 ; Test of ASSERT', module='N/A', &
!        procedure = 'TEST_SDLIB_LOGGER' )

    call test_adding_log_units()

    print *
    print *, 'running log_text_error'
    call global % log_text_error( 'This text should be written to ' //      &
                                  'UNIT1, UNIT2, and OUTPUT_UNIT.',         &
                                  column = 25,                              &
                                  summary = 'There is no real error here.', &
                                  filename = 'dummy.txt',                   &
                                  line_number = 0,                          &
                                  caret = '^',                              &
                                  stat = stat )

    call test_level()

contains

    subroutine test_logging_configuration()

        print *, 'running test_logging_configuration'

        call global % configuration( add_blank_line=add_blank_line, &
            indent=indent, max_width=max_width, time_stamp=time_stamp, &
            log_units=log_units )

        if ( .not. add_blank_line ) then
            write(*,*) 'ADD_BLANK_LINE starts off as .FALSE. as expected.'

        else
            error stop 'ADD_BLANK_LINE starts off as .TRUE. contrary to ' // &
                'expectations.'

        end if

        if ( indent ) then
            write(*,*) 'INDENT starts off as .TRUE. as expected.'

        else
            error stop 'INDENT starts off as .FALSE. contrary to expectations.'

        end if

        if ( max_width == 0 ) then
            write(*,*) 'MAX_WIDTH starts off as 0 as expected.'

        else
            error stop 'MAX_WIDTH starts off as not equal to 0 contrary ' // &
                'to expectations.'

        end if

        if ( time_stamp ) then
            write(*,*) 'TIME_STAMP starts off as .TRUE. as expected.'

        else
            error stop 'TIME_STAMP starts off as .FALSE. contrary to ' // &
                'expectations.'

        end if

        if ( size(log_units) == 0 ) then
            write(*,*) 'SIZE(LOG_UNITS) starts off as 0 as expected.'

        else
            error stop 'SIZE(LOG_UNITS) starts off as non-zero contrary ' // &
                'to expectations.'

        end if

        !testing all calls independently
        call global % configuration( add_blank_line=add_blank_line )

        if ( .not. add_blank_line ) then
            write(*,*) 'ADD_BLANK_LINE starts off as .FALSE. as expected.'

        else
            error stop 'ADD_BLANK_LINE starts off as .TRUE. contrary to ' // &
                'expectations.'

        end if

        call global % configuration( indent=indent )

        if ( indent ) then
            write(*,*) 'INDENT starts off as .TRUE. as expected.'

        else
            error stop 'INDENT starts off as .FALSE. contrary to expectations.'

        end if

        call global % configuration( max_width=max_width )

        if ( max_width == 0 ) then
            write(*,*) 'MAX_WIDTH starts off as 0 as expected.'

        else
            error stop 'MAX_WIDTH starts off as not equal to 0 contrary ' // &
                'to expectations.'

        end if

        call global % configuration( time_stamp=time_stamp )

        if ( time_stamp ) then
            write(*,*) 'TIME_STAMP starts off as .TRUE. as expected.'

        else
            error stop 'TIME_STAMP starts off as .FALSE. contrary to ' // &
                'expectations.'

        end if

        call global % configuration( log_units=log_units )

        if ( size(log_units) == 0 ) then
            write(*,*) 'SIZE(LOG_UNITS) starts off as 0 as expected.'

        else
            error stop 'SIZE(LOG_UNITS) starts off as non-zero contrary ' // &
                'to expectations.'

        end if


        call global % log_information( 'This message should be output ' // &
            'to OUTPUT_UNIT, unlimited in width, not preceded by ' //      &
            'a blank line, then by a time stamp, then by MODULE % ' //     &
            'PROCEDURE, be prefixed by INFO and be indented on ' //        &
            'subsequent lines by 4 columns.',                              &
            module = 'N/A',                                                &
            procedure = 'TEST_STDLIB_LOGGER' )

        call global % log_information( 'This message should be output ' // &
            'to OUTPUT_UNIT, unlimited in width, not preceded by ' //      &
            'a blank line, then by a time stamp, then by MODULE % ' //     &
            'PROCEDURE, be prefixed by INFO. ' // new_line('a') //         &
            'This is a new line of the same log message.',                 &
            module = 'N/A',                                                &
            procedure = 'TEST_STDLIB_LOGGER' )

        call global % log_debug( 'This message should be output ' //       &
            'to OUTPUT_UNIT, unlimited in width, not preceded by ' //      &
            'a blank line, then by a time stamp, then by MODULE % ' //     &
            'PROCEDURE, be prefixed by DEBUG and be indented on ' //       &
            'subsequent lines by 4 columns.',                              &
            module = 'N/A',                                                &
            procedure = 'TEST_STDLIB_LOGGER' )

        call global % log_debug( 'This message should be output ' //       &
            'to OUTPUT_UNIT, unlimited in width, not preceded by ' //      &
            'a blank line, then by a time stamp, then by MODULE % ' //     &
            'PROCEDURE, be prefixed by DEBUG. ' // new_line('a') //        &
            'This is a new line of the same log message.',                 &
            module = 'N/A',                                                &
            procedure = 'TEST_STDLIB_LOGGER' )

        call global % configure( add_blank_line=.true., indent=.false., &
            max_width=72, time_stamp=.false. )

        call global % configuration( add_blank_line=add_blank_line,    &
            indent=indent, max_width=max_width, time_stamp=time_stamp, &
            log_units=log_units )

        if ( add_blank_line ) then
            write(*,*) 'ADD_BLANK_LINE is now .TRUE. as expected.'

        else
            error stop 'ADD_BLANKLINE is now .FALSE. contrary to expectations.'

        end if

        if ( .not. indent ) then
            write(*,*) 'INDENT is now .FALSE. as expected.'

        else
            error stop 'INDENT is now .TRUE. contrary to expectations.'

        end if

        if ( max_width == 72 ) then
            write(*,*) 'MAX_WIDTH is now 72 as expected.'

        else
            error stop 'MAX_WIDTH is not equal to 72 contrary to expectations.'

        end if

        if ( .not. time_stamp ) then
            write(*,*) 'TIME_STAMP is now .FALSE. as expected.'

        else
            error stop 'TIME_STAMP starts off as .FALSE. contrary to ' // &
                'expectations.'

        end if

        if ( size(log_units) == 0 ) then
            write(*,*) 'SIZE(LOG_UNITS) is still 0 as expected.'

        else
            error stop 'SIZE(LOG_UNITS) is now non-zero contrary to ' // &
                'expectations.'

        end if

        call global % log_message( 'This message should still be output ' // &
            'to OUTPUT_UNIT, limited to 72 columns width, preceded by ' //   &
            'a blank line, then by no time stamp, then by MODULE % ' //      &
            'PROCEDURE, have no prefix, and be unindented on subsequent ' // &
            'lines.', &
            module = 'N/A', &
            procedure = 'TEST_STDLIB_LOGGER' )

        call global % log_message( 'The last word of the first line ' //      &
            new_line('a')//'should be "line". "Line"' // new_line('a') //     &
            'is also the last word for the second line. The following ' //    &
            'lines should be limited to 72 columns width.' , &
            module = 'N/A', &
            procedure = 'TEST_STDLIB_LOGGER' )

        call global % configure( add_blank_line=.false., indent=.true., &
            max_width=72, time_stamp=.true. )

        call global % log_warning( 'This message should still be ' //   &
            'output to OUTPUT_UNIT, limited to 72 columns width, ' //   &
            'preceded by no blank line, then by a time stamp, then ' // &
            'by MODULE % PROCEDURE, have a prefix of WARN, and be ' //  &
            'indented by 4 columns on subsequent lines.',               &
            module = 'N/A',                                             &
            procedure = 'TEST_STDLIB_LOGGER' )

        call global % log_message( 'The last word of the first line ' //      &
            new_line('a')//'should be "the". "Line"' // new_line('a') //      &
            'should be the last word for the second line. The following ' //  &
            'lines should be limited to 72 columns width. From the second ' //&
            'line, all lines should be indented by 4 columns.' ,&
            module = 'N/A', &
            procedure = 'TEST_STDLIB_LOGGER' )

    end subroutine test_logging_configuration


    subroutine test_adding_log_files()

        print *
        print *, 'running test_adding_log_files'

        call global % add_log_file( 'first_log_file.txt', unit1, stat=stat )
        if ( stat == success ) then
            write(*,*) 'Able to open "first_log_file.txt" as expected'

        else
            error stop 'Unable to open "first_log_file.txt" contrary to ' // &
                'expectations.'

        end if

        if ( global % log_units_assigned() == 1 ) then
            write(*,*) 'Incremented to 1 LOG_UNITS as expected.'

        else
            error stop 'Unexpected increment to other than 1 LOG_UNITS.'

        end if

        call global % add_log_file( 'second_log_file.txt', unit2, &
            action='readwrite', stat=stat )
        if ( stat == success ) then
            write(*,*) 'Able to open "second_log_file.txt" as expected'

        else
            error stop 'Unable to open "second_log_file.txt" contrary to ' // &
                'expectations.'

        end if

        if ( global % log_units_assigned() == 2 ) then
            write(*,*) 'Incremented to 2 LOG_UNITS as expected.'

        else
            error stop 'Unexpected increment to other than 2 LOG_UNITS.'

        end if

        call global %add_log_file( 'third_log_file.txt', unit3, &
            position='asis', stat=stat )
        if ( stat == success ) then
            write(*,*) 'Able to open "third_log_file.txt" as expected'

        else
            error stop 'Unable to open "third_log_file.txt" as contrary ' // &
                'to expectations.'

        end if

        if ( global % log_units_assigned() == 3 ) then
            write(*,*) 'Incremented to 3 LOG_UNITS as expected.'

        else
            error stop 'Unexpected increment to other than 3 LOG_UNITS.'

        end if

        call global % add_log_file( 'fourth_log_file.txt', unit4, &
            status='new', stat=stat )
        if ( stat /= success ) then
            inquire( file='fourth_log_file.txt', exist=exist )
            write(*,*) 'Unable to OPEN "fourth_log_file.txt" as "NEW" ' // &
                'as it already exists, which is an expected result.'
            call global % add_log_file( 'fourth_log_file.txt', unit4, &
                status='old', position='rewind', stat=stat )

            if ( stat /= success ) then
                error stop 'Unable to open "fourth_log_file.txt" as "OLD".'

            end if

        end if

        if ( global % log_units_assigned() == 4 ) then
            write(*,*) 'Incremented to 4 LOG_UNITS as expected.'

        else
            error stop 'Unexpected increment to other than 4 LOG_UNITS.'

        end if

        call global % add_log_file( 'fifth_log_file.txt', unit5, &
            action='READ', stat=stat )
        if ( stat /= success ) then
            if ( stat == read_only_error ) then
                write(*,*) 'Unable to OPEN "fifth_log_file.txt" as ' // &
                    '"READ", as it makes it read only, which is an ' // &
                    'expected result.'
                call global % add_log_file( 'fifth_log_file.txt', unit5, &
                    action='write', stat=stat )
                if ( stat /= success ) then
                    error stop 'Unable to open "fifth_log_file.txt" as "WRITE".'

                end if

            end if

        end if

        if ( global % log_units_assigned() == 5 ) then
            write(*,*) 'Incremented to 5 LOG_UNITS as expected.'

        else
            error stop 'Unexpected increment to other than 5 LOG_UNITS.'

        end if

    end subroutine test_adding_log_files

    subroutine test_removing_log_units()

        logical :: opened
        integer :: istat

        print *
        print *, 'running test_removing_log_units'
        call global % remove_log_unit( unit5 )
        if ( global % log_units_assigned() == 4 ) then
            write(*,*) 'Decremented to 4 LOG_UNITS as expected.'

        else
            error stop 'Unexpected change to other than 4 LOG_UNITS.'

        end if

        call global % remove_log_unit( unit5 )
! Should do nothing as already removed
        if ( global % log_units_assigned() == 4 ) then
            write(*,*) 'Remained at 4 LOG_UNITS as expected.'

        else
            error stop 'Unexpected change to other than 4 LOG_UNITS.'

        end if

        inquire( unit4, opened=opened )
        if ( opened ) then
            write(*,*) 'UNIT4 is OPENED as expected.'

        else
            error stop 'UNIT4 is not OPENED contrary to expectations.'

        end if

        call global % remove_log_unit( unit4, close_unit=.true., stat=stat )
        if ( stat /= success ) then
            error stop 'Unable to close UNIT4 in REMOVE_LOG_UNIT.'

        end if

        if ( global % log_units_assigned() == 3 ) then
            write(*,*) 'Decremented to 3 LOG_UNITS as expected.'

        else
            error stop 'Unexpected change to other than 3 LOG_UNITS.'

        end if

        inquire( unit4, opened=opened, iostat=istat )
        if(istat /= 0) opened = .false.
        if ( opened ) then
            error stop 'UNIT4 is opened contrary to expectations.'

        else
            write(*,*) 'UNIT4 is not opened as expected.'

        end if

        call global % configuration( log_units=log_units )
        if ( unit1 == log_units(1) .and. unit2 == log_units(2) .and. &
             unit3 == log_units(3) ) then
            write(*,*) 'Units have retained their expected ordering'

        else
            error stop 'Units have not retained their expected ordering'

        end if

        call global % remove_log_unit( unit4, close_unit=.true., stat=stat )
        if ( stat /= success ) then
            error stop 'Attempted to close UNIT4 in REMOVE_LOG_UNIT and failed.'

        end if

        if ( global % log_units_assigned() == 3 ) then
            write(*,*) 'Remained at 3 LOG_UNITS as expected.'

        else
            error stop 'Unexpected change to other than 3 LOG_UNITS.'

        end if

        call global % configuration( log_units=log_units )
        if ( unit1 == log_units(1) .and. unit2 == log_units(2) .and. &
             unit3 == log_units(3) ) then
            write(*,*) 'Units have retained their expected ordering'

        else
            error stop 'Units have not retained their expected ordering'

        end if

        call global % remove_log_unit( unit2 )

        if ( global % log_units_assigned() == 2 ) then
            write(*,*) 'Decremented to 2 LOG_UNITS as expected.'

        else
            error stop 'Unexpected change to other than 2 LOG_UNITS.'

        end if
        call global % configuration( log_units=log_units )
        if ( unit1 == log_units(1) .and. unit3 == log_units(2) ) then
            write(*,*) 'Units have their expected placement'

        else
            error stop 'Units do not have their expected placement'

        end if

    end subroutine test_removing_log_units

    subroutine test_adding_log_units()

        print *
        print *, 'running test_adding_log_units'
        call global % add_log_unit( unit2, stat )
        if ( stat == success ) then
            if ( global % log_units_assigned() == 3 ) then
                write(*,*) 'Successfully added unit2 as expected'

            else
                error stop 'Adding unit2 failed to increase log_units to 3.'

            end if

        else
            error stop 'Unexpected problem adding unit2.'

        end if

        call global % add_log_unit( output_unit, stat )
        if ( stat == success ) then
            if ( global % log_units_assigned() == 4 ) then
                write(*,*) 'Successfully added output_unit as expected'

            else
                error stop 'Adding output_unit failed to increase ' // &
                    'log_units to 4.'

            end if

        else
            error stop 'Unexpected problem adding output_unit.'

        end if

        call global % add_log_unit( error_unit, stat )
        if ( stat == success ) then
            if ( global % log_units_assigned() == 5 ) then
                write(*,*) 'Successfully added error_unit as expected'

            else
                error stop 'Adding error_unit failed to increase ' // &
                    'log_units to 5.'

            end if

        else
            error stop 'Unexpected problem adding error_unit.'

        end if

        call global % add_log_unit( input_unit, stat )
        if ( stat /= success ) then
            if ( global % log_units_assigned() == 5 ) then
                write(*,*) 'Failed at adding input_unit as expected'

            else
                error stop 'Unsuccessfully adding input_unit failed to ' // &
                    'keep log_units to 5.'

            end if

        else
            error stop 'Unexpected success adding input_unit.'

        end if

        open( newunit=unit6, file='sixth_log_file.txt', form='formatted', &
            action='read', status='replace', position='rewind' )
        call global % add_log_unit( unit6, stat )
        if ( stat == read_only_error ) then
            write(*,*) 'Adding unit6 failed with a READ_ONLY_ERROR as expected'

        else
            error stop 'Adding unit6 did not fail with a READ_ONLY_ERROR.'

        end if
        close(unit6)
        call global % add_log_unit( unit6, stat )
        if ( stat == unopened_in_error ) then
            write(*,*) 'Adding unit6 failed with a UNOPENED_IN_ERROR as ' // &
                'expected'

        else
            error stop 'Adding unit6 did not fail with a UNOPENED_IN_ERROR.'

        end if
        open( newunit=unit6, file='sixth_log_file.txt', form='unformatted', &
            action='write', status='replace', position='rewind' )
        call global % add_log_unit( unit6, stat )
        if ( stat == unformatted_in_error ) then
            write(*,*) 'Adding unit6 failed with a UNFORMATTED_IN_ERROR ' // &
                'as expected'

        else
            write(*, *) 'STAT = ', stat
            error stop 'Adding unit6 did not fail with a UNFORMATTED_IN_ERROR.'

        end if
        close(unit6)
        open( newunit=unit6, file='sixth_log_file.txt', form='formatted', &
            action='write', status='replace', access='direct', recl=100 )
        call global % add_log_unit( unit6, stat )
        if ( stat == non_sequential_error ) then
            write(*,*) 'Adding unit6 failed with a ' // &
                'NON_SEQUENTIAL_ERROR as expected'

        else
            error stop 'Adding unit6 did not fail with a ' // &
                'NON_SEQUENTIAL_ERROR.'

        end if
        close(unit6)
        open( newunit=unit6, file='sixth_log_file.txt', form='formatted', &
            action='write', status='replace', position='rewind', &
            access='sequential' )
        call global % add_log_unit( unit6, stat )
        if ( stat == success ) then
            if ( global % log_units_assigned() == 6 ) then
                write(*,*) 'Successfully added unit6 as expected'

            else
                error stop 'Adding unit6 failed to increase log_units to 6.'

            end if

        else
            error stop 'Unexpected problem adding unit6.'

        end if

        call global % remove_log_unit( unit6, stat=stat )
        if ( stat /= success ) then
            error stop 'Unexpected problem removing unit6'

        else
            if ( global % log_units_assigned() /= 5 ) then
                error stop 'Removing unit6 did not decrement log_units to 5.'

            else
                write(*,*) 'Successfully removed unit6 as expected.'

            end if

        end if

        call global % remove_log_unit( error_unit, stat=stat )
        if ( stat /= success ) then
            error stop 'Unexpected problem removing error_unit'

        else
            if ( global % log_units_assigned() /= 4 ) then
                error stop 'Removing error_unit did not decrement ' // &
                    'log_units to 4.'

            else
                write(*,*) 'Successfully removed error_unit as expected.'

            end if

        end if

        call global % remove_log_unit( unit3, stat=stat )
        if ( stat /= success ) then
            error stop 'Unexpected problem removing unit3'

        else
            if ( global % log_units_assigned() /= 3 ) then
                error stop 'Removing unit3 did not decrement ' // &
                    'log_units to 3.'

            else
                write(*,*) 'Successfully removed unit3 as expected.'

            end if

        end if

        return
    end subroutine test_adding_log_units

    subroutine test_level()

        print *, 'running test_level'

        call global % configure( level = all_level )

        call global % configuration( level = level )
        if ( level == all_level ) then
            write(*,*) 'LEVEL is all_level as expected.'

        else
            error stop 'LEVEL starts off as not equal to all_level ' //&
            'contrary to expectations.'

        end if

        call global % log_message('This message should be always printed, &
             & irrespective of the severity level')

        call global % log_debug( 'This message should be printed')
        call global % log_information( 'This message should be printed')
        call global % log_warning( 'This message should be printed')
        call global % log_error( 'This message should be printed')
        call global % log_io_error( 'This message should be printed')

        call global % configure( level = debug_level )

        call global % configuration( level = level )
        if ( level == debug_level ) then
            write(*,*) 'LEVEL is debug_level as expected.'

        else
            error stop 'LEVEL starts off as not equal to debug_level ' //&
            'contrary to expectations.'

        end if

        call global % log_message('This message should be always printed, &
             & irrespective of the severity level')

        call global % log_debug( 'This message should be printed')
        call global % log_information( 'This message should be printed')
        call global % log_warning( 'This message should be printed')
        call global % log_error( 'This message should be printed')
        call global % log_io_error( 'This message should be printed')

        call global % configure( level = information_level )

        call global % configuration( level = level )
        if ( level == information_level ) then
            write(*,*) 'LEVEL is information_level as expected.'

        else
            error stop 'LEVEL starts off as not equal to information_level ' //&
            'contrary to expectations.'

        end if

        call global % log_message('This message should be always printed, &
             & irrespective of the severity level')

        call global % log_debug( 'This message should NOT be printed')
        call global % log_information( 'This message should be printed')
        call global % log_warning( 'This message should be printed')
        call global % log_error( 'This message should be printed')
        call global % log_io_error( 'This message should be printed')

        call global % configure( level = warning_level )

        call global % configuration( level = level )
        if ( level == warning_level ) then
            write(*,*) 'LEVEL is warning_level as expected.'

        else
            error stop 'LEVEL starts off as not equal to warning_level ' //&
            'contrary to expectations.'

        end if

        call global % log_message('This message should be always printed, &
             & irrespective of the severity level')

        call global % log_debug( 'This message should NOT be printed')
        call global % log_information( 'This message should NOT be printed')
        call global % log_warning( 'This message should be printed')
        call global % log_error( 'This message should be printed')
        call global % log_io_error( 'This message should be printed')

        call global % configure( level = error_level )

        call global % configuration( level = level )
        if ( level == error_level ) then
            write(*,*) 'LEVEL is error_level as expected.'

        else
            error stop 'LEVEL starts off as not equal to error_level ' //&
            'contrary to expectations.'

        end if

        call global % log_message('This message should be always printed, &
             & irrespective of the severity level')

        call global % log_debug( 'This message should NOT be printed')
        call global % log_information( 'This message should NOT be printed')
        call global % log_warning( 'This message should NOT be printed')
        call global % log_error( 'This message should be printed')
        call global % log_io_error( 'This message should be printed')

        call global % configure( level = none_level )

        call global % configuration( level = level )
        if ( level == none_level ) then
            write(*,*) 'LEVEL is none_level as expected.'

        else
            error stop 'LEVEL starts off as not equal to none_level ' //&
            'contrary to expectations.'

        end if

        call global % log_message('This message should be always printed, &
             & irrespective of the severity level')

        call global % log_debug( 'This message should NOT be printed')
        call global % log_information( 'This message should NOT be printed')
        call global % log_warning( 'This message should NOT be printed')
        call global % log_error( 'This message should NOT be printed')
        call global % log_io_error( 'This message should NOT be printed')

        print *, 'end of test_level'

    end subroutine test_level

end program test_stdlib_logger
