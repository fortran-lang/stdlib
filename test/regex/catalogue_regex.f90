program catalogue_regex
    use stdlib_regex

    implicit none

    type(regex_type)   :: re
    character(len=100) :: line
    character(len=20)  :: keyword
    character(len=:), allocatable :: value
    character(len=:), allocatable :: expression
    character(len=:), allocatable :: string
    character(len=:), allocatable :: expected

    integer            :: match_start, match_end, status, ierr
    integer            :: mismatches
    logical            :: matched

    open( 10, file = 'catalogue_regex.inp', status = 'old', iostat = ierr )
    if ( ierr /= 0 ) then
        write( *, '(a)' ) 'Could not open the file "catalogue_regex.inp"'
        write( *, '(a)' ) 'It should exist - please check'
        error stop
    endif

    open( 20, file = 'catalogue_regex.report' )

    mismatches = 0

    do
        read( 10, '(a)', iostat = ierr ) line

        if ( ierr /= 0 ) then
            exit
        endif

        call extract_information( line, keyword, value )

        select case( keyword )
            case( 'expression' )
                expression = value

            case( 'input' )
                string = value

            case( 'expected' )
                write( 20, '(a)' ) ''

                expected = value

                call regcomp( re, expression, status )

                if ( status /= 0 ) then
                    mismatches = mismatches + 1
                    write( 20, '(a,i0)' ) 'Error compiling the expression: status = ', status
                    write( 20, '(a,2a)'  ) '    Expression: "', expression, '"'
                else
                    call regmatch( re, string, matched, match_start, match_end )

                    if ( matched ) then
                        write( 20, '(a,2a)'  ) 'Match found:'
                        write( 20, '(a,2a)'  ) '    Expression:   "', expression, '"'
                        write( 20, '(a,2a)'  ) '    Input string: "', string, '"'
                        write( 20, '(a,2a)'  ) '    Substring:    "', string(match_start:match_end), '"'
                        write( 20, '(a,2a)'  ) '    Expected:     "', expected, '"'
                        if ( expected == string(match_start:match_end) ) then
                            write( 20, '(a,2a)'  ) '    Success!'
                        else
                            mismatches = mismatches + 1
                            write( 20, '(a,2a)'  ) '    MISMATCH!'
                        endif
                    else
                        mismatches = mismatches + 1
                        write( 20, '(a,2a)'  ) 'NO match found:'
                        write( 20, '(a,2a)'  ) '    Expression:   "', expression, '"'
                        write( 20, '(a,2a)'  ) '    Input string: "', string, '"'
                        write( 20, '(a,2a)'  ) '    Substring:    (none)'
                        write( 20, '(a,2a)'  ) '    Expected:     "', expected, '"'
                    endif
                endif

            case( 'error-exp' )
                write( 20, '(a)' ) ''
                call regcomp( re, expression, status )

                if ( status /= 0 ) then
                    write( 20, '(a)' ) 'Error detected as expected:'
                    write( 20, '(a,2a)'  ) '    Expression:   "', expression, '"'
                else
                    mismatches = mismatches + 1
                    write( 20, '(a)' ) 'An error was expected but not detected:'
                    write( 20, '(a,2a)'  ) '    Expression:   "', expression, '"'
                endif

            case( 'no-match' )
                write( 20, '(a)' ) ''
                call regcomp( re, expression, status )

                if ( status /= 0 ) then
                    mismatches = mismatches + 1
                    write( 20, '(a,i0)' ) 'Error compiling the expression: status = ', status
                    write( 20, '(a,2a)'  ) '    Expression: "', expression, '"'
                else
                    call regmatch( re, string, matched, match_start, match_end )

                    if ( matched ) then
                        mismatches = mismatches + 1
                        write( 20, '(a,2a)'  ) 'Match found where none expected:'
                        write( 20, '(a,2a)'  ) '    Expression:   "', expression, '"'
                        write( 20, '(a,2a)'  ) '    Input string: "', string, '"'
                        write( 20, '(a,2a)'  ) '    Substring:    "', string(match_start:match_end), '"'
                        write( 20, '(a,2a)'  ) '    Expected:     (none)'
                    else
                        write( 20, '(a,2a)'  ) 'No match found, as expected:'
                        write( 20, '(a,2a)'  ) '    Expression:   "', expression, '"'
                        write( 20, '(a,2a)'  ) '    Input string: "', string, '"'
                        write( 20, '(a,2a)'  ) '    Expected:     (none)'
                    endif
                endif

            case default
                ! Treat any other keyword as comment

        end select
    enddo

    write( 20, '(/,a,i0)' ) 'Number of mismatches or other errors: ', mismatches
    write( *, '(a)' )       'Program completed'

contains

subroutine extract_information( line, keyword, value )
    character(len=*), intent(in)               :: line
    character(len=*), intent(out)              :: keyword
    character(len=:), intent(out), allocatable :: value

    character(len=20), dimension(5)            :: known_keywords = &
        [ 'expression          ', &
          'input               ', &
          'expected            ', &
          'error-exp           ', &
          'no-match            '  ]
    integer :: k1, k2

    if ( line == " " ) then
        keyword = ""
        value = ""
        return
    endif

    read( line, *, iostat = ierr ) keyword

    if ( keyword == 'error-exp' .or. keyword == 'no-match' ) then
        value = ""
        return
    endif

    if ( any( keyword == known_keywords ) ) then
        allocate( value, mold = line )

        k1 = index( line, '"' )
        if ( k1 > 0 ) then
            k2 = k1 + index( line(k1+1:), '"' )
            if ( k2 > 0 ) then
                value = line(k1+1:k2-1)
            else
                write( 20, '(a)' )  'Error interpreting the input line:'
                write( 20, '(2a)' ) '    "', trim(line), '"'
                write( 20, '(2a)' ) 'Program stopped'
                write( *, '(2a)' ) 'Program stopped - error reading input. Please check'
                error stop
            endif
        endif
    else
        value = ""
    endif
end subroutine extract_information

end program catalogue_regex
