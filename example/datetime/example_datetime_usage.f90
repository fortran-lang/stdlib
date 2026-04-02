program example_datetime
    !! Demonstrate the stdlib_datetime module functionality.
    use stdlib_datetime
    implicit none

    type(datetime_type)  :: t1, t2, t3
    type(timedelta_type) :: duration
    integer :: stat

    print '(A)', '=== stdlib_datetime Example ==='
    print *

    ! 1. Get the current local time
    t1 = now()
    print '(A,A)', 'Current local time: ', format_datetime(t1)

    ! 2. Get the current UTC time
    t2 = now_utc()
    print '(A,A)', 'Current UTC time:   ', format_datetime(t2)

    ! 3. Parse an ISO 8601 string
    t2 = parse_datetime('2026-03-17T12:00:00Z', stat)
    if (stat /= 0) then
        print '(A)', 'ERROR: Failed to parse date string!'
        stop 1
    end if
    print '(A,A)', 'Parsed datetime:    ', format_datetime(t2)

    ! 4. Calculate the difference between two datetimes
    duration = t1 - t2
    print '(A,A)', 'Duration (t1-t2):   ', &
        format_timedelta(duration)

    ! 5. Add 30 days to a date
    t3 = t2 + timedelta_type(30, 0, 0)
    print '(A,A)', 'After adding 30d:   ', format_datetime(t3)

    ! 6. Add mixed units (1 day, 6 hours, 30 minutes)
    t3 = t2 + timedelta(days=1, hours=6, minutes=30)
    print '(A,A)', 'After +1d 6h 30m:   ', format_datetime(t3)

    ! 7. Calendar utilities
    print *
    print '(A)', '=== Calendar Utilities ==='
    print '(A,L1)', 'Is 2024 leap year?  ', is_leap_year(2024)
    print '(A,L1)', 'Is 2026 leap year?  ', is_leap_year(2026)
    print '(A,I0)', 'Days in Feb 2024:   ', &
        days_in_month(2, 2024)
    print '(A,I0)', 'Days in Feb 2026:   ', &
        days_in_month(2, 2026)
    print '(A,I0)', 'Day of year (t2):   ', day_of_year(t2)
    print '(A,I0)', 'Day of week (t2):   ', day_of_week(t2)
    print '(A)', '  (1=Mon, 2=Tue, ..., 7=Sun)'

    ! 8. Comparison operators
    print *
    print '(A)', '=== Comparisons ==='
    t2 = parse_datetime('2026-03-17T12:00:00Z')
    t3 = parse_datetime('2026-03-18T12:00:00Z')
    print '(A,L1)', 'Mar17 < Mar18?      ', t2 < t3
    print '(A,L1)', 'Mar17 == Mar17?     ', t2 == t2

    ! Cross-timezone equality: 12:00 UTC == 17:30 IST
    t2 = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    t3 = datetime_type(2026, 3, 17, 17, 30, 0, 0, 330)
    print '(A,L1)', '12:00Z == 17:30+05:30? ', t2 == t3

    ! 9. Unix epoch
    print *
    print '(A,A)', 'Unix epoch:         ', &
        format_datetime(epoch())

    print *
    print '(A)', 'Done!'

end program example_datetime
