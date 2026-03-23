module stdlib_datetime
    !! version: experimental
    !!
    !! Date, time, and time interval handling for Fortran.
    !! ([Specification](../page/specs/stdlib_datetime.html))
    use stdlib_kinds, only: dp, int64
    implicit none
    private

    public :: datetime_type, timedelta_type
    public :: datetime, timedelta, now, now_utc, epoch
    public :: parse_datetime, format_datetime, format_timedelta
    public :: is_leap_year, days_in_month, days_in_year
    public :: day_of_year, day_of_week, to_utc, total_seconds
    public :: operator(+), operator(-)
    public :: operator(==), operator(/=)
    public :: operator(<), operator(<=)
    public :: operator(>), operator(>=)

    type :: datetime_type
        !! version: experimental
        !!
        !! Represents a specific point in time.
        integer :: year        = 1   !! Year (1-9999)
        integer :: month       = 1   !! Month (1-12)
        integer :: day         = 1   !! Day (1-31)
        integer :: hour        = 0   !! Hour (0-23)
        integer :: minute      = 0   !! Minute (0-59)
        integer :: second      = 0   !! Second (0-59)
        integer :: millisecond = 0   !! Millisecond (0-999)
        integer :: utc_offset_minutes = 0 !! UTC offset in minutes
    end type datetime_type

    type :: timedelta_type
        !! version: experimental
        !!
        !! Represents a duration or time interval.
        !! Normalized: seconds in [0,86399], ms in [0,999].
        !! Days can be negative for negative durations.
        integer :: days         = 0   !! Number of days
        integer :: seconds      = 0   !! Seconds (0-86399)
        integer :: milliseconds = 0   !! Milliseconds (0-999)
    end type timedelta_type

    integer(int64), parameter :: MS_PER_SEC  = 1000_int64
    integer(int64), parameter :: MS_PER_MIN  = 60000_int64
    integer(int64), parameter :: MS_PER_HOUR = 3600000_int64
    integer(int64), parameter :: MS_PER_DAY  = 86400000_int64

    interface operator(+)
        module procedure dt_plus_td
        module procedure td_plus_dt
        module procedure td_plus_td
    end interface

    interface operator(-)
        module procedure dt_minus_td
        module procedure dt_minus_dt
        module procedure td_minus_td
        module procedure td_negate
    end interface

    interface operator(==)
        module procedure dt_eq
        module procedure td_eq
    end interface

    interface operator(/=)
        module procedure dt_ne
        module procedure td_ne
    end interface

    interface operator(<)
        module procedure dt_lt
        module procedure td_lt
    end interface

    interface operator(<=)
        module procedure dt_le
        module procedure td_le
    end interface

    interface operator(>)
        module procedure dt_gt
        module procedure td_gt
    end interface

    interface operator(>=)
        module procedure dt_ge
        module procedure td_ge
    end interface

    interface is_leap_year
        module procedure is_leap_year_int
        module procedure is_leap_year_dt
    end interface

contains

    pure function datetime(year, month, day, hour, minute, &
                           second, millisecond, &
                           utc_offset_minutes) result(dt)
        !! version: experimental
        !!
        !! Create a datetime_type from individual components.
        integer, intent(in), optional :: year, month, day
        integer, intent(in), optional :: hour, minute, second
        integer, intent(in), optional :: millisecond
        integer, intent(in), optional :: utc_offset_minutes
        type(datetime_type) :: dt
        if (present(year))        dt%year        = year
        if (present(month))       dt%month       = month
        if (present(day))         dt%day         = day
        if (present(hour))        dt%hour        = hour
        if (present(minute))      dt%minute      = minute
        if (present(second))      dt%second      = second
        if (present(millisecond)) dt%millisecond = millisecond
        if (present(utc_offset_minutes)) &
            dt%utc_offset_minutes = utc_offset_minutes
    end function datetime

    pure function timedelta(days, hours, minutes, seconds, &
                            milliseconds) result(td)
        !! version: experimental
        !!
        !! Create a normalized timedelta_type from mixed units.
        integer, intent(in), optional :: days, hours, minutes
        integer, intent(in), optional :: seconds, milliseconds
        type(timedelta_type) :: td
        integer(int64) :: total_ms
        total_ms = 0_int64
        if (present(days)) &
            total_ms = total_ms &
                     + int(days, int64) * MS_PER_DAY
        if (present(hours)) &
            total_ms = total_ms &
                     + int(hours, int64) * MS_PER_HOUR
        if (present(minutes)) &
            total_ms = total_ms &
                     + int(minutes, int64) * MS_PER_MIN
        if (present(seconds)) &
            total_ms = total_ms &
                     + int(seconds, int64) * MS_PER_SEC
        if (present(milliseconds)) &
            total_ms = total_ms + int(milliseconds, int64)
        td = ms_to_td(total_ms)
    end function timedelta

    function now() result(dt)
        !! version: experimental
        !!
        !! Return the current local date and time.
        type(datetime_type) :: dt
        integer :: v(8)
        call date_and_time(values=v)
        dt = datetime_type(v(1), v(2), v(3), v(5), &
                           v(6), v(7), v(8), v(4))
    end function now

    function now_utc() result(dt)
        !! version: experimental
        !!
        !! Return the current UTC date and time.
        type(datetime_type) :: dt
        dt = to_utc(now())
    end function now_utc

    pure function epoch() result(dt)
        !! version: experimental
        !!
        !! Return the Unix epoch: 1970-01-01T00:00:00Z.
        type(datetime_type) :: dt
        dt = datetime_type(1970, 1, 1, 0, 0, 0, 0, 0)
    end function epoch

    pure function dt_plus_td(dt, td) result(res)
        !! datetime + timedelta
        type(datetime_type), intent(in)  :: dt
        type(timedelta_type), intent(in) :: td
        type(datetime_type) :: res
        res = epoch_ms_to_dt( &
            dt_to_epoch_ms(dt) + td_to_ms(td), &
            dt%utc_offset_minutes)
    end function dt_plus_td

    pure function td_plus_dt(td, dt) result(res)
        !! timedelta + datetime (commutative)
        type(timedelta_type), intent(in) :: td
        type(datetime_type), intent(in)  :: dt
        type(datetime_type) :: res
        res = dt_plus_td(dt, td)
    end function td_plus_dt

    pure function td_plus_td(td1, td2) result(res)
        !! timedelta + timedelta
        type(timedelta_type), intent(in) :: td1, td2
        type(timedelta_type) :: res
        res = ms_to_td(td_to_ms(td1) + td_to_ms(td2))
    end function td_plus_td

    pure function dt_minus_td(dt, td) result(res)
        !! datetime - timedelta
        type(datetime_type), intent(in)  :: dt
        type(timedelta_type), intent(in) :: td
        type(datetime_type) :: res
        res = epoch_ms_to_dt( &
            dt_to_epoch_ms(dt) - td_to_ms(td), &
            dt%utc_offset_minutes)
    end function dt_minus_td

    pure function dt_minus_dt(dt1, dt2) result(res)
        !! datetime - datetime (both converted to UTC)
        type(datetime_type), intent(in) :: dt1, dt2
        type(timedelta_type) :: res
        res = ms_to_td(dt_to_utc_ms(dt1) - dt_to_utc_ms(dt2))
    end function dt_minus_dt

    pure function td_minus_td(td1, td2) result(res)
        !! timedelta - timedelta
        type(timedelta_type), intent(in) :: td1, td2
        type(timedelta_type) :: res
        res = ms_to_td(td_to_ms(td1) - td_to_ms(td2))
    end function td_minus_td

    pure function td_negate(td) result(res)
        !! Unary minus: -timedelta
        type(timedelta_type), intent(in) :: td
        type(timedelta_type) :: res
        res = ms_to_td(-td_to_ms(td))
    end function td_negate

    pure function dt_eq(dt1, dt2) result(res)
        type(datetime_type), intent(in) :: dt1, dt2
        logical :: res
        res = dt_to_utc_ms(dt1) == dt_to_utc_ms(dt2)
    end function dt_eq

    pure function dt_ne(dt1, dt2) result(res)
        type(datetime_type), intent(in) :: dt1, dt2
        logical :: res
        res = dt_to_utc_ms(dt1) /= dt_to_utc_ms(dt2)
    end function dt_ne

    pure function dt_lt(dt1, dt2) result(res)
        type(datetime_type), intent(in) :: dt1, dt2
        logical :: res
        res = dt_to_utc_ms(dt1) < dt_to_utc_ms(dt2)
    end function dt_lt

    pure function dt_le(dt1, dt2) result(res)
        type(datetime_type), intent(in) :: dt1, dt2
        logical :: res
        res = dt_to_utc_ms(dt1) <= dt_to_utc_ms(dt2)
    end function dt_le

    pure function dt_gt(dt1, dt2) result(res)
        type(datetime_type), intent(in) :: dt1, dt2
        logical :: res
        res = dt_to_utc_ms(dt1) > dt_to_utc_ms(dt2)
    end function dt_gt

    pure function dt_ge(dt1, dt2) result(res)
        type(datetime_type), intent(in) :: dt1, dt2
        logical :: res
        res = dt_to_utc_ms(dt1) >= dt_to_utc_ms(dt2)
    end function dt_ge

    pure function td_eq(td1, td2) result(res)
        type(timedelta_type), intent(in) :: td1, td2
        logical :: res
        res = td_to_ms(td1) == td_to_ms(td2)
    end function td_eq

    pure function td_ne(td1, td2) result(res)
        type(timedelta_type), intent(in) :: td1, td2
        logical :: res
        res = td_to_ms(td1) /= td_to_ms(td2)
    end function td_ne

    pure function td_lt(td1, td2) result(res)
        type(timedelta_type), intent(in) :: td1, td2
        logical :: res
        res = td_to_ms(td1) < td_to_ms(td2)
    end function td_lt

    pure function td_le(td1, td2) result(res)
        type(timedelta_type), intent(in) :: td1, td2
        logical :: res
        res = td_to_ms(td1) <= td_to_ms(td2)
    end function td_le

    pure function td_gt(td1, td2) result(res)
        type(timedelta_type), intent(in) :: td1, td2
        logical :: res
        res = td_to_ms(td1) > td_to_ms(td2)
    end function td_gt

    pure function td_ge(td1, td2) result(res)
        type(timedelta_type), intent(in) :: td1, td2
        logical :: res
        res = td_to_ms(td1) >= td_to_ms(td2)
    end function td_ge

    pure function format_datetime(dt) result(str)
        !! version: experimental
        !!
        !! Format a datetime_type as an ISO 8601 string.
        type(datetime_type), intent(in) :: dt
        character(:), allocatable :: str
        character(len=29) :: buf
        integer :: n, off_h, off_m

        write(buf(1:4),  '(I4.4)') dt%year
        buf(5:5) = '-'
        write(buf(6:7),  '(I2.2)') dt%month
        buf(8:8) = '-'
        write(buf(9:10), '(I2.2)') dt%day
        buf(11:11) = 'T'
        write(buf(12:13), '(I2.2)') dt%hour
        buf(14:14) = ':'
        write(buf(15:16), '(I2.2)') dt%minute
        buf(17:17) = ':'
        write(buf(18:19), '(I2.2)') dt%second
        n = 19

        if (dt%millisecond /= 0) then
            buf(n+1:n+1) = '.'
            write(buf(n+2:n+4), '(I3.3)') dt%millisecond
            n = n + 4
        end if

        if (dt%utc_offset_minutes == 0) then
            buf(n+1:n+1) = 'Z'
            n = n + 1
        else
            off_h = abs(dt%utc_offset_minutes) / 60
            off_m = mod(abs(dt%utc_offset_minutes), 60)
            if (dt%utc_offset_minutes > 0) then
                buf(n+1:n+1) = '+'
            else
                buf(n+1:n+1) = '-'
            end if
            write(buf(n+2:n+3), '(I2.2)') off_h
            buf(n+4:n+4) = ':'
            write(buf(n+5:n+6), '(I2.2)') off_m
            n = n + 6
        end if

        str = buf(1:n)
    end function format_datetime

    pure function format_timedelta(td) result(str)
        !! version: experimental
        !!
        !! Format a timedelta_type as a readable string.
        type(timedelta_type), intent(in) :: td
        character(:), allocatable :: str
        character(len=40) :: buf
        integer :: h, m, s, n

        h = td%seconds / 3600
        m = mod(td%seconds, 3600) / 60
        s = mod(td%seconds, 60)

        write(buf, &
            '(I0," days, ",I2.2,":",I2.2,":",I2.2)') &
            td%days, h, m, s
        n = len_trim(buf)

        if (td%milliseconds /= 0) then
            write(buf(n+1:n+4), '(".",I3.3)') &
                td%milliseconds
            n = n + 4
        end if

        str = buf(1:n)
    end function format_timedelta

    pure function parse_datetime(str, stat) result(dt)
        !! version: experimental
        !!
        !! Parse an ISO 8601 date/time string.
        character(len=*), intent(in) :: str
        integer, intent(out), optional :: stat
        type(datetime_type) :: dt
        integer :: slen, ios, off_h, off_m, ms_end
        integer :: max_day
        character(len=1) :: sign_ch

        if (present(stat)) stat = 0
        dt = datetime_type()
        slen = len_trim(str)

        ! Require at least YYYY-MM-DD (10 characters)
        if (slen < 10) then
            if (present(stat)) stat = 1
            return
        end if

        ! Check required date separators for ISO 8601 (YYYY-MM-DD)
        if (str(5:5) /= '-' .or. str(8:8) /= '-') then
            if (present(stat)) stat = 1
            return
        end if

        read(str(1:4), '(I4)', iostat=ios) dt%year
        if (ios /= 0) then
            if (present(stat)) stat = 1
            return
        end if
        read(str(6:7), '(I2)', iostat=ios) dt%month
        if (ios /= 0) then
            if (present(stat)) stat = 1
            return
        end if
        ! Validate month range [1,12]
        if (dt%month < 1 .or. dt%month > 12) then
            if (present(stat)) stat = 1
            return
        end if
        read(str(9:10), '(I2)', iostat=ios) dt%day
        if (ios /= 0) then
            if (present(stat)) stat = 1
            return
        end if
        ! Validate day range [1, days_in_month]
        max_day = days_in_month(dt%month, dt%year)
        if (dt%day < 1 .or. dt%day > max_day) then
            if (present(stat)) stat = 1
            return
        end if

        if (slen == 10) return

        if (str(11:11) /= 'T' .and. &
            str(11:11) /= 't' .and. &
            str(11:11) /= ' ') then
            if (present(stat)) stat = 1
            return
        end if

        if (slen < 19) then
            if (present(stat)) stat = 1
            return
        end if

        ! Validate required time separators (HH:MM:SS)
        if (str(14:14) /= ':' .or. str(17:17) /= ':') then
            if (present(stat)) stat = 1
            return
        end if

        read(str(12:13), '(I2)', iostat=ios) dt%hour
        if (ios /= 0) then
            if (present(stat)) stat = 1
            return
        end if
        ! Validate hour range [0,23]
        if (dt%hour < 0 .or. dt%hour > 23) then
            if (present(stat)) stat = 1
            return
        end if
        read(str(15:16), '(I2)', iostat=ios) dt%minute
        if (ios /= 0) then
            if (present(stat)) stat = 1
            return
        end if
        ! Validate minute range [0,59]
        if (dt%minute < 0 .or. dt%minute > 59) then
            if (present(stat)) stat = 1
            return
        end if
        read(str(18:19), '(I2)', iostat=ios) dt%second
        if (ios /= 0) then
            if (present(stat)) stat = 1
            return
        end if
        ! Validate second range [0,59]
        if (dt%second < 0 .or. dt%second > 59) then
            if (present(stat)) stat = 1
            return
        end if

        if (slen == 19) return

        ms_end = 19
        if (str(20:20) == '.') then
            if (slen < 23) then
                if (present(stat)) stat = 1
                return
            end if
            read(str(21:23), '(I3)', iostat=ios) &
                dt%millisecond
            if (ios /= 0) then
                if (present(stat)) stat = 1
                return
            end if
            ! Validate millisecond range [0,999]
            if (dt%millisecond < 0 .or. &
                dt%millisecond > 999) then
                if (present(stat)) stat = 1
                return
            end if
            ms_end = 23
        end if

        if (slen <= ms_end) return

        sign_ch = str(ms_end+1:ms_end+1)
        if (sign_ch == 'Z' .or. sign_ch == 'z') then
            dt%utc_offset_minutes = 0
        else if (sign_ch == '+' .or. sign_ch == '-') then
            if (slen < ms_end + 6) then
                if (present(stat)) stat = 1
                return
            end if
            read(str(ms_end+2:ms_end+3), '(I2)', &
                 iostat=ios) off_h
            if (ios /= 0) then
                if (present(stat)) stat = 1
                return
            end if
            ! Require ':' between offset hours and minutes
            if (str(ms_end+4:ms_end+4) /= ':') then
                if (present(stat)) stat = 1
                return
            end if
            read(str(ms_end+5:ms_end+6), '(I2)', &
                 iostat=ios) off_m
            if (ios /= 0) then
                if (present(stat)) stat = 1
                return
            end if
            ! Validate timezone offset ranges
            if (off_h < 0 .or. off_h > 23 .or. &
                off_m < 0 .or. off_m > 59) then
                if (present(stat)) stat = 1
                return
            end if
            dt%utc_offset_minutes = off_h * 60 + off_m
            if (sign_ch == '-') &
                dt%utc_offset_minutes = &
                    -dt%utc_offset_minutes
        else
            if (present(stat)) stat = 1
            return
        end if
    end function parse_datetime

    pure elemental function is_leap_year_int(year) &
        result(res)
        !! version: experimental
        !!
        !! Check if a year is a leap year.
        integer, intent(in) :: year
        logical :: res
        res = (mod(year, 4) == 0 &
               .and. mod(year, 100) /= 0) &
              .or. (mod(year, 400) == 0)
    end function is_leap_year_int

    pure elemental function is_leap_year_dt(dt) &
        result(res)
        !! version: experimental
        !!
        !! Check if a datetime's year is a leap year.
        type(datetime_type), intent(in) :: dt
        logical :: res
        res = is_leap_year_int(dt%year)
    end function is_leap_year_dt

    pure function days_in_month(month, year) result(d)
        !! version: experimental
        !!
        !! Return the number of days in a given month.
        integer, intent(in) :: month, year
        integer :: d
        integer, parameter :: mdays(12) = &
            [31,28,31,30,31,30,31,31,30,31,30,31]
        if (month < 1 .or. month > 12) then
            d = 0
            return
        end if
        d = mdays(month)
        if (month == 2 .and. is_leap_year_int(year)) &
            d = 29
    end function days_in_month

    pure function days_in_year(year) result(d)
        !! version: experimental
        !!
        !! Return 366 for leap years, 365 otherwise.
        integer, intent(in) :: year
        integer :: d
        d = merge(366, 365, is_leap_year_int(year))
    end function days_in_year

    pure function day_of_year(dt) result(doy)
        !! version: experimental
        !!
        !! Return the ordinal day of the year (1-366).
        type(datetime_type), intent(in) :: dt
        integer :: doy
        integer, parameter :: cum(12) = &
            [0,31,59,90,120,151,181,212,243,273,304,334]
        ! Guard against invalid month values
        if (dt%month < 1 .or. dt%month > 12) then
            doy = 0
            return
        end if
        doy = cum(dt%month) + dt%day
        if (dt%month > 2 .and. is_leap_year_int(dt%year))&
            doy = doy + 1
    end function day_of_year

    pure function day_of_week(dt) result(dow)
        !! version: experimental
        !!
        !! Return ISO weekday (1=Monday ... 7=Sunday).
        type(datetime_type), intent(in) :: dt
        integer :: dow
        integer :: y, w
        integer, parameter :: t(12) = &
            [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
        ! Guard against invalid month values
        if (dt%month < 1 .or. dt%month > 12) then
            dow = 0
            return
        end if
        y = dt%year
        if (dt%month < 3) y = y - 1
        w = mod(y + y/4 - y/100 + y/400 &
                + t(dt%month) + dt%day, 7)
        dow = mod(w + 6, 7) + 1
    end function day_of_week

    pure function to_utc(dt) result(utc_dt)
        !! version: experimental
        !!
        !! Convert a datetime to UTC.
        type(datetime_type), intent(in) :: dt
        type(datetime_type) :: utc_dt
        utc_dt = epoch_ms_to_dt(dt_to_utc_ms(dt), 0)
    end function to_utc

    pure function total_seconds(td) result(secs)
        !! version: experimental
        !!
        !! Return the total duration in seconds as real(dp).
        type(timedelta_type), intent(in) :: td
        real(dp) :: secs
        secs = real(td%days, dp) * 86400.0_dp &
             + real(td%seconds, dp) &
             + real(td%milliseconds, dp) * 0.001_dp
    end function total_seconds

    pure function days_from_civil(y, m, d) result(days)
        !! Convert civil date to days since 1970-01-01.
        !! Howard Hinnant's algorithm (public domain).
        integer, intent(in) :: y, m, d
        integer(int64) :: days
        integer :: yr, era, yoe, doy, doe, mp
        yr = y
        if (m <= 2) yr = yr - 1
        if (yr >= 0) then
            era = yr / 400
        else
            era = (yr - 399) / 400
        end if
        yoe = yr - era * 400
        if (m > 2) then
            mp = m - 3
        else
            mp = m + 9
        end if
        doy = (153 * mp + 2) / 5 + d - 1
        doe = yoe * 365 + yoe/4 - yoe/100 + doy
        days = int(era, int64) * 146097_int64 &
             + int(doe, int64) - 719468_int64
    end function days_from_civil

    pure subroutine civil_from_days(z, y, m, d)
        !! Convert days since 1970-01-01 to civil date.
        !! Howard Hinnant's algorithm (public domain).
        integer(int64), intent(in) :: z
        integer, intent(out) :: y, m, d
        integer(int64) :: zz, era64
        integer :: doe, yoe, doy, mp, era
        zz = z + 719468_int64
        if (zz >= 0) then
            era64 = zz / 146097_int64
        else
            era64 = (zz - 146096_int64) / 146097_int64
        end if
        era = int(era64)
        doe = int(zz - era64 * 146097_int64)
        yoe = (doe - doe/1461 + doe/36524 &
               - doe/146096) / 365
        y = yoe + era * 400
        doy = doe - (365*yoe + yoe/4 - yoe/100)
        mp = (5*doy + 2) / 153
        d = doy - (153*mp + 2)/5 + 1
        if (mp < 10) then
            m = mp + 3
        else
            m = mp - 9
        end if
        if (m <= 2) y = y + 1
    end subroutine civil_from_days

    pure function dt_to_epoch_ms(dt) result(ms)
        !! Datetime to milliseconds since epoch (local).
        type(datetime_type), intent(in) :: dt
        integer(int64) :: ms
        ms = days_from_civil(dt%year, dt%month, dt%day) &
                * MS_PER_DAY &
           + int(dt%hour, int64)        * MS_PER_HOUR &
           + int(dt%minute, int64)      * MS_PER_MIN &
           + int(dt%second, int64)      * MS_PER_SEC &
           + int(dt%millisecond, int64)
    end function dt_to_epoch_ms

    pure function dt_to_utc_ms(dt) result(ms)
        !! Datetime to UTC milliseconds since epoch.
        type(datetime_type), intent(in) :: dt
        integer(int64) :: ms
        ms = dt_to_epoch_ms(dt) &
           - int(dt%utc_offset_minutes, int64) * MS_PER_MIN
    end function dt_to_utc_ms

    pure function epoch_ms_to_dt(ms, utc_offset) result(dt)
        !! Milliseconds since epoch to datetime.
        integer(int64), intent(in) :: ms
        integer, intent(in) :: utc_offset
        type(datetime_type) :: dt
        integer(int64) :: d, rem
        d = ms / MS_PER_DAY
        rem = ms - d * MS_PER_DAY
        if (rem < 0) then
            d = d - 1_int64
            rem = rem + MS_PER_DAY
        end if
        call civil_from_days(d, dt%year, dt%month, dt%day)
        dt%hour        = int(rem / MS_PER_HOUR)
        rem = mod(rem, MS_PER_HOUR)
        dt%minute      = int(rem / MS_PER_MIN)
        rem = mod(rem, MS_PER_MIN)
        dt%second      = int(rem / MS_PER_SEC)
        dt%millisecond = int(mod(rem, MS_PER_SEC))
        dt%utc_offset_minutes = utc_offset
    end function epoch_ms_to_dt

    pure function td_to_ms(td) result(ms)
        !! Timedelta to total milliseconds.
        type(timedelta_type), intent(in) :: td
        integer(int64) :: ms
        ms = int(td%days, int64)         * MS_PER_DAY &
           + int(td%seconds, int64)      * MS_PER_SEC &
           + int(td%milliseconds, int64)
    end function td_to_ms

    pure function ms_to_td(ms) result(td)
        !! Total milliseconds to normalized timedelta.
        integer(int64), intent(in) :: ms
        type(timedelta_type) :: td
        integer(int64) :: rem
        td%days = int(ms / MS_PER_DAY)
        rem = ms - int(td%days, int64) * MS_PER_DAY
        if (rem < 0) then
            td%days = td%days - 1
            rem = rem + MS_PER_DAY
        end if
        td%seconds      = int(rem / MS_PER_SEC)
        td%milliseconds = int(mod(rem, MS_PER_SEC))
    end function ms_to_td

end module stdlib_datetime
