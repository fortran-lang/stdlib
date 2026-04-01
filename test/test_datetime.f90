module test_datetime
    use testdrive, only: new_unittest, unittest_type, &
                         error_type, check
    use stdlib_datetime
    implicit none
    private
    public :: collect_datetime

contains

subroutine collect_datetime(testsuite)
    !> Collection of tests
    type(unittest_type), allocatable, intent(out) :: &
        testsuite(:)
    testsuite = [ &
        new_unittest("leap_year_basic", &
                     test_leap_year_basic), &
        new_unittest("leap_year_century", &
                     test_leap_year_century), &
        new_unittest("days_in_month_basic", &
                     test_days_in_month_basic), &
        new_unittest("days_in_month_feb_leap", &
                     test_days_in_month_feb_leap), &
        new_unittest("days_in_year_test", &
                     test_days_in_year), &
        new_unittest("day_of_year_test", &
                     test_day_of_year), &
        new_unittest("day_of_week_known", &
                     test_day_of_week_known), &
        new_unittest("constructor_datetime", &
                     test_constructor_datetime), &
        new_unittest("constructor_timedelta", &
                     test_constructor_timedelta), &
        new_unittest("timedelta_normalization", &
                     test_timedelta_normalization), &
        new_unittest("epoch_value", &
                     test_epoch_value), &
        new_unittest("dt_plus_td", &
                     test_dt_plus_td), &
        new_unittest("dt_minus_td", &
                     test_dt_minus_td), &
        new_unittest("dt_minus_dt", &
                     test_dt_minus_dt), &
        new_unittest("td_plus_td", &
                     test_td_plus_td), &
        new_unittest("td_minus_td", &
                     test_td_minus_td), &
        new_unittest("td_negate", &
                     test_td_negate_op), &
        new_unittest("dt_comparison_eq", &
                     test_dt_eq), &
        new_unittest("dt_comparison_lt_gt", &
                     test_dt_lt_gt), &
        new_unittest("dt_comparison_timezone", &
                     test_dt_cmp_timezone), &
        new_unittest("td_comparison", &
                     test_td_comparison), &
        new_unittest("parse_date_only", &
                     test_parse_date_only), &
        new_unittest("parse_datetime_utc", &
                     test_parse_datetime_utc), &
        new_unittest("parse_datetime_offset", &
                     test_parse_datetime_offset), &
        new_unittest("parse_datetime_ms", &
                     test_parse_datetime_ms), &
        new_unittest("parse_invalid", &
                     test_parse_invalid), &
        new_unittest("format_datetime_utc", &
                     test_format_datetime_utc), &
        new_unittest("format_datetime_offset", &
                     test_format_datetime_offset), &
        new_unittest("format_timedelta_test", &
                     test_format_timedelta), &
        new_unittest("timedelta_ms_rollover", &
                     test_timedelta_ms_rollover), &
        new_unittest("to_utc_test", &
                     test_to_utc), &
        new_unittest("total_seconds_test", &
                     test_total_seconds), &
        new_unittest("add_30_days", &
                     test_add_30_days), &
        new_unittest("midnight_rollover", &
                     test_midnight_rollover), &
        new_unittest("year_boundary", &
                     test_year_boundary), &
        new_unittest("now_returns_valid", &
                     test_now_valid)]
end subroutine collect_datetime

subroutine test_leap_year_basic(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, is_leap_year(2000), &
        "2000 should be a leap year")
    if (allocated(error)) return
    call check(error, is_leap_year(2024), &
        "2024 should be a leap year")
    if (allocated(error)) return
    call check(error, .not. is_leap_year(2023), &
        "2023 should not be a leap year")
    if (allocated(error)) return
    call check(error, .not. is_leap_year(2025), &
        "2025 should not be a leap year")
    if (allocated(error)) return
end subroutine test_leap_year_basic

subroutine test_leap_year_century(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, .not. is_leap_year(1900), &
        "1900 should not be a leap year")
    if (allocated(error)) return
    call check(error, .not. is_leap_year(2100), &
        "2100 should not be a leap year")
    if (allocated(error)) return
    call check(error, is_leap_year(2400), &
        "2400 should be a leap year")
    if (allocated(error)) return
end subroutine test_leap_year_century

subroutine test_days_in_month_basic(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, days_in_month(1, 2026) == 31, &
        "January 2026 should have 31 days")
    if (allocated(error)) return
    call check(error, days_in_month(4, 2026) == 30, &
        "April 2026 should have 30 days")
    if (allocated(error)) return
    call check(error, days_in_month(2, 2026) == 28, &
        "February 2026 should have 28 days")
    if (allocated(error)) return
end subroutine test_days_in_month_basic

subroutine test_days_in_month_feb_leap(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, days_in_month(2, 2024) == 29, &
        "February 2024 (leap) should have 29 days")
    if (allocated(error)) return
    call check(error, days_in_month(2, 2000) == 29, &
        "February 2000 (leap) should have 29 days")
    if (allocated(error)) return
    call check(error, days_in_month(2, 1900) == 28, &
        "February 1900 (non-leap) should have 28 days")
    if (allocated(error)) return
end subroutine test_days_in_month_feb_leap

subroutine test_days_in_year(error)
    type(error_type), allocatable, intent(out) :: error
    call check(error, days_in_year(2024) == 366, &
        "2024 (leap) should have 366 days")
    if (allocated(error)) return
    call check(error, days_in_year(2026) == 365, &
        "2026 (non-leap) should have 365 days")
    if (allocated(error)) return
end subroutine test_days_in_year

subroutine test_day_of_year(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    dt = datetime_type(2026, 1, 1, 0, 0, 0, 0, 0)
    call check(error, day_of_year(dt) == 1, &
        "Jan 1 should be day 1 of year")
    if (allocated(error)) return
    dt = datetime_type(2026, 3, 17, 0, 0, 0, 0, 0)
    call check(error, day_of_year(dt) == 76, &
        "Mar 17, 2026 should be day 76")
    if (allocated(error)) return
    dt = datetime_type(2024, 12, 31, 0, 0, 0, 0, 0)
    call check(error, day_of_year(dt) == 366, &
        "Dec 31, 2024 (leap) should be day 366")
    if (allocated(error)) return
end subroutine test_day_of_year

subroutine test_day_of_week_known(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    dt = datetime_type(2026, 3, 17, 0, 0, 0, 0, 0)
    call check(error, day_of_week(dt) == 2, &
        "2026-03-17 should be Tuesday (2)")
    if (allocated(error)) return
    dt = datetime_type(1970, 1, 1, 0, 0, 0, 0, 0)
    call check(error, day_of_week(dt) == 4, &
        "1970-01-01 should be Thursday (4)")
    if (allocated(error)) return
    dt = datetime_type(2000, 1, 1, 0, 0, 0, 0, 0)
    call check(error, day_of_week(dt) == 6, &
        "2000-01-01 should be Saturday (6)")
    if (allocated(error)) return
end subroutine test_day_of_week_known

subroutine test_constructor_datetime(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    dt = datetime(year=2026, month=3, day=17, &
                  hour=12, minute=30, second=45)
    call check(error, dt%year == 2026, &
        "datetime year should be 2026")
    if (allocated(error)) return
    call check(error, dt%month == 3, &
        "datetime month should be 3")
    if (allocated(error)) return
    call check(error, dt%day == 17, &
        "datetime day should be 17")
    if (allocated(error)) return
    call check(error, dt%hour == 12, &
        "datetime hour should be 12")
    if (allocated(error)) return
    call check(error, dt%minute == 30, &
        "datetime minute should be 30")
    if (allocated(error)) return
    call check(error, dt%second == 45, &
        "datetime second should be 45")
    if (allocated(error)) return
end subroutine test_constructor_datetime

subroutine test_constructor_timedelta(error)
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td
    td = timedelta(days=5, hours=3, minutes=30)
    call check(error, td%days == 5, &
        "timedelta days should be 5")
    if (allocated(error)) return
    call check(error, td%seconds == 12600, &
        "timedelta seconds should be 12600")
    if (allocated(error)) return
end subroutine test_constructor_timedelta

subroutine test_timedelta_normalization(error)
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td
    td = timedelta(hours=25)
    call check(error, td%days == 1, &
        "25 hours should normalize to 1 day")
    if (allocated(error)) return
    call check(error, td%seconds == 3600, &
        "25 hours should normalize to 3600 remaining seconds")
    if (allocated(error)) return
    td = timedelta(seconds=-1)
    call check(error, td%days == -1, &
        "-1 second should normalize to -1 day")
    if (allocated(error)) return
    call check(error, td%seconds == 86399, &
        "-1 second should normalize to 86399 seconds")
    if (allocated(error)) return
end subroutine test_timedelta_normalization

subroutine test_epoch_value(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    dt = epoch()
    call check(error, dt%year == 1970, &
        "epoch year should be 1970")
    if (allocated(error)) return
    call check(error, dt%month == 1, &
        "epoch month should be 1")
    if (allocated(error)) return
    call check(error, dt%day == 1, &
        "epoch day should be 1")
    if (allocated(error)) return
end subroutine test_epoch_value

subroutine test_dt_plus_td(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt, res
    type(timedelta_type) :: td
    dt = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    td = timedelta(days=1, hours=6)
    res = dt + td
    call check(error, res%day == 18, &
        "dt + td day should be 18")
    if (allocated(error)) return
    call check(error, res%hour == 18, &
        "dt + td hour should be 18")
    if (allocated(error)) return
    res = td + dt
    call check(error, res%day == 18, &
        "td + dt day should be 18 (commutative)")
    if (allocated(error)) return
end subroutine test_dt_plus_td

subroutine test_dt_minus_td(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt, res
    type(timedelta_type) :: td
    dt = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    td = timedelta(days=17)
    res = dt - td
    call check(error, res%month == 2, &
        "dt - 17 days month should be February")
    if (allocated(error)) return
    call check(error, res%day == 28, &
        "dt - 17 days day should be 28")
    if (allocated(error)) return
end subroutine test_dt_minus_td

subroutine test_dt_minus_dt(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt1, dt2
    type(timedelta_type) :: td
    dt1 = datetime_type(2026, 3, 17, 0, 0, 0, 0, 0)
    dt2 = datetime_type(2026, 3, 10, 0, 0, 0, 0, 0)
    td = dt1 - dt2
    call check(error, td%days == 7, &
        "difference should be 7 days")
    if (allocated(error)) return
    call check(error, td%seconds == 0, &
        "difference seconds should be 0")
    if (allocated(error)) return
end subroutine test_dt_minus_dt

subroutine test_td_plus_td(error)
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td1, td2, res
    td1 = timedelta(days=1, hours=12)
    td2 = timedelta(hours=18)
    res = td1 + td2
    call check(error, res%days == 2, &
        "td1 + td2 days should be 2")
    if (allocated(error)) return
    call check(error, res%seconds == 21600, &
        "td1 + td2 seconds should be 21600")
    if (allocated(error)) return
end subroutine test_td_plus_td

subroutine test_td_minus_td(error)
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td1, td2, res
    td1 = timedelta(days=5)
    td2 = timedelta(days=3)
    res = td1 - td2
    call check(error, res%days == 2, &
        "td1 - td2 days should be 2")
    if (allocated(error)) return
end subroutine test_td_minus_td

subroutine test_td_negate_op(error)
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td, res
    td = timedelta(days=3, hours=6)
    res = -td
    call check(error, res%days == -4, &
        "negated days should be -4")
    if (allocated(error)) return
    call check(error, res%seconds == 64800, &
        "negated seconds should be 64800")
    if (allocated(error)) return
end subroutine test_td_negate_op

subroutine test_dt_eq(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt1, dt2
    dt1 = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    dt2 = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    call check(error, dt1 == dt2, &
        "identical datetimes should be equal")
    if (allocated(error)) return
    call check(error, .not. (dt1 /= dt2), &
        "identical datetimes should not be unequal")
    if (allocated(error)) return
end subroutine test_dt_eq

subroutine test_dt_lt_gt(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt1, dt2
    dt1 = datetime_type(2026, 3, 17, 11, 0, 0, 0, 0)
    dt2 = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    call check(error, dt1 < dt2, &
        "earlier datetime should be less than later")
    if (allocated(error)) return
    call check(error, dt2 > dt1, &
        "later datetime should be greater than earlier")
    if (allocated(error)) return
    call check(error, dt1 <= dt2, &
        "earlier datetime should be <= later")
    if (allocated(error)) return
    call check(error, dt2 >= dt1, &
        "later datetime should be >= earlier")
    if (allocated(error)) return
end subroutine test_dt_lt_gt

subroutine test_dt_cmp_timezone(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt1, dt2
    dt1 = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    dt2 = datetime_type(2026,3,17, 17,30, 0, 0, 330)
    call check(error, dt1 == dt2, &
        "UTC 12:00 should equal +05:30 17:30")
    if (allocated(error)) return
end subroutine test_dt_cmp_timezone

subroutine test_td_comparison(error)
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td1, td2
    td1 = timedelta(days=1)
    td2 = timedelta(hours=25)
    call check(error, td2 > td1, &
        "25 hours should be greater than 1 day")
    if (allocated(error)) return
    td2 = timedelta(hours=24)
    call check(error, td1 == td2, &
        "1 day should equal 24 hours")
    if (allocated(error)) return
end subroutine test_td_comparison

subroutine test_parse_date_only(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    integer :: stat
    dt = parse_datetime('2026-03-17', stat)
    call check(error, stat == 0, &
        "parsing '2026-03-17' should succeed")
    if (allocated(error)) return
    call check(error, dt%year == 2026, &
        "parsed year should be 2026")
    if (allocated(error)) return
    call check(error, dt%month == 3, &
        "parsed month should be 3")
    if (allocated(error)) return
    call check(error, dt%day == 17, &
        "parsed day should be 17")
    if (allocated(error)) return
end subroutine test_parse_date_only

subroutine test_parse_datetime_utc(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    integer :: stat
    dt = parse_datetime('2026-03-17T12:00:00Z', stat)
    call check(error, stat == 0, &
        "parsing UTC datetime should succeed")
    if (allocated(error)) return
    call check(error, dt%year == 2026, &
        "parsed year should be 2026")
    if (allocated(error)) return
    call check(error, dt%hour == 12, &
        "parsed hour should be 12")
    if (allocated(error)) return
    call check(error, dt%utc_offset_minutes == 0, &
        "parsed UTC offset should be 0")
    if (allocated(error)) return
end subroutine test_parse_datetime_utc

subroutine test_parse_datetime_offset(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    integer :: stat
    dt = parse_datetime( &
        '2026-03-17T23:05:15+05:30', stat)
    call check(error, stat == 0, &
        "parsing offset datetime should succeed")
    if (allocated(error)) return
    call check(error, dt%hour == 23, &
        "parsed hour should be 23")
    if (allocated(error)) return
    call check(error, dt%minute == 5, &
        "parsed minute should be 5")
    if (allocated(error)) return
    call check(error, dt%utc_offset_minutes == 330, &
        "parsed UTC offset should be 330 minutes")
    if (allocated(error)) return
end subroutine test_parse_datetime_offset

subroutine test_parse_datetime_ms(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    integer :: stat
    dt = parse_datetime( &
        '2026-03-17T12:00:00.500Z', stat)
    call check(error, stat == 0, &
        "parsing datetime with milliseconds should succeed")
    if (allocated(error)) return
    call check(error, dt%millisecond == 500, &
        "parsed millisecond should be 500")
    if (allocated(error)) return
end subroutine test_parse_datetime_ms

subroutine test_parse_invalid(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    integer :: stat
    dt = parse_datetime('bad', stat)
    call check(error, stat /= 0, &
        "parsing 'bad' should fail with non-zero stat")
    if (allocated(error)) return
end subroutine test_parse_invalid

subroutine test_format_datetime_utc(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    dt = datetime_type(2026, 3, 17, 12, 0, 0, 0, 0)
    call check(error, &
        format_datetime(dt) == '2026-03-17T12:00:00Z', &
        "UTC format should be '2026-03-17T12:00:00Z'")
    if (allocated(error)) return
end subroutine test_format_datetime_utc

subroutine test_format_datetime_offset(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    dt = datetime_type(2026,3,17, 23,5,15, 0, 330)
    call check(error, &
        format_datetime(dt) == '2026-03-17T23:05:15+05:30', &
        "offset format should be '2026-03-17T23:05:15+05:30'")
    if (allocated(error)) return
end subroutine test_format_datetime_offset

subroutine test_format_timedelta(error)
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td
    td = timedelta(days=30, hours=1, minutes=30)
    call check(error, &
        format_timedelta(td) == '30 days, 01:30:00', &
        "timedelta format should be '30 days, 01:30:00'")
    if (allocated(error)) return
end subroutine test_format_timedelta

subroutine test_timedelta_ms_rollover(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt, res
    dt = datetime_type(1970, 1, 1, 0, 0, 0, 0, 0)
    res = dt + timedelta(days=0)
    call check(error, res%year == 1970, &
        "epoch + 0 days year should be 1970")
    if (allocated(error)) return
    call check(error, res%month == 1, &
        "epoch + 0 days month should be 1")
    if (allocated(error)) return
    call check(error, res%day == 1, &
        "epoch + 0 days day should be 1")
    if (allocated(error)) return
    dt = datetime_type(2000, 2, 29, 23,59,59, 999, 0)
    res = dt + timedelta(milliseconds=1)
    call check(error, res%month == 3, &
        "leap day ms rollover month should be 3")
    if (allocated(error)) return
    call check(error, res%day == 1, &
        "leap day ms rollover day should be 1")
    if (allocated(error)) return
    call check(error, res%hour == 0, &
        "leap day ms rollover hour should be 0")
    if (allocated(error)) return
    call check(error, res%second == 0, &
        "leap day ms rollover second should be 0")
    if (allocated(error)) return
end subroutine test_timedelta_ms_rollover

subroutine test_to_utc(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt, utc
    dt = datetime_type(2026,3,17, 23,5,15, 0, 330)
    utc = to_utc(dt)
    call check(error, utc%hour == 17, &
        "to_utc hour should be 17")
    if (allocated(error)) return
    call check(error, utc%minute == 35, &
        "to_utc minute should be 35")
    if (allocated(error)) return
    call check(error, utc%utc_offset_minutes == 0, &
        "to_utc offset should be 0")
    if (allocated(error)) return
end subroutine test_to_utc

subroutine test_total_seconds(error)
    use stdlib_kinds, only: dp
    type(error_type), allocatable, intent(out) :: error
    type(timedelta_type) :: td
    td = timedelta(days=1, hours=1)
    call check(error, &
        abs(total_seconds(td) - 90000.0_dp) &
            < 0.001_dp, &
        "total_seconds of 1 day 1 hour should be 90000")
    if (allocated(error)) return
end subroutine test_total_seconds

subroutine test_add_30_days(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt, res
    dt = datetime_type(2026, 1, 15, 0, 0, 0, 0, 0)
    res = dt + timedelta_type(30, 0, 0)
    call check(error, res%month == 2, &
        "Jan 15 + 30 days month should be February")
    if (allocated(error)) return
    call check(error, res%day == 14, &
        "Jan 15 + 30 days day should be 14")
    if (allocated(error)) return
end subroutine test_add_30_days

subroutine test_midnight_rollover(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt, res
    dt = datetime_type(2026,3,17, 23,59,59, 0, 0)
    res = dt + timedelta(seconds=1)
    call check(error, res%day == 18, &
        "midnight rollover day should be 18")
    if (allocated(error)) return
    call check(error, res%hour == 0, &
        "midnight rollover hour should be 0")
    if (allocated(error)) return
    call check(error, res%minute == 0, &
        "midnight rollover minute should be 0")
    if (allocated(error)) return
    call check(error, res%second == 0, &
        "midnight rollover second should be 0")
    if (allocated(error)) return
end subroutine test_midnight_rollover

subroutine test_year_boundary(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt, res
    dt = datetime_type(2025,12,31, 23,59,59, 0, 0)
    res = dt + timedelta(seconds=1)
    call check(error, res%year == 2026, &
        "year boundary rollover year should be 2026")
    if (allocated(error)) return
    call check(error, res%month == 1, &
        "year boundary rollover month should be 1")
    if (allocated(error)) return
    call check(error, res%day == 1, &
        "year boundary rollover day should be 1")
    if (allocated(error)) return
end subroutine test_year_boundary

subroutine test_now_valid(error)
    type(error_type), allocatable, intent(out) :: error
    type(datetime_type) :: dt
    dt = now()
    call check(error, &
        dt%year >= 1 .and. dt%year <= 9999, &
        "now() year should be in valid range")
    if (allocated(error)) return
    call check(error, &
        dt%month >= 1 .and. dt%month <= 12, &
        "now() month should be in valid range")
    if (allocated(error)) return
    call check(error, &
        dt%day >= 1 .and. dt%day <= 31, &
        "now() day should be in valid range")
    if (allocated(error)) return
end subroutine test_now_valid

end module test_datetime


program tester
    use iso_fortran_env
    use testdrive, only: run_testsuite, new_testsuite, &
                         testsuite_type
    use test_datetime, only: collect_datetime
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    integer :: stat, is

    stat = 0
    testsuites = [ &
        new_testsuite("datetime", collect_datetime)]

    do is = 1, size(testsuites)
        write(error_unit, fmt) &
            "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, &
                           error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, &
            "test(s) failed!"
        error stop
    end if

end program tester
