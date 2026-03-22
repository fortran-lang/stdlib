---
title: datetime
---

[TOC]

## Introduction

The `stdlib_datetime` module provides types and procedures for date, time, and duration handling. It defines two primary derived types — `datetime_type` for representing specific points in time and `timedelta_type` for representing durations — along with arithmetic operators, comparison operators, ISO 8601 parsing/formatting, and calendar utilities.

No C-bindings or parameterized derived types are required. The module uses only standard Fortran intrinsics, calendar arithmetic, and string manipulation.

### Status

Experimental

## Derived Types

### `datetime_type`

Represents a specific point in time.

| Component | Type | Default | Description |
|-----------|------|---------|-------------|
| `year` | `integer` | 1 | Year (1–9999) |
| `month` | `integer` | 1 | Month (1–12) |
| `day` | `integer` | 1 | Day (1–31) |
| `hour` | `integer` | 0 | Hour (0–23) |
| `minute` | `integer` | 0 | Minute (0–59) |
| `second` | `integer` | 0 | Second (0–59) |
| `millisecond` | `integer` | 0 | Millisecond (0–999) |
| `utc_offset_minutes` | `integer` | 0 | UTC offset in minutes. Minutes are used because all real-world UTC offsets are whole multiples of minutes (ISO 8601 specifies offsets as `±HH:MM`). |

### `timedelta_type`

Represents a duration or interval. After normalization, `seconds` is always in [0, 86399] and `milliseconds` is always in [0, 999] (i.e., they are never negative). For negative durations, only the `days` component is negative while `seconds` and `milliseconds` remain non-negative.

| Component | Type | Default | Description |
|-----------|------|---------|-------------|
| `days` | `integer` | 0 | Number of days (can be negative for negative durations) |
| `seconds` | `integer` | 0 | Seconds, always in [0, 86399] after normalization |
| `milliseconds` | `integer` | 0 | Milliseconds, always in [0, 999] after normalization |

## Constructors

### `datetime` — Create from components

#### Status

Experimental

#### Class

Pure function.

#### Description

Creates a `datetime_type` from individual components. All arguments are optional; if omitted, they default to the type's initial values: `year=1`, `month=1`, `day=1`, `hour=0`, `minute=0`, `second=0`, `millisecond=0`, `utc_offset_minutes=0`.

#### Syntax

`dt = ` [[stdlib_datetime(module):datetime(function)]] `([year] [, month] [, day] [, hour] [, minute] [, second] [, millisecond] [, utc_offset_minutes])`

#### Arguments

All arguments are optional with `intent(in)` and type `integer`.

#### Return value

A `datetime_type` value with components equal to those provided, or to the default ones.

### `timedelta` — Create from mixed units

#### Status

Experimental

#### Class

Pure function.

#### Description

Creates a normalized `timedelta_type`. Accepts mixed units (days, hours, minutes, seconds, milliseconds) and normalizes them. If no arguments are provided, all components default to `0`.

#### Syntax

`td = ` [[stdlib_datetime(module):timedelta(function)]] `([days] [, hours] [, minutes] [, seconds] [, milliseconds])`

#### Arguments

All arguments are optional with `intent(in)` and type `integer`.

#### Return value

A normalized `timedelta_type` value with `seconds` in [0, 86399] and `milliseconds` in [0, 999].

### `now` — Current local time

#### Status

Experimental

#### Class

Function (not `pure`; calls the intrinsic `date_and_time` which is impure).

#### Description

Returns the current local date and time from the system clock.

#### Syntax

`dt = ` [[stdlib_datetime(module):now(function)]] `()`

### `now_utc` — Current UTC time

#### Status

Experimental

#### Class

Function (not `pure`; calls `now()` internally).

#### Description

Returns the current UTC date and time.

#### Syntax

`dt = ` [[stdlib_datetime(module):now_utc(function)]] `()`

### `epoch` — Unix epoch

#### Status

Experimental

#### Class

Pure function.

#### Description

Returns the Unix epoch: `1970-01-01T00:00:00Z`. This is a mathematical constant (a fixed date) and does not depend on the operating system.

#### Syntax

`dt = ` [[stdlib_datetime(module):epoch(function)]] `()`

## Operators

### Arithmetic

| Expression | Result Type | Description |
|------------|-------------|-------------|
| `datetime + timedelta` | `datetime_type` | Add duration to timestamp |
| `timedelta + datetime` | `datetime_type` | Commutative form |
| `timedelta + timedelta` | `timedelta_type` | Add two durations |
| `datetime - timedelta` | `datetime_type` | Subtract duration |
| `datetime - datetime` | `timedelta_type` | Difference between two timestamps |
| `timedelta - timedelta` | `timedelta_type` | Subtract durations |
| `-timedelta` | `timedelta_type` | Negate duration |

### Comparison

All six comparison operators are provided for both `datetime_type` and `timedelta_type`: `==`, `/=`, `<`, `<=`, `>`, `>=`.

**Important:** `datetime_type` comparisons convert both operands to UTC internally, so comparing across timezones works correctly.

## Parsing and Formatting

### `parse_datetime` — Parse ISO 8601 string

#### Status

Experimental

#### Class

Function (not `pure`; uses internal `read` statements for parsing).

#### Description

Parses an ISO 8601 date/time string into a `datetime_type`.

#### Syntax

`dt = ` [[stdlib_datetime(module):parse_datetime(function)]] `(str [, stat])`

#### Arguments

`str`: `character(len=*)`, `intent(in)`. The ISO 8601 string to parse.

`stat` (optional): `integer`, `intent(out)`. Returns 0 on success, non-zero on error.

#### Supported formats

- `YYYY-MM-DD`
- `YYYY-MM-DDTHH:MM:SS`
- `YYYY-MM-DDTHH:MM:SSZ`
- `YYYY-MM-DDTHH:MM:SS+HH:MM`
- `YYYY-MM-DDTHH:MM:SS.fffZ`
- `YYYY-MM-DDTHH:MM:SS.fff+HH:MM`

For the `YYYY-MM-DDTHH:MM:SS` form without a timezone designator, the value is interpreted
as UTC and `utc_offset_minutes` is set to `0`. Forms with `Z` or an explicit offset use
the specified UTC offset.

### `format_datetime` — Format as ISO 8601

#### Status

Experimental

#### Class

Function (not `pure`; uses internal `write` statements for formatting).

#### Description

Formats a `datetime_type` as an ISO 8601 string.

#### Syntax

`str = ` [[stdlib_datetime(module):format_datetime(function)]] `(dt)`

#### Return value

`character(:), allocatable` — e.g. `"2026-03-17T12:00:00Z"` or `"2026-03-17T23:05:15+05:30"`.

### `format_timedelta` — Format duration

#### Status

Experimental

#### Class

Function (not `pure`; uses internal `write` statements for formatting).

#### Description

Formats a `timedelta_type` as a human-readable string.

#### Syntax

`str = ` [[stdlib_datetime(module):format_timedelta(function)]] `(td)`

#### Return value

`character(:), allocatable` — e.g. `"30 days, 01:30:00"`.

## Utility Functions

### `is_leap_year`

#### Status

Experimental

#### Class

Pure elemental function / interface.

#### Description

Returns `.true.` if the given year (or datetime's year) is a leap year.

#### Syntax

`result = ` [[stdlib_datetime(module):is_leap_year(interface)]] `(year)` or `(dt)`

### `days_in_month`

#### Status

Experimental

#### Class

Pure function.

#### Description

Returns the number of days in a given month and year.

#### Syntax

`d = ` [[stdlib_datetime(module):days_in_month(function)]] `(month, year)`

### `days_in_year`

#### Status

Experimental

#### Class

Pure function.

#### Description

Returns 365 or 366.

#### Syntax

`d = ` [[stdlib_datetime(module):days_in_year(function)]] `(year)`

### `day_of_year`

#### Status

Experimental

#### Class

Pure function.

#### Description

Returns the ordinal day (1–366).

#### Syntax

`doy = ` [[stdlib_datetime(module):day_of_year(function)]] `(dt)`

### `day_of_week`

#### Status

Experimental

#### Class

Pure function.

#### Description

Returns the ISO weekday (1=Monday, ..., 7=Sunday).

#### Syntax

`dow = ` [[stdlib_datetime(module):day_of_week(function)]] `(dt)`

### `to_utc`

#### Status

Experimental

#### Class

Pure function.

#### Description

Converts a `datetime_type` to UTC.

#### Syntax

`utc_dt = ` [[stdlib_datetime(module):to_utc(function)]] `(dt)`

### `total_seconds`

#### Status

Experimental

#### Class

Pure function.

#### Description

Returns the total duration as `real(dp)`.

`secs = ` [[stdlib_datetime(module):total_seconds(function)]] `(td)`

## Example

```fortran
{!example/datetime/example_datetime_usage.f90!}
```
