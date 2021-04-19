---
title: string handling
---

# The `stdlib_strings` module

[TOC]

## Introduction

The `stdlib_strings` module provides basic string handling and manipulation routines.


## Procedures and methods provided


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `strip`

#### Description

Remove leading and trailing whitespace characters.

#### Syntax

`string = [[stdlib_strings(module):strip(interface)]] (string)`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).

#### Result value

The result is of the same type as `string`.

#### Example

```fortran
program demo
  use stdlib_ascii, only : TAB, VT, NUL, LF, CR, FF
  use stdlib_strings, only : strip
  implicit none
  print'(a)', strip("   hello   ")             ! "hello"
  print'(a)', strip(TAB//"goodbye"//CR//LF)    ! "goodbye"
  print'(a)', strip(" "//TAB//LF//VT//FF//CR)  ! ""
  print'(a)', strip("  !  ")//"!"              ! "!!"
  print'(a)', strip("Hello")                   ! "Hello"
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `chomp`

#### Description

Remove trailing characters in *set* or *substring* from *string*.
If no character *set* or *substring* is provided trailing whitespace is removed.

#### Syntax

`string = [[stdlib_strings(module):chomp(interface)]] (string[, set|substring])`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `set`: Array of length one character. This argument is intent(in).
- `substring`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).

#### Result value

The result is of the same type as `string`.

#### Example

```fortran
program demo
  use stdlib_ascii, only : TAB, VT, NUL, LF, CR, FF
  use stdlib_strings, only : chomp
  implicit none
  print'(a)', chomp("   hello   ")             ! "   hello"
  print'(a)', chomp(TAB//"goodbye"//CR//LF)    ! "\tgoodbye"
  print'(a)', chomp(" "//TAB//LF//VT//FF//CR)  ! ""
  print'(a)', chomp("  !  ")//"!"              ! "  !!"
  print'(a)', chomp("Hello")                   ! "Hello"
  print'(a)', chomp("hello", ["l", "o"])       ! "he"
  print'(a)', chomp("hello", set=["l", "o"])   ! "he"
  print'(a)', chomp("hello", "lo")             ! "hel"
  print'(a)', chomp("hello", substring="lo")   ! "hel"
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `starts_with`

#### Description

Check if a *string* starts with a given *substring*.

#### Syntax

`string = [[stdlib_strings(module):starts_with(interface)]] (string, substring)`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `substring`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).

#### Result value

The result is of scalar logical type.

#### Example

```fortran
program demo
  use stdlib_strings, only : starts_with
  implicit none
  print'(a)', starts_with("pattern", "pat")  ! T
  print'(a)', starts_with("pattern", "ern")  ! F
end program demo
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `ends_with`

#### Description

Check if a *string* ends with a given *substring*.

#### Syntax

`string = [[stdlib_strings(module):ends_with(interface)]] (string, substring)`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `substring`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).

#### Result value

The result is of scalar logical type.

#### Example

```fortran
program demo
  use stdlib_strings, only : ends_with
  implicit none
  print'(a)', ends_with("pattern", "ern")  ! T
  print'(a)', ends_with("pattern", "pat")  ! F
end program demo
```
