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

Remove trailing characters in *set* from *string*.
If no character *set* is provided trailing whitespace is removed.

#### Syntax

`string = [[stdlib_strings(module):chomp(interface)]] (string[, set])`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `set`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
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
  print'(a)', chomp("hello", set="lo")         ! "he"
end program demo
```
