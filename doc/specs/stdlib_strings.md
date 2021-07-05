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


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `slice`

#### Description

Extracts the characters from the defined region of the input string by taking strides.

Deduction Process:
Function first automatically deduces the optional arguments that are not provided by the user.  
This process is independent of both input `string` and permitted indexes of Fortran.  
Deduced `first` and `last` argument take +infinity or -infinity value whereas deduced `stride` argument takes +1 or -1 value.

Validation Process:
Argument `first` and `last` defines this region for extraction by function `slice`.  
If the defined region is invalid i.e. region contains atleast one invalid index, `first` and 
`last` are converted to first and last valid indexes in this defined region respectively, 
if no valid index exists in this region an empty string is returned.  
`stride` can attain both negative or positive values but when the only invalid value 
0 is given, it is converted to 1.

Extraction Process:
After all this, extraction starts from `first` index and takes stride of length `stride`.  
Extraction starts only if `last` index is crossable from `first` index with stride `stride` 
and remains active until `last` index is crossed.  

#### Syntax

`string = [[stdlib_strings(module):slice(interface)]] (string [, first, last, stride])`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `first`: integer.
  This argument is intent(in) and optional.
- `last`: integer.
  This argument is intent(in) and optional.
- `stride`: integer.
  This argument is intent(in) and optional.

#### Result value

The result is of the same type as `string`.

#### Example

```fortran
program demo_slice
  use stdlib_string_type
  use stdlib_strings, only : slice
  implicit none
  type(string_type) :: string
  character(len=10) :: char

  string = "abcdefghij"
  ! string <-- "abcdefghij"

  char = "abcdefghij"
  ! char <-- "abcdefghij"

  print'(a)', slice("abcdefghij", 2, 6, 2)   ! "bdf"
  print'(a)', slice(char, 2, 6, 2)           ! "bdf"
  
  string = slice(string, 2, 6, 2)
  ! string <-- "bdf"

end program demo_slice
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `find`

#### Description

Returns the starting index of the `occurrence`th occurrence of the substring `pattern` 
in the input string `string`.  
Default value of `occurrence` is set to `1`. 
If `consider_overlapping` is not provided or is set to `.true.` the function counts two overlapping occurrences of substring `pattern` as two different occurrences.  
If `occurrence`th occurrence is not found, function returns `0`.

#### Syntax

`string = [[stdlib_strings(module):find(interface)]] (string, pattern [, occurrence, consider_overlapping])`

#### Status

Experimental

#### Class

Elemental function

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `pattern`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `occurrence`: integer.
  This argument is intent(in) and optional.
- `consider_overlapping`: logical.
  This argument is intent(in) and optional.

#### Result value

The result is a scalar of integer type or an integer array of rank equal to the highest rank among all dummy arguments.

#### Example

```fortran
program demo_find
  use stdlib_string_type, only: string_type, assignment(=)
  use stdlib_strings, only : find
  implicit none
  type(string_type) :: string

  string = "needle in the character-stack"

  print *, find(string, "needle")                       ! 1
  print *, find(string, ["a", "c"], [3, 2])             ! [27, 20]
  print *, find("qwqwqwq", "qwq", 3, [.false., .true.]) ! [0, 5]

end program demo_find
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `replace_all`

#### Description

Replaces all occurrences of substring `pattern` in the input `string` with the replacement `replacement`.  
Occurrences overlapping on a base occurrence will not be replaced.

#### Syntax

`string = [[stdlib_strings(module):replace_all(interface)]] (string, pattern, replacement)`

#### Status

Experimental

#### Class

Pure function

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `pattern`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `replacement`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).

#### Result value

The result is of the same type as `string`.

#### Example

```fortran
program demo_replace_all
  use stdlib_string_type, only: string_type, assignment(=)
  use stdlib_strings, only : replace_all
  implicit none
  type(string_type) :: string

  string = "hurdles here, hurdles there, hurdles everywhere"
  ! string <-- "hurdles here, hurdles there, hurdles everywhere"

  print'(a)', replace_all(string, "hurdles", "learn from")
  ! "learn from here, learn from there, learn from everywhere"

  string = replace_all(string, "hurdles", "technology")
  ! string <-- "technology here, technology there, technology everywhere"

end program demo_replace_all
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `count`

#### Description

Returns the number of times the substring `pattern` has occurred in the input string `string`.  
If `consider_overlapping` is not provided or is set to `.true.` the function counts two overlapping occurrences of substring `pattern` as two different occurrences.

#### Syntax

`string = [[stdlib_strings(module):count(interface)]] (string, pattern [, consider_overlapping])`

#### Status

Experimental

#### Class

Elemental function

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `pattern`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `consider_overlapping`: logical.
  This argument is intent(in) and optional.

#### Result value

The result is a scalar of integer type or an integer array of rank equal to the highest rank among all dummy arguments.

#### Example

```fortran
program demo_count
  use stdlib_string_type, only: string_type, assignment(=)
  use stdlib_strings, only : count
  implicit none
  type(string_type) :: string

  string = "How much wood would a woodchuck chuck if a woodchuck could chuck wood?"

  print *, count(string, "wood")                                  ! 4
  print *, count(string, ["would", "chuck", "could"])             ! [1, 4, 1]
  print *, count("a long queueueueue", "ueu", [.false., .true.])  ! [2, 4]

end program demo_count
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `format_to_string`

#### Description

Format or transfer a `integer/real/complex/logical` scalar as a string.  
Input a wrong `format` that cause the internal-IO to fail, the result value is a string of `[*]`.

#### Syntax

`format_to_string = [[stdlib_strings(module):format_to_string(interface)]] (value [, format])`

#### Status

Experimental

#### Class

Pure function

#### Argument

- `value`: Shall be an `integer/real/complex/logical` scalar.
  This is an `intent(in)` argument.
- `format`: Shall be a `character` scalar like `'(F6.2)'`.
  This is an `intent(in)` and `optional` argument.

#### Result value

The result is an allocatable length `character` scalar with up to 512 `character` length.

#### Example

```fortran
program demo_format_to_string
    use :: stdlib_strings, only: format_to_string
    implicit none

    print *, 'format_to_string(complex) : '
        print *, format_to_string((1, 1))              ! (1.00000000,1.00000000)
        print *, format_to_string((1, 1), '(F6.2)')    ! (  1.00,  1.00)
        print *, format_to_string((1000, 1), '(ES0.2)'), format_to_string((1000, 1), '(SP,F6.3)')     ! (1.00E+3,1.00)(******,+1.000)
                        !! Too narrow formatter for real number
                        !! Normal demonstration(`******` from Fortran Standard)

    print *, 'format_to_string(integer) : '
        print *, format_to_string(1)                   ! 1
        print *, format_to_string(1, '(I4)')           !     1
        print *, format_to_string(1, '(I0.4)'), format_to_string(2, '(B4)')           ! 0001  10  

    print *, 'format_to_string(real) : '
        print *, format_to_string(1.)                  ! 1.00000000
        print *, format_to_string(1., '(F6.2)')        !   1.00 
        print *, format_to_string(1., '(SP,ES9.2)'), format_to_string(1, '(F7.3)')    ! +1.00E+00[*]
                        !! 1 wrong demonstration(`*` from `format_to_string`)

    print *, 'format_to_string(logical) : '
        print *, format_to_string(.true.)              ! T
        print *, format_to_string(.true., '(L2)')      !  T
        print *, format_to_string(.true., 'L2'), format_to_string(.false., '(I5)')    ! [*][*]
                        !! 2 wrong demonstrations(`*` from `format_to_string`)

end program demo_format_to_string
```