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
If `consider_overlapping` is not provided or is set to `.true.` the function counts two overlapping occurrences of substring as two different occurrences.  
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

The result is a scalar of integer type or integer array of rank equal to the highest rank among all dummy arguments.

#### Example

```fortran
program demo_find
  use stdlib_string_type, only: string_type, assignment(=)
  use stdlib_strings, only : find
  implicit none
  string_type :: string

  string = "needle in the character-stack"

  print *, find(string, "needle")                       ! 1
  print *, find(string, ["a", "c"], [3, 2])             ! [27, 20]
  print *, find("qwqwqwq", "qwq", 3, [.false., .true.]) ! [0, 5]

end program demo_find
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `format_string`

#### Description

Format or transfer a integer/real/complex/logical variable as a character sequence.


#### Syntax

`format_string = [[stdlib_strings(module):format_string(interface)]] (value [, format])`

#### Status

Experimental

#### Class

Pure function

#### Argument

- `value`: Integer/real/complex/logical scalar.
  This argument is intent(in).
- `format`: Character scalar like `'(F6.2)'`.
  This argument is intent(in) and optional.

#### Result value

The result is a allocatable length Character scalar.

#### Example

```fortran
program test_strings_format_string
    use stdlib_strings, only: format_string, starts_with
    use stdlib_error, only: check
    use stdlib_optval, only: optval
    implicit none
    print *, 'format_string(complex) : '
    call check_formatter(format_string((1, 1)), "(1.0", &
        & "Default formatter for complex number", partial=.true.)
    call check_formatter(format_string((1, 1), '(F6.2)'), "(  1.00,  1.00)", &
        & "Formatter for complex number")
    call check_formatter(format_string((-1, -1), '(F6.2)'), "( -1.00, -1.00)", &
        & "Formatter for negative complex number")
    call check_formatter(format_string((1, 1), '(SP,F6.2)'), "( +1.00, +1.00)", &
        & "Formatter with sign control descriptor for complex number")
    call check_formatter(format_string((1, 1), '(F6.2)')//format_string((2, 2), '(F7.3)'), &
        & "(  1.00,  1.00)(  2.000,  2.000)", &
        & "Multiple formatters for complex numbers")
    print *, 'format_string(integer) : '
    call check_formatter(format_string(100), "100", &
        & "Default formatter for integer number")
    call check_formatter(format_string(100, '(I6)'), "   100", &
        & "Formatter for integer number")
    call check_formatter(format_string(100, '(I0.6)'), "000100", &
        & "Formatter with zero padding for integer number")
    call check_formatter(format_string(100, '(I6)')//format_string(1000, '(I7)'), &
        & "   100   1000", &
        & "Multiple formatters for integers")
    call check_formatter(format_string(34, '(B8)'), "  100010", &
        & "Binary formatter for integer number")
    call check_formatter(format_string(34, '(O0.3)'), "042", &
        & "Octal formatter with zero padding for integer number")
    call check_formatter(format_string(34, '(Z3)'), " 22", &
        & "Hexadecimal formatter for integer number")
    print *, 'format_string(real) : '
    call check_formatter(format_string(100.), "100.0", &
        & "Default formatter for real number", partial=.true.)
    call check_formatter(format_string(100., '(F6.2)'), "100.00", &
        & "Formatter for real number")
    call check_formatter(format_string(289., '(E7.2)'), ".29E+03", &
        & "Exponential formatter with rounding for real number")
    call check_formatter(format_string(128., '(ES8.2)'), "1.28E+02", &
        & "Exponential formatter for real number")
    ! Wrong demonstration
    call check_formatter(format_string(-100., '(F6.2)'), "*", &
        & "Too narrow formatter for signed real number", partial=.true.)
    call check_formatter(format_string(1000., '(F6.3)'), "*", &
        & "Too narrow formatter for real number", partial=.true.)
    call check_formatter(format_string(1000, '(F7.3)'), "*", &
        & "Real formatter for integer number", partial=.true.)
    print *, 'format_string(logical) : '
    call check_formatter(format_string(.true.), "T", &
        & "Default formatter for logcal value")
    call check_formatter(format_string(.true., '(L2)'), " T", &
        & "Formatter for logical value")
    call check_formatter(format_string(.false., '(L2)')//format_string(.true., '(L5)'), &
        & " F    T", &
        & "Multiple formatters for logical values")
    ! Wrong demonstration
    call check_formatter(format_string(.false., '(I5)'), "*", &
        & "Integer formatter for logical value", partial=.true.)

contains
    subroutine check_formatter(actual, expected, description, partial)
        character(len=*), intent(in) :: actual, expected, description
        logical, intent(in), optional :: partial
        logical :: stat
        character(len=:), allocatable :: msg

        if (optval(partial, .false.)) then
            stat = starts_with(actual, expected)
        else
            stat = actual == expected
        end if
        if (.not.stat) then
            msg = description // new_line("a") // &
                & "Expected: '"//expected//"' but got '"//actual//"'"
        else
            print '(" - ", a, /, "   Result: ''", a, "''")', description, actual
        end if
        call check(stat, msg)
    end subroutine check_formatter
end program test_strings_format_string
```
**Results**
```fortran
 format_string(complex) : 
 - Default formatter for complex number
   Result: '(1.00000000,1.00000000)'    !! Different compilers have different widths here.
                                        !! [link](https://github.com/fortran-lang/stdlib/pull/444#issuecomment-868965643)
 - Formatter for complex number
   Result: '(  1.00,  1.00)'
 - Formatter for negative complex number
   Result: '( -1.00, -1.00)'
 - Formatter with sign control descriptor for complex number
   Result: '( +1.00, +1.00)'
 - Multiple formatters for complex numbers
   Result: '(  1.00,  1.00)(  2.000,  2.000)'
 format_string(integer) :
 - Default formatter for integer number
   Result: '100'
 - Formatter for integer number
   Result: '   100'
 - Formatter with zero padding for integer number
   Result: '000100'
 - Multiple formatters for integers
   Result: '   100   1000'
 - Binary formatter for integer number
   Result: '  100010'
 - Octal formatter with zero padding for integer number
   Result: '042'
 - Hexadecimal formatter for integer number
   Result: ' 22'
 format_string(real) :
 - Default formatter for real number
   Result: '100.000000'                 !! Ditto
 - Formatter for real number
   Result: '100.00'
 - Exponential formatter with rounding for real number
   Result: '.29E+03'
 - Exponential formatter for real number
   Result: '1.28E+02'
 - Too narrow formatter for signed real number
   Result: '******'
 - Too narrow formatter for real number
   Result: '******'
 - Real formatter for integer number
   Result: '*'
 format_string(logical) :
 - Default formatter for logcal value
   Result: 'T'
 - Formatter for logical value
   Result: ' T'
 - Multiple formatters for logical values
   Result: ' F    T'
 - Integer formatter for logical value
   Result: '*'
```