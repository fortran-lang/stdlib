---
title: strings
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

`string = ` [[stdlib_strings(module):strip(interface)]] ` (string)`

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
{!example/strings/example_strip.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `chomp`

#### Description

Remove trailing characters in *set* or *substring* from *string*.
If no character *set* or *substring* is provided trailing whitespace is removed.

#### Syntax

`string = ` [[stdlib_strings(module):chomp(interface)]] ` (string[, set|substring])`

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
{!example/strings/example_chomp.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `starts_with`

#### Description

Check if a *string* starts with a given *substring*.

#### Syntax

`string = ` [[stdlib_strings(module):starts_with(interface)]] ` (string, substring)`

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
{!example/strings/example_starts_with.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `ends_with`

#### Description

Check if a *string* ends with a given *substring*.

#### Syntax

`string = ` [[stdlib_strings(module):ends_with(interface)]] ` (string, substring)`

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
{!example/strings/example_ends_with.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `slice`

#### Description

Extracts the characters from the defined region of the input string by taking strides.  
Argument `first` and `last` defines this region for extraction by function `slice`.  
Argument `stride` defines the magnitude and direction (+/-) of stride to be taken while extraction. 
`stride` when given invalid value 0, is converted to +1.

Deduction Process:  
Function first automatically deduces the optional arguments that are not provided by the user.  
Deduced `first` and `last` argument take +infinity or -infinity value and deduced `stride` argument 
takes value +1 or -1 depending upon the actual argument(s) provided by the user.  

Extraction Process:  
Extraction starts only if `last` is crossable from `first` with stride of `stride`.  
Extraction starts from the first valid index in the defined region to take stride of `stride` 
and ends when the last valid index in the defined region is crossed.  
If no valid index exists in the defined region, empty string is returned.

#### Syntax

`string = ` [[stdlib_strings(module):slice(interface)]] ` (string [, first, last, stride])`

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
{!example/strings/example_slice.f90!}
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

`string = ` [[stdlib_strings(module):find(interface)]] ` (string, pattern [, occurrence, consider_overlapping])`

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
{!example/strings/example_find.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `replace_all`

#### Description

Replaces all occurrences of substring `pattern` in the input `string` with the replacement `replacement`.  
Occurrences overlapping on a base occurrence will not be replaced.

#### Syntax

`string = ` [[stdlib_strings(module):replace_all(interface)]] ` (string, pattern, replacement)`

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
{!example/strings/example_replace_all.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `padl`

#### Description

Returns a string of length `output_length` left padded with `pad_with` character if it is provided, otherwise with `" "` (1 whitespace).  
If `output_length` is less than or equal to the length of `string`, padding is not performed.

#### Syntax

`string = ` [[stdlib_strings(module):padl(interface)]] ` (string, output_length [, pad_with])`

#### Status

Experimental

#### Class

Pure function

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `output_length`: integer.
  This argument is intent(in).
- `pad_with`: Character scalar of length 1.
  This argument is intent(in) and optional.

#### Result value

The result is of the same type as `string`.

#### Example

```fortran
{!example/strings/example_padl.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `padr`

#### Description

Returns a string of length `output_length` right padded with `pad_with` character if it is provided, otherwise with `" "` (1 whitespace).  
If `output_length` is less than or equal to the length of `string`, padding is not performed.

#### Syntax

`string = ` [[stdlib_strings(module):padr(interface)]] ` (string, output_length [, pad_with])`

#### Status

Experimental

#### Class

Pure function

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `output_length`: integer.
  This argument is intent(in).
- `pad_with`: Character scalar of length 1.
  This argument is intent(in) and optional.

#### Result value

The result is of the same type as `string`.

#### Example

```fortran
{!example/strings/example_padr.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `count`

#### Description

Returns the number of times the substring `pattern` has occurred in the input string `string`.  
If `consider_overlapping` is not provided or is set to `.true.` the function counts two overlapping occurrences of substring `pattern` as two different occurrences.

#### Syntax

`string = ` [[stdlib_strings(module):count(interface)]] ` (string, pattern [, consider_overlapping])`

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
{!example/strings/example_count.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `zfill`

#### Description

Returns a string of length `output_length` left-padded with zeros.
If `output_length` is less than or equal to the length of `string`, padding is not performed.

#### Syntax

`string = ` [[stdlib_strings(module):zfill(interface)]] ` (string, output_length)`

#### Status

Experimental

#### Class

Pure function

#### Argument

- `string`: Character scalar or [[stdlib_string_type(module):string_type(type)]].
  This argument is intent(in).
- `output_length`: integer.
  This argument is intent(in).

#### Result value

The result is of the same type as `string`.

#### Example

```fortran
{!example/strings/example_zfill.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `to_string`

#### Description

Format or transfer a `integer/real/complex/logical` scalar as a string.  
Input a wrong `format` that cause the internal-IO to fail, the result value is a string of `[*]`.

#### Syntax

`string = ` [[stdlib_strings(module):to_string(interface)]] ` (value [, format])`

#### Status

Experimental

#### Class

Pure function.

#### Argument

- `value`: Shall be an `integer/real/complex/logical` scalar.
  This is an `intent(in)` argument.
- `format`: Shall be a `character(len=*)` scalar like `'(F6.2)'` or just `'F6.2'`.
  This is an `intent(in)` and `optional` argument.  
  Contains the edit descriptor to format `value` into a string, for example `'(F6.2)'` or `'(f6.2)'`. 
  `to_string` will automatically enclose `format` in a set of parentheses, so passing `F6.2` or `f6.2` as `format` is possible as well.
  
#### Result value

The result is an `allocatable` length `character` scalar with up to `128` cached `character` length.

#### Example

```fortran
{!example/strings/example_to_string.f90!}
```
