---
title: str2num
---

# The `stdlib_str2num` module

This module proposes a function-style interface for string-to-number conversion. It also profits from Fortran's interfaces to implement precision-dependant algorithms to maximize runtime efficiency.

[TOC]

## `to_num` - conversion of strings to numbers

### Status

Experimental

### Description

Convert a string or an array of strings to numerical types.

### Syntax

`number = ` [[stdlib_str2num(module):to_num(interface)]] `(string, mold)`

### Arguments

`string`: argument has `intent(in)` and is of type `character(*)`.

`mold`: argument has `intent(in)` and is of numerical type (that is of `integer` or of `real`). **Note**: The type of the `mold` argument defines the type of the result.

### Return value

Return a scalar of numerical type (i.e., `integer`, or `real`).

### Example

```fortran
{!example/strings/example_string_to_number.f90!}
```

## `to_num_from_stream` - conversion of a stream of values in a string to numbers

### Status

Experimental

### Description

Convert a stream of values in a string to an array of values.

### Syntax

`number = ` [[stdlib_str2num(module):to_num_from_stream(interface)]] `(string, mold)`

### Arguments

`string`: argument has `intent(in)` and is of type `character(:), pointer`.

`mold`: argument has `intent(in)` and is of numerical type (currently of `integer` or `real`). **Note**: The type of the `mold` argument defines the type of the result.

### Return value

Return a scalar of numerical type (i.e., `integer` or `real`).

### Example

```fortran
{!example/strings/example_stream_of_strings_to_numbers.f90!}
```

## Note
The accuracy of the conversion is implementation dependent; it is recommended that implementers guarantee precision down to the last 3 bits.

**The current implementation has been tested to provide for** :

`sp`  : exact match

`dp`  : precision up-to epsilon(0.0_dp)

`qp` : precision around 200*epsilon(0.0_qp)

Where precision refers to the relative difference between `to_num` and `read`. On the other hand, `to_num` provides speed-ups ranging from 4x to >10x compared to the intrinsic `read`.
