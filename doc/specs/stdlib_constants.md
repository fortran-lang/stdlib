---
title: constants
---

[TOC]

## Introduction


The [[stdlib_constants]] module provides mathematical constants and the most common physical constants.

**Warning**: The names of the most common physical constants are kept short as they are inside a dedicated module. 
Nonetheless, in case of overlapping names, they can always be renamed as following:

```fortran
use stdlib_constants, only: clight => c
```

## Codata

The [[stdlib_codata(module)]] module defines all codata (physical) constants as derived 
type. The module is automatically generated with a simple 
[parser written in Python](https://github.com/MilanSkocic/codata/)
The latest codata constants were released in 2022 by the [NIST](http://physics.nist.gov/constants)
All values for the codata constants are provided as double precision reals. 
The names are quite long and can be aliased with shorter names.

The derived type [[stdlib_codata_type(module):codata_constant_type(type)]] defines:

* 4 members:

    * `name` (string)
    * `value` (double precision real)
    * `uncertainty` (double precision real)
    * `unit` (string)

* 2 type-bound procedures:

    * `print`: to print the values of the constant members;
    * `to_real`: to get the value or the uncertainty to the desired precision.

A module level interface [[stdlib_codata_type(module):to_real(interface)]] is 
available for getting the constant value or uncertainty of a constant. 

## `to_real` - Get the constant value or its uncertainty.

### Status

Experimental

### Description

Convert a [[stdlib_codata_type(module):codata_constant_type(type)]] to a `real` (at least `sp`, or `dp`) scalar. 
**Warning**: Some constants cannot be converted to single precision `sp` reals due to the value of the exponents.

### Syntax

`r = ` [[stdlib_codata_type(module):to_real(interface)]] `(c, mold [, uncertainty])`

### Arguments

`c`: argument has `intent(in) ` and shall be of type [[stdlib_codata_type(module):codata_constant_type(type)]].

`mold`: argument has `intent(in)` and shall be of `real` type. 
**Note**: The type of the `mold` argument defines the type of the result.

`uncertainty` (optional): argument has `intent(in)` and shall be of `logical` type. 
It specifies if the uncertainty needs to be returned instead of the value. Default to `.false.`.

### Return value

Returns a scalar of `real` type which is either the value or the uncertainty of a codata constant.

## Example

```fortran
{!example/constants/example_constants.f90!}
```
