---
title: constants
---

[TOC]

## Introduction


The [[stdlib_constants]] module provides mathematical constants and the most common physical constants.


## Codata

The [[stdlib_codata(module)]] module defines all codata (physical) constants as derived 
type. The latest codata constants 
were released in 2018 by the [NIST](http://physics.nist.gov/constants)
All values for the codata constants are provided as double precision reals. 
The names are quite long and can be aliased with shorter names.

The derived type [[stdlib_codata_type(module):codata_constant_type(type)]] defines:
procedures.

* 4 members:

    * `name` (string)
    * `value` (double precision real)
    * `uncertainty` (double precision real)
    * `unit` (string)

* 2 type-bound procedures:

    * `print` for print the values of the constant members.
    * `to_real` for converting the value or the uncertainty to the desired precision.

A module level interface [[stdlib_codata_type(module):to_real(interface)]] is available for converting the constant value
or uncertainty.


## Example

```fortran
{!example/constants/example_constants.f90!}
```
