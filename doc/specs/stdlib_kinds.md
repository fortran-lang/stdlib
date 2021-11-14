---
title: kinds
---

# The `stdlib_kinds` module

[TOC]

## Introduction

The `stdlib_kinds` module provides kind parameters for the Fortran intrinsic data types,
*integer*, *logical*, *real*, and *complex*.


## Constants provided by `stdlib_kinds`

### `sp`

Single precision real kind parameter.
Provides real kind parameter for floating point numbers with a minimal precision of 6 significant digits.


### `dp`

Double precision real kind parameter.
Provides real kind parameter for floating point numbers with a minimal precision of 15 significant digits.


### `xdp`

Extended double precision real kind parameter.
Provides real kind parameter for floating point numbers with a minimal precision of 18 significant digits.
If not available it has value `-1`.


### `qp`

Quadruple precision real kind parameter.
Provides real kind parameter for floating point numbers with a minimal precision of 33 significant digits.
If not available it has value `-1`.


### `int8`

Reexported intrinsic named constant `int8` from `iso_fortran_env`.


### `int16`

Reexported intrinsic named constant `int16` from `iso_fortran_env`.


### `int32`

Reexported intrinsic named constant `int32` from `iso_fortran_env`.


### `int64`

Reexported intrinsic named constant `int64` from `iso_fortran_env`.


### `lk`

Kind parameter of the default logical data type.


### `c_bool`

Reexported intrinsic named constant `c_bool` from `iso_c_binding`.
