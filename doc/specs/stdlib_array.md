---
title: array
---

# The `stdlib_array` module

[TOC]

## Introduction

Module for index manipulation and array handling tasks.

## Procedures and methods provided


### `trueloc`

#### Status

Experimental

#### Description

Turn a logical mask into an index array by selecting all true values.
Provides similar functionality like the built-in `where` or the intrinsic procedures `merge` and `pack` when working with logical mask.
The built-in / intrinsics are usually preferable to `trueloc`, unless the access to the index array is required.

#### Syntax

`loc =` [[trueloc(function)]] `(array[, lbound])`

#### Class

Pure function.

#### Arguments

`array`: List of default logical arrays. This argument is `intent(in)`.

`lbound`: Lower bound of the array to index. This argument is `optional` and `intent(in)`.

#### Return value

Returns an array of default integer size, with a maximum length of `size(array)` elements.

#### Examples

```fortran
{!example/array/example_trueloc.f90!}
```


### `falseloc`

#### Status

Experimental

#### Description

Turn a logical mask into an index array by selecting all false values.
Provides similar functionality like the built-in `where` or the intrinsic procedures `merge` and `pack` when working with logical mask.
The built-in / intrinsics are usually preferable to `falseloc`, unless the access to the index array is required.

#### Syntax

`loc =` [[falseloc(function)]] `(array[, lbound])`

#### Class

Pure function.

#### Arguments

`array`: List of default logical arrays. This argument is `intent(in)`.

`lbound`: Lower bound of the array to index. This argument is `optional` and `intent(in)`.

#### Return value

Returns an array of default integer size, with a maximum length of `size(array)` elements.

#### Examples

```fortran
{!example/array/example_falseloc.f90!}
```
