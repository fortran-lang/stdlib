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

#### Syntax

`call [[trueloc(function)]] (array[, lbound])`

#### Arguments

`array`: List of default logical arrays. This argument is `intent(in)`.

`lbound`: Lower bound of the array to index. This argument is `optional` and `intent(in)`.

#### Return value

Returns an array of default integer size, with a maximum length of `size(array)` elements.

#### Examples

```fortran
program demo
  use stdlib_array, only : trueloc
  implicit none
  real, allocatable :: array(:)
  allocate(array(500))
  call random_number(array)
  array(trueloc(array > 0.5)) = 0.0
end program demo
```


### `falseloc`

#### Status

Experimental

#### Description

Turn a logical mask into an index array by selecting all false values.

#### Syntax

`call [[falseloc(function)]] (array[, lbound])`

#### Arguments

`array`: List of default logical arrays. This argument is `intent(in)`.

`lbound`: Lower bound of the array to index. This argument is `optional` and `intent(in)`.

#### Return value

Returns an array of default integer size, with a maximum length of `size(array)` elements.

#### Examples

```fortran
program demo
  use stdlib_array, only : falseloc
  implicit none
  real, allocatable :: array(:)
  allocate(array(-200:200))
  call random_number(array)
  array(falseloc(array < 0.5), lbound(array)) = 0.0
end program demo
```
