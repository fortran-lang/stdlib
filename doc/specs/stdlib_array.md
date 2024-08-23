---
title: array
---

# The `stdlib_array` module

[TOC]

## Introduction

Module for index manipulation and array handling tasks.

## Derived types provided

### `array_wrapper_type`

A derived type that wraps a polymorphic `array_type` and helps with its allocation. By loading an npz file with `load_npz`, a list of array wrappers will be obtained. On the other hand, a list of array wrappers can be saved to an npz file using `save_npz`. Use `add_array` to add an array to a list of array wrappers and call `get_values` on the array wrapper to obtain the values of the underlying array.

#### Status

Experimental

#### Example

```fortran
program npz_example
    use stdlib_array, only: array_wrapper_type, add_array
    use stdlib_io_np, only: save_npz, load_npz
    implicit none

    type(array_wrapper_type), allocatable :: input_arrays(:), output_arrays(:)
    real :: x(3, 2) = 1
    integer :: y(2, 3) = 2
    real, allocatable :: x_out(:,:)
    integer, allocatable :: y_out(:,:)

    call add_array(input_arrays, x)
    call add_array(input_arrays, y)

    call save_npz('example_save.npz', input_arrays)

    call load_npz('example_save.npz', output_arrays)

    if (size(input_arrays) /= 2) then
        print *, 'Error: Output array has unexpected size.'; stop
    end if

    call output_arrays(1)%get_values(x_out)
    call output_arrays(2)%get_values(y_out)

    print *, x_out
    print *, y_out
end
```

### `array_type`

An abstract type that can be extended according to the type and rank of the stored array. It is usually not necessary to interact with this type directly. It is used to store multiple arrays of different types and ranks in a single array.

#### Status

Experimental

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

### `add_array`

#### Status

Experimental

#### Description

Add an array of defined type and rank to a list of array wrappers.

#### Syntax

`call ` [[stdlib_array(module):add_array(interface)]] ` (arrays, array[, stat, msg, name])`

#### Class

Pure subroutine.

#### Arguments

`arrays`: List of array wrappers of type `array_wrapper_type` to add `array` to. This argument is `intent(inout)`.

`array`: Array with defined type and rank to be added to the list of array wrappers. This argument is `intent(in)`.

`stat`: Status variable of type `integer`. This argument is `optional` and `intent(out)`. The operation is successful if `stat` is `0`.

`msg`: Error message. This argument is `optional` and `intent(out)`.

`name`: Name of the array. This argument is `optional` and `intent(in)`. If not provided, the name will be set to the default value.

#### Examples

```fortran
{!example/io/example_save_npz.f90!}
```
### `get_values`

#### Status

Experimental

#### Description

Get the values of the array within the array wrapper.

#### Syntax

`call ` [[stdlib_array(module):array_wrapper_type(type)]] `%` [[array_wrapper_type(type):get_values(bound)]] ` (wrapper, values[, stat, msg])`

#### Class

Pure subroutine.

#### Arguments

`wrapper`: Array wrapper of type `array_wrapper_type` to get the values from. This argument is `intent(in)`.

`values`: Array of the same type and rank as the array within the array wrapper. This argument is `intent(out)`.

`stat`: Status variable of type `integer`. This argument is `optional` and `intent(out)`. The operation is successful if `stat` is `0`.

`msg`: Error message. This argument is `optional` and `intent(out)`.

#### Examples

```fortran
{!example/io/example_load_npz.f90!}
```
