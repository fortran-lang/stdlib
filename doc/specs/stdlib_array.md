# `stdlib_array`

```{contents}
:local:
:depth: 2
```

## Introduction

The `stdlib_array` module provides two functions that return the indices of
true or false elements in a logical mask.

Use these functions when the indices are needed. For other operations on a
logical mask, use `where` for masked assignment, `pack` to collect selected
values, or `merge` to choose between two values at each position.

See the [API reference](../api/stdlib_array.rst) for the arguments and return values.

## `trueloc`

The following example sets elements greater than `0.5` to zero.
The lower bound of `array` is 1, the default for `trueloc`.

```{literalinclude} ../../example/array/example_trueloc.f90
:language: fortran
```

## `falseloc`

The following example sets elements greater than or equal to `0.5` to zero.
The lower bound of `array` is `-200`. Passing `lbound(array, 1)` gives
indices relative to that lower bound. If the argument is omitted, the first
element of the mask has index 1.

```{literalinclude} ../../example/array/example_falseloc.f90
:language: fortran
```
