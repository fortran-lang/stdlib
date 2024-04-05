---
title: optval
---

# Default values for optional arguments

[TOC]

## `optval` - fallback value for optional arguments

### Status

Experimental

### Description

Returns `x` if it is present, otherwise `default`. 

This function is intended to be called in a procedure with one or more `optional` arguments, in order to conveniently fall back to a default value if an `optional` argument is not present.

### Syntax

`result = ` [[stdlib_optval(module):optval(interface)]] `(x, default)`

### Arguments

`x`: Shall be of type `integer`, `real`, `complex`, or `logical`, or a scalar of type `character`.

`default`: Shall have the same type, kind, and rank as `x`.

### Return value

If `x` is present, the result is `x`, otherwise the result is `default`.

### Example

```fortran
{!example/optval/example_optval.f90!}
```
