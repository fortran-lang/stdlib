---
title: Map data structure
...

# The `stdlib_map` module

[TOC]


## Introduction


## Reexported modules

This module reexport the `map_class` derived type from [[stdlib_map_class]] for convenience.


## Available procedures


### `new_map` constructor for map data types

#### Status

Experimental

#### Description

Create a new map using one of the available implementations:

- [[stdlib_map_cuckoohash(module):cuckoo_hash(type)]]: unordered map using cuckoo hashing

#### Syntax

`res = [[stdlib_map(module):new_map(subroutine)]] (map)`

#### Class

Pure subroutine.

#### Argument

`map`: shall be an allocated `map_class` type. It is an `intent(out)` argument.

#### Example

```fortran
program demo_map
    use stdlib_map, only : new_map, map_class, len
    implicit none
    type(map_class) :: map

    call new_map(map)
    print *, len(map)  ! return 0
end program demo_map
```
