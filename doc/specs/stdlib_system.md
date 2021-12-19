---
title: system
---

# The `stdlib_system` module

[TOC]

## Introduction

Interface to system functions.

## Procedures and methods provided


### `sleep`

#### Status

Experimental

#### Description

Wait for a given period of time.

#### Syntax

`call [[sleep(subroutine)]] (time)`


#### Arguments

`time`: Time to wait in millisecs.


#### Examples

```fortran
program demo
    use stdlib_system, only: sleep
    implicit none
    call sleep(150)
end program demo
```


### `get_argument`

#### Status

Experimental

#### Description

Obtain the nth command line argument

#### Syntax

`call [[get_argument(interface)]] (idx, val[, stat])`

#### Arguments

`idx`: Index of the command line argument to retrieve.
       This is an `intent(in)` argument of default integer kind.

`val`: Value of command line argument on return.
       Can be either a deferred length character or a [[string_type]].
       This is an `intent(out)` argument.

`stat`: Default integer, contains status of operation, zero in case of success.
        It is an optional argument, in case not present the program will halt for non-zero status.
        This argument is `intent(out)`.


#### Examples

```fortran
program demo
    use stdlib_system, only: get_argument
    implicit none
    character(len=:), allocatable :: prog
    call get_argument(0, prog)
    print '(a)', prog
end program demo
```



### `get_variable`

#### Status

Experimental

#### Description

Obtain the value of an environment variable

#### Syntax

`call [[get_variable(interface)]] (var, val[, stat])`

#### Arguments

`val`: Name of the environment variable to read.
       Can be either a default character or a [[string_type]].
       This is an `intent(in)` argument.

`val`: Value of the environment variable on return.
       Can be either a deferred length character or a [[string_type]].
       This is an `intent(out)` argument.

`stat`: Default integer, contains status of operation, zero in case of success.
        It is an optional argument, in case not present the program will halt for non-zero status.
        This argument is `intent(out)`.


#### Examples

```fortran
program demo
    use stdlib_system, only: get_variable
    implicit none
    character(len=:), allocatable :: home
    call get_variable("HOME", home)
    print '(a)', home
end program demo
```



### `set_variable`

#### Status

Experimental

#### Description

Set the value of an environment variable

#### Syntax

`call [[set_variable(interface)]] (var, val[, stat])`

#### Arguments

`val`: Name of the environment variable to set.
       Can be either a default character or a [[string_type]].
       This is an `intent(in)` argument.

`val`: Value of the environment variable to set
       Can be either a deferred length character or a [[string_type]].
       This is an `intent(in)` argument.

`stat`: Default integer, contains status of operation, zero in case of success.
        It is an optional argument, in case not present the program will halt for non-zero status.
        This argument is `intent(out)`.


#### Examples

```fortran
program demo
    use stdlib_system, only: set_variable
    implicit none
    call set_variable("OMP_NUM_THREADS", "1")
end program demo
```
