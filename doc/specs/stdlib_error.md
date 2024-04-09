---
title: error
---

# The `stdlib_error` module

[TOC]

## Introduction

Catching and handling errors.

## Procedures and methods provided


### `check` - Checks the value of a logical condition

#### Status

Experimental

#### Description

Checks the value of a logical condition.

#### Syntax

`call ` [[check(subroutine)]] `(condition, msg, code, warn)`


#### Arguments

`condition`: Shall be a scalar of type `logical`.

`msg` (optional): Shall be a character expression containing the message to be printed to `stderr`. The default `msg` is 'Check failed.'.

`code` (optional): Shall be a scalar of type `integer`. The default `code` is `1`.

`warn` (optional): Shall be a scalar of type `logical`. The default `warn` is `.true.`.

#### Return value

If `condition` is `.false.`, and:

 * no other arguments are provided, this subroutine stops the program with the default message and exit code 1;

 * `msg` is provided, this subroutine stops the program and it prints the value of `msg`;

 * `code` is provided, this subroutine stops the program with the given exit code;

 * `warn` is provided and `warn` is `.true.`, this subroutine doesn't stop the program and prints the message.

#### Examples

```fortran
{!example/error/example_check1.f90!}
```
```fortran
{!example/error/example_check2.f90!}
```
```fortran
{!example/error/example_check3.f90!}
```
```fortran
{!example/error/example_check4.f90!}
```

### `error_stop` - aborts the program

#### Status

Experimental

#### Description

Aborts the program with a message and a nonzero exit code.

#### Syntax

`call ` [[stdlib_error(module):error_stop(interface)]] `(msg, code)`

#### Arguments

`msg`: Shall be a character expression containing the message to be printed to `stderr`.

`code` (optional): Shall be a scalar of type `integer` to be returned as exit code.

#### Output

Aborts the program with printing the message `msg` to `stderr` and a nonzero exit code. The nonzero exit code is equal to `code` if provided, and 1 otherwise.

#### Examples

Without error code:

```fortran
{!example/error/example_error_stop1.f90!}
```

With error code:

```fortran
{!example/error/example_error_stop2.f90!}
```
