---
title: state_type
---

# State and Error Handling Derived Type

[TOC]

## Introduction

The `stdlib_error` module provides a derived type holding information on the state of operations within the standard library and procedures for expert control of workflows.
An optional `state_type` variable to hold such information is provided as a form of expert API.
If the user does not require state information but fatal errors are encountered during execution, the program will undergo a hard stop.
Instead, if the state argument is present, the program will never stop but will return detailed error information into the state handler.

## Derived types provided

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `state_type` derived type

The `state_type` is defined as a derived type containing an integer error flag and fixed-size character strings to store an error message and the location of the error state change.
Fixed-size string storage was chosen to facilitate the compiler's memory allocation and ultimately ensure maximum computational performance.  

A similarly named generic interface, `state_type`, is provided to allow the developer to create diagnostic messages and raise error flags easily. 
The call starts with an error flag or the location of the event and is followed by an arbitrary list of `integer`, `real`, `complex`, or `character` variables. 
Numeric variables may be provided as either scalars or rank-1 (array) inputs.

#### Type-bound procedures

The following convenience type-bound procedures are provided:
- `print()` returns an allocatable character string containing state location, message, and error flag;
- `print_message()` returns an allocatable character string containing the state message;
- `ok()` returns a `logical` flag that is `.true.` in case of successful state (`flag==STDLIB_SUCCESS`);
- `error()` returns a `logical` flag that is `.true.` in case of an error state (`flag/=STDLIB_SUCCESS`).

#### Status

Experimental

#### Example

```fortran
{!example/error/example_error_state1.f90!}
```

## Error flags provided

The module provides the following state flags:
- `STDLIB_SUCCESS`: Successful execution
- `STDLIB_VALUE_ERROR`: Numerical errors (such as infinity, not-a-number, range bounds) are encountered.
- `STDLIB_LINALG_ERROR`: Linear Algebra errors are encountered, such as non-converging iterations, impossible operations, etc.
- `STDLIB_INTERNAL_ERROR`: Provided as a developer safeguard for internal errors that should never occur.
- `STDLIB_IO_ERROR`: Input/Output-related errors, such as file reading/writing failures.
- `STDLIB_FS_ERROR`: File system-related errors, such as directory access issues.

## Comparison operators provided

The module provides overloaded comparison operators for all comparisons of a `state_type` variable with an integer error flag: `<`, `<=`, `==`, `>=`, `>`, `/=`.

