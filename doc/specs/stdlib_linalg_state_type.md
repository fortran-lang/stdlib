---
title: linalg_state_type
---

# Linear Algebra -- State and Error Handling Module

[TOC]

## Introduction

The `stdlib_linalg_state` module provides a derived type holding information on the  
state of linear algebra operations, and procedures for expert control of linear algebra workflows. 
All linear algebra procedures are engineered to support returning an optional `linalg_state_type` 
variable to holds such information, as a form of expert API. If the user does not require state 
information, but fatal errors are encountered during the execution of linear algebra routines, the 
program will undergo a hard stop.
Instead, if the state argument is present, the program will never stop, but will return detailed error 
information into the state handler. 

## Derived types provided

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `linalg_state_type` derived type

The `linalg_state_type` is defined as a derived type containing an integer error flag, and 
fixed-size character strings to store an error message and the location of the error state change. 
Fixed-size string storage was chosen to facilitate the compiler's memory allocation and ultimately 
ensure maximum computational performance.  

A similarly named generic interface, `linalg_state_type`, is provided to allow the developer to 
create diagnostic messages and raise error flags easily. The call starts with an error flag or 
the location of the event, and is followed by an arbitrary list of `integer`, `real`, `complex` or 
`character` variables. Numeric variables may be provided as either scalars or rank-1 (array) inputs. 

#### Type-bound procedures

The following convenience type-bound procedures are provided: 
- `print()` returns an allocatable character string containing state location, message, and error flag; 
- `print_message()` returns an allocatable character string containing the state message; 
- `ok()` returns a `logical` flag that is `.true.` in case of successful state (`flag==LINALG_SUCCESS`);
- `error()` returns a `logical` flag that is `.true.` in case of error state (`flag/=LINALG_SUCCESS`).

#### Status

Experimental

#### Example

```fortran
{!example/linalg/example_state1.f90!}
```

## Error flags provided

The module provides the following state flags: 
- `LINALG_SUCCESS`: Successful execution
- `LINALG_VALUE_ERROR`: Numerical errors (such as infinity, not-a-number, range bounds) are encountered.
- `LINALG_ERROR`: Linear Algebra errors are encountered, such as: non-converging iterations, impossible operations, etc.
- `LINALG_INTERNAL_ERROR`: Provided as a developer safeguard for internal errors that should never occur.

## Comparison operators provided

The module provides overloaded comparison operators for all comparisons of a `linalg_state_type` variable 
with an integer error flag: `<`, `<=`, `==`, `>=`, `>`, `/=`.
