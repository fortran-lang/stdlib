---
title: version
---

# The `stdlib_version` module

[TOC]

## Introduction

The `stdlib_version` module contains the version of the standard library.
The version information can be used as a compile time constant or retrieved from a getter function at runtime.
In case the standard library is dynamically linked, the version number retrieved from the getter might mismatch the compile time constants provided from the version built against.
Therefore, it is recommended to retrieve the version information always at runtime.


## Constants provided by `stdlib_version`

### `stdlib_version_string`

String constant representing the version number.

### `stdlib_version_compact`

Compact representation of the version string following the scheme:
major * 10000 + minor * 100 + patch.


### `get_stdlib_version`

#### Status

Experimental

#### Description

Getter function to retrieve version information

#### Syntax

`res = ` [[stdlib_version(module):get_stdlib_version(subroutine)]] ` ([major], [minor], [patch], [string])`

#### Class

Pure subroutine.

#### Argument

`major`: shall be an intrinsic integer type. It is an optional, `intent(out)` argument.
`minor`: shall be an intrinsic integer type. It is an optional, `intent(out)` argument.
`patch`: shall be an intrinsic integer type. It is an optional, `intent(out)` argument.
`string`: shall be a deferred length character type. It is an optional, `intent(out)` argument.

#### Example

```fortran
{!example/version/example_version.f90!}
``` 
