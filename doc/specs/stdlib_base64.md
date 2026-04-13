---
title: base64
---

# The `stdlib_base64` module

[TOC]

## Introduction

The `stdlib_base64` module provides procedures to encode and decode intrinsic
Fortran data using the Base64 encoding scheme defined by RFC 4648.

## Specification of the `stdlib_base64` procedures

### `base64_encode`

#### Status

Experimental

#### Description

Encodes intrinsic arrays into Base64 text.

This is the ergonomic API for encoding. It allocates and returns the output
string automatically.

#### Syntax

`res =` [[stdlib_base64(module):base64_encode(interface)]] `(data)`

#### Class

Function.

#### Argument

`data`: shall be a contiguous array of an intrinsic type (`real`, `integer`,
`complex`, `logical`, or `integer(int8)`). It is an `intent(in)` argument.

#### Result value

The result `res` is an allocatable character string (`character(len=:)`)
containing the Base64 representation of the input `data`.

#### Example

```fortran
character(len=:), allocatable :: encoded
integer(int8) :: bytes(3) = [77_int8, 97_int8, 110_int8]

encoded = base64_encode(bytes)  ! "TWFu"
```

### `base64_encode_into`

#### Status

Experimental

#### Description

Encodes bytes into a caller-provided output buffer.

This is the preallocated API for throughput-sensitive workflows. It does not
allocate and reports status through `err_state`. On success,
`err_state%ok()` is `.true.` and `encoded_len` is the number of meaningful
characters written to `str`.

#### Syntax

`call` [[stdlib_base64(module):base64_encode_into(subroutine)]] &
`(bytes, str, encoded_len, err_state)`

#### Class

Pure module subroutine.

#### Arguments

`bytes`: shall be a contiguous array of type `integer(int8)`. It is an
`intent(in)` argument.

`str`: shall be an intrinsic character type. It is an `intent(out)` argument.

`encoded_len`: shall be an `integer`. It is an `intent(out)` argument
representing the number of encoded characters written.

`err_state`: shall be a `type(state_type)` from `stdlib_error`. It is an
`intent(out)` argument used to report success or errors.

### `base64_decode`

#### Status

Experimental

#### Description

Decodes Base64 text and returns an allocated byte string.

This is the ergonomic API for decoding. It allocates and returns the result
automatically. On error, an empty result is returned. If `err_state` is
present, error details are reported there.

#### Syntax

`res =` [[stdlib_base64(module):base64_decode(function)]] `(str [, err_state])`

#### Class

Function.

#### Arguments

`str`: shall be an intrinsic character type. It is an `intent(in)` argument.

`err_state` (optional): shall be a `type(state_type)` from `stdlib_error`.
It is an `intent(out)` argument used to report invalid Base64 sequences.

#### Result value

The result `res` is an allocatable character string (`character(len=:)`)
containing the decoded bytes.

#### Example

```fortran
character(len=:), allocatable :: decoded

decoded = base64_decode("TWFu")  ! "Man"
```

### `base64_decode_into`

#### Status

Experimental

#### Description

Decodes Base64 text into a caller-provided output buffer.

This is the preallocated API for throughput-sensitive workflows. It does not
allocate and reports status through `err_state`. The optional `skip_despace`
argument can be used when the input is already whitespace-free to bypass the
despacing step.

#### Syntax

`call` [[stdlib_base64(module):base64_decode_into(subroutine)]] &
`(str, res, decoded_len, err_state [, skip_despace])`

#### Class

Pure module subroutine.

#### Arguments

`str`: shall be an intrinsic character type. It is an `intent(in)` argument.

`res`: shall be an intrinsic character type. It is an `intent(out)` argument.

`decoded_len`: shall be an `integer`. It is an `intent(out)` argument
representing the number of decoded bytes written.

`err_state`: shall be a `type(state_type)` from `stdlib_error`. It is an
`intent(out)` argument used to report success or errors.

`skip_despace` (optional): shall be a `logical`. It is an `intent(in)`
argument. If `.true.`, the routine assumes the input contains no whitespace.
