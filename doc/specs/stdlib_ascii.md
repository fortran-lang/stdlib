---
title: ASCII
---

# The `stdlib_ascii` module

[TOC]

## Introduction

The `stdlib_ascii` module provides procedures for handling and manipulating
intrinsic character variables and constants.


## Constants provided by `stdlib_ascii`

@note Specification of constants is currently incomplete.


## Specification of the `stdlib_ascii` procedures

@note Specification of procedures is currently incomplete.


### `to_lower`

#### Status

Experimental

#### Description

Converts input character variable to all lowercase.

#### Syntax

```f90
res = to_lower("HELLO!")
! res == "hello!"
```

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.


### `to_upper`

#### Status

Experimental

#### Description

Converts input character variable to all uppercase.

#### Syntax

```
res = to_upper("hello!")
! res == "HELLO!"
```

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.


### `to_title`

#### Status

Experimental

#### Description

Returns capitalized version of input character variable.
The first alphanumeric character is capitalized.

#### Syntax

```
res = to_title("hello!")
! res == "Hello!"
res = to_title("'enquoted'")
! res == "'Enquoted'"
res = to_title("1st")
! res == "1st"
```

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.


### `reverse`

#### Status

Experimental

#### Description

Reverses the order of all characters in the input character type.

#### Syntax

```f90
res = reverse("Hello, World!")
! res == "!dlroW ,olleH"
res = reverse(res)
! res == "Hello, World!"
```

#### Class

Pure function.

#### Argument

`string`: shall be an intrinsic character type. It is an `intent(in)` argument.

#### Result value

The result is an intrinsic character type of the same length as `string`.
