---
title: regex
---

# Regular Expressions

[TOC]

## `regex_type` - Regular expression type

### Status

Experimental

### Description

A type for representing compiled regular expressions.

### Syntax

```fortran
type(regex_type) :: re
```

## `regcomp` - Compile a regular expression

### Status

Experimental

### Description

Compiles a regular expression string into a `regex_type` object.

### Syntax

```fortran
call regcomp(re, pattern [, flags, status])
```

### Class

Subroutine

### Arguments

`re`: An `intent(out)` argument of type `regex_type`.
`pattern`: An `intent(in)` argument of type `character(len=*)`.
`flags` (optional): An `intent(in)` argument of type `integer`.
`status` (optional): An `intent(out)` argument of type `integer`.
