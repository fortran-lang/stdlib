---
title: regex
---

# Regular Expressions

[TOC]

## Overview

The `stdlib_regex` module provides a pure Fortran regular expression engine
based on Thompson's NFA (Nondeterministic Finite Automaton) construction.
It guarantees linear-time matching `O(n × m)` with no backtracking,
making it safe for use with arbitrary input without risk of catastrophic
performance degradation.

### Supported Syntax

| Pattern     | Description                          | Example          |
|-------------|--------------------------------------|------------------|
| `.`         | Match any single character           | `a.c` → `abc`   |
| `*`         | Zero or more of preceding element    | `ab*c` → `ac`   |
| `+`         | One or more of preceding element     | `ab+c` → `abbc` |
| `?`         | Zero or one of preceding element     | `colou?r`        |
| `\|`        | Alternation                          | `cat\|dog`       |
| `(` `)`     | Grouping                             | `(ab)+`          |
| `[...]`     | Character class                      | `[a-z]`          |
| `[^...]`    | Negated character class              | `[^0-9]`         |
| `^`         | Start of string anchor               | `^foo`           |
| `$`         | End of string anchor                 | `bar$`           |
| `\d`        | Digit `[0-9]`                        | `\d+`            |
| `\w`        | Word character `[a-zA-Z0-9_]`        | `\w+`            |
| `\s`        | Whitespace (space, tab, newline, CR) | `\s+`            |
| `\`         | Escape next character                | `\.`             |

## `regex_type` - Regular expression type

### Status

Experimental

### Description

A derived type representing a compiled regular expression. It stores the
internal NFA state graph produced by `regcomp` and is passed to `regmatch`
for pattern matching.

### Syntax

```fortran
type(regex_type) :: re
```

## `regcomp` - Compile a regular expression

### Status

Experimental

### Description

Compiles a regular expression pattern string into a `regex_type` object.
The compiled object can then be reused for multiple calls to `regmatch`
without recompilation.

### Syntax

```fortran
call [[stdlib_regex(module):regcomp(subroutine)]](re, pattern [, status])
```

### Class

Subroutine

### Arguments

`re`: Shall be of type `regex_type`. It is an `intent(out)` argument.
The compiled regular expression.

`pattern`: Shall be of type `character(len=*)`. It is an `intent(in)` argument.
The regular expression pattern string to compile.

`status` (optional): Shall be of type `integer`. It is an `intent(out)` argument.
Returns 0 on success, or a non-zero value if the pattern is invalid
(e.g., mismatched parentheses or brackets).

### Example

```fortran
{!example/regex/example_regex_regcomp.f90!}
```

## `regmatch` - Match a compiled regular expression

### Status

Experimental

### Description

Searches for the first occurrence of the compiled regular expression `re`
within the input `string`. If a match is found, `is_match` is set to `.true.`
and the optional `match_start` and `match_end` arguments are set to
the 1-based start and end positions of the matched substring.

### Syntax

```fortran
call [[stdlib_regex(module):regmatch(subroutine)]](re, string, is_match [, match_start, match_end])
```

### Class

Subroutine

### Arguments

`re`: Shall be of type `regex_type`. It is an `intent(in)` argument.
A compiled regular expression obtained from `regcomp`.

`string`: Shall be of type `character(len=*)`. It is an `intent(in)` argument.
The input string to search for a match.

`is_match`: Shall be of type `logical`. It is an `intent(out)` argument.
Set to `.true.` if a match is found, `.false.` otherwise.

`match_start` (optional): Shall be of type `integer`. It is an `intent(out)` argument.
The 1-based index of the first character of the match.

`match_end` (optional): Shall be of type `integer`. It is an `intent(out)` argument.
The 1-based index of the last character of the match.

### Example

```fortran
{!example/regex/example_regex_regmatch.f90!}
```
