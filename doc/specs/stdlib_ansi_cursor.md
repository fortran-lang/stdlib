---
title: ansi_cursor
---

# The `stdlib_ansi_cursor` module

[TOC]

## Introduction

Module for cursor control using ansi terminal escape sequences

## Constants provided by `stdlib_ansi_cursor`

### ``esc``

The ESC character


### ``home``

ansi escape code to move the cursor to it's home coordinates `(1,1)`


### ``clear_till_screen_start``

ansi escape code to clear the screen till the start of the terminal


### ``clear_till_screen_end``

ansi escape code to clear the screen till the end of the terminal


### ``clear_completetely``

ansi escape code to clear the terminal screen completely


### ``clear_till_line_end``

ansi escape code to clear till the current line end


### ``clear_till_line_start``

ansi escape code to clear till the current line start


### ``clear_entire_line``

ansi escape code to clear the entire line



## Procedures and methods provided


### `move_to`

#### Status

Experimental

#### Description

moves the cursor to the specified `line` and `column`

#### Syntax

`code =` [[stdlib_ansi_cursor(module):move_to(function)]] `(line, col)`

#### Class

Pure function.

#### Arguments

`line`: line (row) number to move it to 

`col`: col (column) number to move it to

#### Return value

a default character string

#### Examples

```fortran
program test
    use stdlib_ansi_cursor, only: move_to
    implicit none

    character(len=1) :: input

    print *, move_to(1, 1) ! Same as printing the constant `home`
    read (*,*), input  ! Waiting for input to actually see the effect of the `move_to` function
end program test
```

A more detailed example of drawing a blue box in a terminal

```fortran
{!example/terminal/example_ansi_cursor.f90!}
```


### `move_to_column`

#### Status

Experimental

#### Description

moves the cursor to the specified `column`

#### Syntax

`code =` [[stdlib_ansi_cursor(module):move_to_column(function)]] `(col)`

#### Class

Pure function.

#### Arguments

`col`: col (column) number to move it to

#### Return value

a default character string


### `move_up`

#### Status

Experimental

#### Description

moves the cursor up by `line` lines

#### Syntax

`code =` [[stdlib_ansi_cursor(module):move_up(function)]] `(line)`

#### Class

Pure function.

#### Arguments

`line`: number of lines to move it above by

#### Return value

a default character string


### `move_down`

#### Status

Experimental

#### Description

moves the cursor down by `line` lines

#### Syntax

`code =` [[stdlib_ansi_cursor(module):move_down(function)]] `(line)`

#### Class

Pure function.

#### Arguments

`line`: number of lines to move it below by

#### Return value

a default character string


### `move_left`

#### Status

Experimental

#### Description

moves the cursor to the left by `col` columns

#### Syntax

`code =` [[stdlib_ansi_cursor(module):move_left(function)]] `(col)`

#### Class

Pure function.

#### Arguments

`col`: number of columns to move the cursor to the left by

#### Return value

a default character string


### `move_right`

#### Status

Experimental

#### Description

moves the cursor to the right by `col` columns

#### Syntax

`code =` [[stdlib_ansi_cursor(module):move_right(function)]] `(col)`

#### Class

Pure function.

#### Arguments

`col`: number of columns to move the cursor to the right by

#### Return value

a default character string

