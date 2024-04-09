---
title: terminal colors
...


# The `stdlib_ansi` module

[TOC]

## Introduction

Support terminal escape sequences to produce styled and colored terminal output.


## Derived types provided


### ``ansi_code`` type

The ``ansi_code`` type represent an ANSI escape sequence with a style, foreground
color and background color attribute. By default the instances of this type are
empty and represent no escape sequence.

#### Status

Experimental

#### Example

```fortran
program demo_color
  use stdlib_ansi, only : fg_color_blue, style_bold, style_reset, ansi_code, &
    & operator(//), operator(+)
  implicit none
  type(ansi_code) :: highlight, reset

  print '(a)', highlight // "Dull text message" // reset

  highlight = fg_color_blue + style_bold
  reset = style_reset

  print '(a)', highlight // "Colorful text message" // reset
end program demo_color
```


## Constants provided

### ``style_reset``

Style enumerator representing a reset escape code.


### ``style_bold``

Style enumerator representing a bold escape code.


### ``style_dim``

Style enumerator representing a dim escape code.


### ``style_italic``

Style enumerator representing an italic escape code.


### ``style_underline``

Style enumerator representing an underline escape code.


### ``style_blink``

Style enumerator representing a blink escape code.


### ``style_blink_fast``

Style enumerator representing a (fast) blink escape code.


### ``style_reverse``

Style enumerator representing a reverse escape code.


### ``style_hidden``

Style enumerator representing a hidden escape code.


### ``style_strikethrough``

Style enumerator representing a strike-through escape code.


### ``fg_color_black``

Foreground color enumerator representing a foreground black color escape code.


### ``fg_color_red``

Foreground color enumerator representing a foreground red color escape code.


### ``fg_color_green``

Foreground color enumerator representing a foreground green color escape code.


### ``fg_color_yellow``

Foreground color enumerator representing a foreground yellow color escape code.


### ``fg_color_blue``

Foreground color enumerator representing a foreground blue color escape code.


### ``fg_color_magenta``

Foreground color enumerator representing a foreground magenta color escape code.


### ``fg_color_cyan``

Foreground color enumerator representing a foreground cyan color escape code.


### ``fg_color_white``

Foreground color enumerator representing a foreground white color escape code.


### ``fg_color_default``

Foreground color enumerator representing a foreground default color escape code.


### ``bg_color_black``

Background color enumerator representing a background black color escape code.


### ``bg_color_red``

Background color enumerator representing a background red color escape code.


### ``bg_color_green``

Background color enumerator representing a background green color escape code.


### ``bg_color_yellow``

Background color enumerator representing a background yellow color escape code.


### ``bg_color_blue``

Background color enumerator representing a background blue color escape code.


### ``bg_color_magenta``

Background color enumerator representing a background magenta color escape code.


### ``bg_color_cyan``

Background color enumerator representing a background cyan color escape code.


### ``bg_color_white``

Background color enumerator representing a background white color escape code.


### ``bg_color_default``

Background color enumerator representing a background default color escape code.


## Procedures and methods provided

### ``to_string``

Generic interface to turn a style, foreground or background enumerator into an actual escape code string for printout.

#### Syntax

`string =` [[stdlib_ansi(module):to_string(interface)]] `(code)`

#### Class

Pure function.

#### Argument

``code``: Style, foreground or background code of ``ansi_code`` type,
          this argument is ``intent(in)``.

#### Result value

The result is a default character string.

#### Status

Experimental

#### Example

```fortran
program demo_string
  use stdlib_ansi, only : fg_color_green, style_reset, to_string
  implicit none

  print '(a)', to_string(fg_color_green) // "Colorized text message" // to_string(style_reset)
end program demo_string
```


### ``operator(+)``

Add two escape sequences, attributes in the right value override the left value ones.

#### Syntax

`code = lval + rval`

#### Class

Pure function.

#### Argument

``lval``: Style, foreground or background code of ``ansi_code`` type,
          this argument is ``intent(in)``.
``rval``: Style, foreground or background code of ``ansi_code`` type,
          this argument is ``intent(in)``.

#### Result value

The result is a style, foreground or background code of ``ansi_code`` type.

#### Status

Experimental

#### Example

```fortran
program demo_combine
  use stdlib_ansi, only : fg_color_red, style_bold, ansi_code
  implicit none
  type(ansi_code) :: bold_red

  bold_red = fg_color_red + style_bold
end program demo_combine
```


### ``operator(//)``

Concatenate an escape code with a string and turn it into an actual escape sequence

#### Syntax

`str = lval // rval`

#### Class

Pure function.

#### Argument

``lval``: Style, foreground or background code of ``ansi_code`` type or a character string,
          this argument is ``intent(in)``.
``rval``: Style, foreground or background code of ``ansi_code`` type or a character string,
          this argument is ``intent(in)``.

#### Result value

The result is a character string with the escape sequence prepended or appended.

#### Status

Experimental

#### Example

```fortran
program demo_concat
  use stdlib_ansi, only : fg_color_red, style_reset, operator(//)
  implicit none

  print '(a)', fg_color_red // "Colorized text message" // style_reset
end program demo_concat
```
