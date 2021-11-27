---
title: terminal colors
...


# The `stdlib_terminal_colors` module

[TOC]

## Introduction

Support terminal escape sequences to produce styled and colored terminal output.


## Derived types provided


### ``fg_color24`` type

The ``fg_color24`` type represent a true color (24-bit) foreground color.
It contains the members ``red``, ``blue`` and ``green`` as default integer types.

#### Status

Experimental


### ``bg_color24`` type

The ``bg_color24`` type represent a true color (24-bit) background color.
It contains the members ``red``, ``blue`` and ``green`` as default integer types.

#### Status

Experimental


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

`string = [[stdlib_string_colors(module):to_string(interface)]] (enum)`

#### Class

Pure function.

#### Argument

``enum``: Style, foreground or background enumerator, this argument is ``intent(in)``.

#### Result value

The result is a default character string.

#### Status

Experimental


### ``to_string``

Generic interface to turn a foreground or background true color type into an actual escape code string for printout.

#### Syntax

`string = [[stdlib_string_colors(module):to_string(interface)]] (color24)`

#### Class

Pure function.

#### Argument

``color24``: Foreground or background true color instance, this argument is ``intent(in)``.

#### Result value

The result is a default character string.

#### Status

Experimental
