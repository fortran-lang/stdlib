# Fortran stdlib Style Guide

Adopting a consistent style can improve code legibility through the choice of good naming conventions.
In addition, style checks will be run during CI to flag any severe non-conformance.
This allows code review discussions to focus on semantics and substance rather than pedantry.
Consistent whitespace usage, and not polluting line endings with trailing white space makes `git diff`s considerably more legible.
This style guide is a living document and proposed changes may be adopted after discussing them and coming to a consensus.

## Use (modern) standard Fortran

* Do not use obsolescent or deleted language features
  E.g., `common`, `pause`, `entry`, arithmetic `if` and computed `goto`
* Do not use vendor extensions in the form of non-standard syntax and vendor supplied intrinsic procedures
  E.g., `real*8` or `etime()`

## File naming conventions

* Source files should contain at most one `program`, `module`, or `submodule`
* The filename should match the program or module name and have the file extension `.f90` or `.F90` if preprocessing is required
* If the interface and implementation is split using submodules the implementation submodule file should have the same name as the
  interface (parent) module but end in `_implementation`
  E.g., `string_class.f90` and `string_class_implementation.f90`
* Tests should be added in the `test` subdirectory and have the same name as the module they are testing with the `test_` prefix
  added
  E.g., `string_class.f90` and `test/test_string_class.f90`

## Indentation & whitespace

By setting and following a convention for indentation and whitespace, code reviews and git-diffs can
focus on the semantics of the proposed changes rather than style and formatting.

* The body of every Fortran construct should be indented by __four (4) spaces__
* Line length *should be limited to 80 characters* and __must not exceed 132__
* Please do not use <kbd>Tab</kbd> characters for indentation
* Please remove trailing white space before committing code

## Variable and procedure naming

* Variable and procedure names, as well as Fortran keywords, should be written in lowercase
* Variable and procedure names should be made up of one or more full words separated by an underscore,
  for example `has_failed` is preferred over `hasfailed`
* Where conventional and appropriate shortening of a word is used then the underscore may be omitted,
  for example `linspace` is preferred over `lin_space`

## Attributes

<!-- ATTENTION! This section includes intentional trailing whitespace to get decent formatting with GFM and Python Markdown. -->

* Always specify `intent` for dummy arguments.
* Don't use `dimension` attribute to declare arrays because it is more verbose.
  Use this:  
  ```
  real, allocatable :: a(:), b(:,:)
  ```  
  instead of:  
  ```
  real, dimension(:), allocatable :: a
  ```  
  ```
  real, dimension(:,:), allocatable :: b
  ```  
  When defining many arrays of the same dimension, `dimension` can be used as an exception if it makes the code less verbose.
* If the `optional` attribute is used to declare a dummy argument, it should follow the `intent` attribute.
* For module procedures, it is recommended to declare attributes before the module keyword for better retro compatibility (Projects using CMake versions lower than CMake 3.25.0 are concerned see [Spurious modules](https://gitlab.kitware.com/cmake/cmake/-/issues/18427#note_983426)).
Prefer the following pattern:
```
  <attribute> <attribute> module <function/subroutine> <name>
  ```
  instead of:
  ```
  module <attribute> <attribute> <function/subroutine> <name>
  ```
## End <scope> block closing statements

Fortran allows certain block constructs or scopes to include the name of the program unit in the end statement.
The convention adopted herein is to include procedure names, `module` names and `program` names in the `end` statement,
unless the closing statement can reasonably be expected to be on the same screen or page, within about 25 lines.

## Document public API code with FORD

Documentation strings should be provided for all public and protected entities and their arguments or parameters.
This is currently accomplished using the [FORD tool](https://github.com/Fortran-FOSS-Programmers/ford).
For help writing FORD style documentation please see the [FORD wiki](https://github.com/Fortran-FOSS-Programmers/ford/wiki).
The following two sections are most relevant for contributing new code:

* [Writing Documentation](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation)
* [Documentation Meta Data](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Documentation-Meta-Data)
* [Limitations](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Limitations)

To write the "spec" (specification) for a new proposal, please place it in the
[FORD "pages"](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Pages) directory at
[`doc/specs/`](https://github.com/fortran-lang/stdlib/tree/HEAD/doc/specs).
To get help please see the ["Writing Pages"](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Pages)
and ["Writing Documentation"](https://github.com/Fortran-FOSS-Programmers/ford/wiki/Writing-Documentation) pages
on the [FORD wiki](https://github.com/Fortran-FOSS-Programmers/ford/wiki).
