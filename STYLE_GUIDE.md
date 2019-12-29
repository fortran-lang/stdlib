# Fortran stdlib Style Guide

> [A]utomate and codify as much as you possibly can while remembering that the human touch is still necessary to praise
> and be kind to your contributors.
> Let robots handle your project’s pedantry and humans handle your project’s empathy.

-- @mikemcquaid, [Homebrew] project leader<sup>[1]</sup>

[1]: https://mikemcquaid.com/2018/06/05/robot-pedantry-human-empathy/
[Homebrew]: https://brew.sh

## File naming conventions

* Source files should contain at most one `program`, `module`, or `submodule`.
* The filename should match the program or module name and have the file extension `.f90` or `.F90` if preprocessing is required.
* If the interface and implementation is split using submodules the implementation submodule file should have the same name as the
  interface (parent) module but end in `_implementation`.
  E.g., `string_class.f90` and `string_class_implementation.f90`
* Tests should be added in the `tests` subdirectory and have the same name as the module they are testing with the `test_` prefix
  added.
  E.g., `string_class.f90` and `tests/test_string_class.f90`
* There should only ever be one `program` and `module` statement per file.

## Indentation & whitespace

By setting and following a convention for indentation and whitespace, code reviews and git-diffs can
focus on the semantics of the proposed changes rather than style and formatting.

* The body of every Fortran construct should be indented by __4 spaces__
* Line length *should be limited to 80 characters* and __must not exceed 132__.
* Please do not use <kbd>Tab</kbd> characters for indentation
* Please remove trailing white space before committing code

## Variable and procedure naming

Variable names should be descriptive, and not artificially short.
By default, where it makes sense to do so, variable names shall be made up of one or more full words separated by an underscore.
For cases where a conventional & appropriate shortening of a word is used then the underscore may be omitted.

Examples:

__GOOD__:

``` fortran
logical :: has_failed
real function linspace(...)
```

__BAD__:

``` fortran
logical :: has_failed
real function lin_space(...)
```

## Keyword case

Fortran keywords should __*not*__ be UPPERCASE.
Modern editors and IDEs can highlight Fortran syntax and UPPERCASE keywords.
UPPERCASE keywords give Fortran source code the appearance of being antiquated.

## End <scope> block closing statements

Fortran allows certain block constructs or scopes to include the name of the program unit in the end statement.
The convention adopted herein is to __NOT__ include procedure names, `module` names and `program` names in the `end` statement.
There should only ever be one `program` and `module` statement per file and therefore including the name provides no useful information.
An exception to this rule is for procedures (`function`s and `subroutine`s) that are longer than a page of text.
Long procedures often are a sign of poor abstraction, however, sometimes they are necessary.
In such cases, the procedure name may be included in the `end` procedure statement.
