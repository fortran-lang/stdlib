# Fortran stdlib Spec Template

* A specification document should describe exactly one module.
* Module name has a level-1 header.
* Derived type, procedure, and variables sections have level-2 headers.
* The Table of Contents (`<toc>`) lists only level-2 headers.
* Derived types (if any) are listed first, procedures (if any) second, and variables (if any) are listed last.
* Each category of items are listed in alphabetical order.
* Variables can include both constants (`parameter` variables) and non-constants.
* The "Result" section is only included for functions.
* Each derived type, type-bound method, or procedure section contains at least one example.
* If only one example is provided, it's placed in the level-2 section "Example".
* If multiple examples are provided, each is placed in a level-3 section "Example <number>", all inside level-2 section "Examples".
* An [Example](SPEC_EXAMPLE.md) ís provided.
* A [Template](SPEC_TEMPLATE.md) is provided.