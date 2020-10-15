---
title: Bitsets
---

# The `stdlib_bitsets` module

[TOC]

## Introduction

The `stdlib_bitsets` module implements bitset types. A bitset is a
compact representation of a sequence of `bits` binary values. It can
equivalently be considered as a sequence of logical values or as a subset of
the integers 0 ... `bits-1`. The bits are indexed from 0 to
`bits(bitset)-1`. A bitset is used when space savings are critical in
applications that require a large number
of closely related logical values.
It may also improve performance by reducing memory traffic. To implement
bitsets the module
defines three bitset types, multiple constants, a character string
literal that can be read to and from strings and formatted files, a
simple character string literal that can be read to and from strings,
assignments, procedures, methods, and operators. Note that the module
assumes two's complement integers, but all current Fortran 95+ processors use such integers.


## The module's constants

The module defines several public integer constants, almost all
intended to serve as error codes in reporting problems through an
optional `stat` argument. One constant, `bits_kind` is
the integer kind value for indexing bits and reporting counts of
bits. The other constants that are error codes are summarized below:

|Error Code|Summary|
|----------|-------|
|`success`|No problems found|
|`alloc_fault`|Failure with a memory allocation|
|`array_size_invalid_error`|Attempt to define either negative bits or more than 64 bits in a `bitset_64`|
|`char_string_invalid_error`|Invalid character found in a character string|
|`char_string_too_large_error`|Character string was too large to be encoded in the bitset|
|`char_string_too_small_error`|Character string was too small to hold the expected number of bits|
|`index_invalid_error`|Index to a bitstring was less than zero or greater than the number of bits|
|`integer_overflow_error`|Attempt to define an integer value bigger than `huge(0_bits_kind`)|
|`read_failure`|Failure on a `read` statement|
|`eof_failure`|An unexpected "End-of-File" on a `read` statement|
|`write_failure`|Failure on a `write` statement|


## The `stdlib_bitsets` derived types

The `stdlib_bitsets` module defines three derived types,
`bitset_type`, `bitset_64`, and `bitset_large`. `bitset_type` is an abstract
type that serves as the ancestor of `bitset_64` and
`bitset_large`. `bitset_type` defines one method, `bits`, all of its
other methods are deferred to its extensions. `bitset_64` is a bitset
that can handle up to 64 bits. `bitset_large` is a bitset that can handle
up `huge(0_bits_kind)` bits. All attributes of the bitset types are
private. The various types each define a sequence of binary values: 0
or 1. In some cases it is useful to associate a logical value, `test`,
for each element of the sequence, where `test` is `.true.` if the value
is 1 and `.false.` otherwise. The number of such values in an entity
of that type is to be termed, `bits`. The bits are ordered in terms of
position, that, in turn, is indexed from 0 to `bits-1`. `bitset_type` is
used only as a `class` to define entities that can be either a `bitset_64` or
a `bitset_large`. The syntax for using the types are:

`class([[stdlib_bitset(module):bitset_type(class)]]) :: variable`

`type([[stdlib_bitset(module):bitset_64(type)]]) :: variable`

and

`type([[stdlib_bitset(module):bitset_large(type)]]) :: variable`

## The *bitset-literal*

A bitset value may be represented as a *bitset-literal-constant*
character string in source code or as a *bitset-literal* in
formatted files and non-constant strings.

*bitset-literal-constant* is ' *bitset-literal* '
                          or " *bitset-literal* "

*bitset-literal* is *bitsize-literal* *binary-literal*

*bitsize-literal* is S *digit* [ *digit* ] ...

*binary-literal* is B *binary-digit* [ *binary-digit* ] ...

*digit* is 0
        or 1
        or 2
        or 3
        or 4
        or 5
        or 6
        or 7
        or 8
        or 9


*binary-digit* is 0
               or 1

The *bitset-literal* consists of two parts: a *bitsize-literal* and a
*binary-literal*. The sequence of decimal digits that is part of the
*bitsize-literal* is interpreted as the decimal value of `bits`.
The *binary-literal* value is interpreted as a sequence of bit
values and there must be as many binary digits in the literal as there
are `bits`. The sequence of binary digits are treated as if they were
an unsigned integer with the i-th digit corresponding to the `bits-i`
bit position.

## The *binary-literal*

In defining the *bitset-literal* we also defined a
*binary-literal*. While not suitable for file I/0, the
*binary-literal* is suitable for transfer to and from character
strings. In that case the length of the string is the number of bits
and all characters in the string must be either "0" or "1".

## Summary of the module's operations

The `stdlib_bitsets` module defines a number of operations:
* "unary" methods of class `bitset_type`,
* "binary" procedure overloads of type `bitset_64` or `bitset_large`,
* assignments, and
* "binary" comparison operators of type `bitset_64` or `bitset_large`.

Each category will be discussed separately.

### Table of the `bitset_type` methods

The `bitset_type` class has a number of methods. All except one, `bits`,
are deferred. The methods consist of all procedures with one argument
of class `bitset_type`. The procedures with two arguments of type
`bitset_64` or `bitset_large` are not methods and are
summarized in a separate table of procedures. The methods are
summarized below:

|Method name|Class|Summary|
|-----------|-----|-------|
|`all`|function|`.true.` if all bits are 1, `.false.` otherwise|
|`any`|function|`.true.` if any bits are 1, `.false.` otherwise|
|`bit_count`|function|returns the number of bits that are 1|
|`bits`|function|returns the number of bits in the bitset|
|`clear`|subroutine|sets a sequence of one or more bits to 0|
|`flip`|subroutine|flips the value of a sequence of one or more bits|
|`from_string`|subroutine|reads the bitset from a string treating it as a binary literal|
|`init`|subroutine|creates a new bitset of size `bits` with no bits set|
|`input`|subroutine|reads a bitset from an unformatted I/O unit|
|`none`|function|`.true.` if no bits are 1, `.false.` otherwise|
|`not`|subroutine|performs a logical `not` operation on all the bits|
|`output`|subroutine|writes a bitset to an unformatted I/O unit|
|`read_bitset`|subroutine|reads a bitset from a bitset literal in a character string or formatted I/O unit|
|`set`|subroutine|sets a sequence of one or more bits to 1|
|`test`|function|`.true.` if the bit at `pos` is 1, `.false.` otherwise|
|`to_string`|subroutine|represents the bitset as a binary literal|
|`value`|function|1 if the bit at `pos` is 1, 0 otherwise|
|`write_bitset`|subroutine|writes a bitset as a bitset literal to a  character string or formatted I/O unit|

### Table of the non-member procedure overloads

The procedures with two arguments of type `bitset_large` or
`bitset_64` must have both arguments of the same known type which
prevents them from being methods. The bitwise "logical" procedures,
`and`, `and_not`, `or`, and `xor`  also require that the two bitset
arguments have the same number of bits, otherwise the results are
undefined. These procedures are summarized in the following table:

|Procedure name|Class|Summary|
|--------------|-----|-------|
|`and`|elemental subroutine|Sets `self` to the bitwise `and` of the original bits in `self` and `set2`|
|`and_not`|elemental subroutine|Sets `self` to the bitwise `and` of the original bits in `self` and the negation of `set2`|
|`extract`|subroutine|creates a new bitset, `new`, from a range in `old`|
|`or`|elemental subroutine|Sets `self` to the bitwise `or` of the original bits in `self` and `set2`|
|`xor`|elemental subroutine|Sets `self` to the bitwise exclusive `or` of the original bits in `self` and `set2`|


### Assignments

The module defines an assignment operation, `=`, that creates a
duplicate of an original bitset. It also defines assignments to and
from rank one arrays of logical type of kinds `int8`, `int16`,
`int32`, and `int64`. In the assignment to and from logical arrays
array index, `i`, is mapped to bit position, `pos=i-1`, and `.true.`
is mapped to a set bit, and `.false.` is mapped to an unset bit.


#### Example

```fortran
    program demo_assignment
        use stdlib_bitsets
        logical(int8)  :: logical1(64) = .true.
        logical(int32), allocatable :: logical2(:)
        type(bitset_64) :: set0, set1
        set0 = logical1
        if ( set0 % bits() /= 64 ) then
            error stop procedure // &
                ' initialization with logical(int8) failed to set' // &
                ' the right size.'
        else if ( .not. set0 % all() ) then
            error stop procedure // ' initialization with' // &
                ' logical(int8) failed to set the right values.'
        else
            write(*,*) 'Initialization with logical(int8) succeeded.'
        end if
        set1 = set0
        if ( set1 == set0 ) &
            write(*,*) 'Initialization by assignment succeeded'
        logical2 = set1
        if ( all( logical2 ) ) then
            write(*,*) 'Initialization of logical(int32) succeeded.'
        end if
    end program demo_assignment
```

### Table of the non-member comparison operations
The comparison operators with two arguments of type `bitset_large` or
`bitset_64` must have both arguments of the same known type which
prevents them from being methods. The operands must also have the same
number of bits otherwise the results are undefined. These operators
are summarized in the following table:

|Operator|Description|
|--------|-----------|
|`==`|`.true.` if all bits in `set1` and `set2` have the same value, `.false.` otherwise|
|`/=`|`.true.` if any bits in `set1` and `set2` differ in value, `.false.` otherwise|
|`>`|`.true.` if the bits in `set1` and `set2` differ in value and the highest order differing bit is 1 in `set1` and 0 in `set2`, `.false.` otherwise|
|`>=`|`.true.` if the bits in `set1` and `set2` are the same or the highest order differing bit is 1 in `set1` and 0 in `set2`, `.false.` otherwise|
|`<`|`.true.` if the bits in `set1` and `set2` differ in value and the highest order differing bit is 0 in `set1` and 1 in `set2`, `.false.` otherwise|
|`<=`|`.true.` if the bits in `set1` and `set2` are the same or the highest order differing bit is 0 in `set1` and 1 in `set2`, `.false.` otherwise|


## Specification of the `stdlib_bitsets` methods and procedures

### `all` - determine whether all bits are set in `self`.

#### Status

Experimental

#### Description

Determines whether all bits are set to 1 in self.

#### Syntax

`result = self % [[bitset_type(class):all(bound)]]()`

#### Class

Elemental function.

#### Argument

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if all bits in `self` are set,
otherwise it is `.false.`.

#### Example

```fortran
    program demo_all
        use stdlib_bitsets
        character(*), parameter :: &
            bits_all = '111111111111111111111111111111111'
        type(bitset_64) :: set0
        call set0 % from_string( bits_all )
        if ( .not. set0 % all() ) then
            error stop "FROM_STRING failed to interpret" // &
                "BITS_ALL's value properly."
        else
            write(*,*) "FROM_STRING transferred BITS_ALL properly" // &
                " into set0."
        end if
    end program demo_all
```

#### `and` - bitwise `and` of the bits of two bitsets.

#### Status

Experimental

#### Description

Sets the bits in `set1` to the bitwise `and` of the original bits in
`set1` and `set2`. Note that `set1` and `set2` must have the same
number of bits, otherwise the result is undefined.

#### Syntax

`call [[stdlib_bitsets(module):and(interface]] (set1, set2)`

#### Class

Elemental subroutine.

#### Arguments

`set1`: shall be a `bitset_64` or `bitset_large` scalar variable. It
is an `intent(inout)` argument. On return the values of the bits in
`set1` are the bitwise `and` of the original bits in `set1` with the
corresponding bits in `set2`.

`set2`: shall be a scalar expression of the same type as `set1`. It is
an `intent(in)` argument. Note that `set2` must also have the same
number of bits as `set1`.

#### Example

```fortran
    program demo_and
        use stdlib_bitsets
        type(bitset_large) :: set0, set1
        call set0 % init(166)
        call set1 % init(166)
        call and( set0, set1 ) ! none none
        if ( none(set0) ) write(*,*) 'First test of AND worked.'
        call set0 % not()
        call and( set0, set1 ) ! all none
        if ( none(set0) ) write(*,*) 'Second test of AND worked.'
        call set1 % not()
        call and( set0, set1 ) ! none all
        if ( none(set0) ) write(*,*) 'Third test of AND worked.'
        call set0 % not()
        call and( set0, set1 ) ! all all
        if ( all(set0) ) write(*,*) 'Fourth test of AND worked.'
    end program demo_and
```

### `and_not` - Bitwise `and` of one bitset with the negation of another

#### Status

Experimental

#### Description

Sets the bits of `set1` to bitwise `and` of the bits of `set1` with
the bitwise negation of the corresponding bits of `set2`. Note that
`set1` and `set2` must have the same number of bits, otherwise the
result is undefined.

#### Syntax

`call [[stdlib_bitsets(module):and_not(interface)]](set1, set2)`

#### Class

Elemental subroutine.

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` variable. It
is an `intent(inout)` argument. On return the values of the bits in
`set1` are the bitwise `and` of the original bits in `set1` with the
corresponding negation of the bits in `set2`.

`set2`: shall be a scalar expression of the same type as `set1`. It is
an `intent(in)` argument. Note that it should also have the same
number of bits as `set1`, otherwise the result is undefined.

#### Example

```fortran
    program demo_and_not
        use stdlib_bitsets
        type(bitset_large) :: set0, set1
        call set0 % init(166)
        call set1 % init(166)
        call and_not( set0, set1 ) ! none none
        if ( none(set0) ) write(*,*) 'First test of AND_NOT worked.'
        call set0 % not()
        call and_not( set0, set1 ) ! all none
        if ( all(set0) ) write(*,*) 'Second test of AND_NOT worked.'
        call set0 % not()
        call set1 % not()
        call and_not( set0, set1 ) ! none all
        if ( none(set0) ) write(*,*) 'Third test of AND_NOT worked.'
        call set0 % not()
        call and_not( set0, set1 ) ! all all
        if ( none(set0) ) write(*,*) 'Fourth test of AND_NOT worked.'
    end program demo_and_not
```

### `any` - determine whether any bits are set

#### Status

Experimental

#### Description

Determines whether any bits are set in `self`.

#### Syntax

`result = self % [[bitset_type(class):any(bound)]]()`

#### Class

Elemental function.

#### Argument

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

#### Result value

The result is a default logical scalar. The result is `.true.` if any bits in `self` are set, otherwise it
is `.false.`.

#### Example

```fortran
    program demo_any
        use stdlib_bitsets
        character(*), parameter :: &
            bits_0 = '0000000000000000000'
        type(bitset_64) :: set0
        call set0 % from_string( bits_0 )
        if ( .not. set0 % any() ) then
            write(*,*) "FROM_STRING interpreted " // &
                "BITS_0's value properly."
        end if
        call set0 % set(5)
        if ( set0 % any() ) then
            write(*,*) "ANY interpreted SET0's value properly."
        end if
    end program demo_any
```

### `bit_count` - return the number of bits that are set

#### Status

Experimental

#### Description

Returns the number of bits that are set to one in `self`.

#### Syntax

`result = self % [[bitset_type(class):bit_count(bound)]] ()`

#### Class

Elemental function.

#### Argument

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

#### Result value

The result is an integer scalar of kind `bits_kind`,
equal to the number of bits that are set in `self`.

#### Example

```fortran
    program demo_bit_count
        use stdlib_bitsets
        character(*), parameter :: &
            bits_0 = '0000000000000000000'
        type(bitset_64) :: set0
        call set0 % from_string( bits_0 )
        if ( set0 % bit_count() == 0 ) then
            write(*,*) "FROM_STRING interpreted " // &
                "BITS_0's value properly."
        end if
        call set0 % set(5)
        if ( set0 % bit_count() == 1 ) then
            write(*,*) "BIT_COUNT interpreted SET0's value properly."
        end if
    end program demo_bit_count
```

#### `bits` - returns the number of bits

#### Status

Experimental

#### Description

Reports the number of bits in `self`.

#### Syntax

`result = self % [[bitset_type(class):bits(bound)]] ()`

#### Class

Elemental function.

#### Argument

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

#### Result value

The result is an integer scalar of kind `bits_kind`, equal to
the number of defined bits in `self`.

#### Example

```fortran
    program demo_bits
        use stdlib_bitsets
        character(*), parameter :: &
            bits_0 = '0000000000000000000'
        type(bitset_64) :: set0
        call set0 % from_string( bits_0 )
        if ( set0 % bits() == 19 ) then
            write(*,*) "FROM_STRING interpreted " // &
                "BITS_0's size properly."
        end if
    end program demo_bits
```

### `clear` - clears a sequence of one or more bits.

#### Status

Experimental

#### Description

* If only `pos` is present, clears the bit with position `pos` in
`self`.

* If `start_pos` and `end_pos` are present with `end_pos >= start_pos`
clears the bits with positions from `start_pos` to `end_pos` in `self`.

* if `start_pos` and `end_pos` are present with `end_pos < start_pos`
`self` is unmodified.

Note: Positions outside the range 0 to `bits(set) -1` are ignored.

#### Syntax

`call self % [[bitset_type(class):clear(bound)]](pos)'

or

`call self % [[bitset_type(class):clear(bound)]](start_pos, end_pos)`

#### Class

Elemental subroutine

#### Arguments

`self`: shall be a scalar variable of class `bitset_type`. It is an
  `intent(inout)` argument.

`pos`: shall be a scalar integer expression of kind `bits_kind`. It is
an `intent(in)` argument.

`start_pos`: shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument.

`end_pos`: shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument.

#### Example

```fortran
    program demo_clear
        use stdlib_bitsets
        type(bitset_large) :: set0
        call set0 % init(166)
        call set0 % not()
        if ( set0 % all() ) write(*,*) 'SET0 is properly initialized.'
        call set0 % clear(165)
        if ( .not. set0 % test(165) ) write(*,*) 'Bit 165 is cleared.'
        call set0 % clear(0,164)
        if ( set0 % none() ) write(*,*) 'All bits are cleared.'
    end program demo_clear
```

### `extract` - create a new bitset from a range in an old bitset

#### Status

Experimental

#### Description

Creates a new bitset, `new`, from a range, `start_pos` to `stop_pos`,
in bitset `old`. If `start_pos` is greater than `stop_pos` the new
bitset is empty. If `start_pos` is less than zero or `stop_pos` is
greater than `bits(old)-1` then if `status` is present it has the
value `index_invalid_error`, otherwise processing stops with an
informative message.

#### Syntax

`call [[stdlib_bitsets(module):extract(interface)]](new, old, start_pos, stop_pos, status )`

#### Class

Subroutine

####  Arguments

`new`: shall be a scalar `bitset_64` or  `bitset_large` variable. It
is an `intent(out)` argument. It will be the new bitset.

`old`: shall be a scalar expression of the same type as `new`. It is
an `intent(in)` argument. It will be the source bitset.

`start_pos`: shall be a scalar integer expression of the kind
`bits_kind`. It is an `intent(in)` argument.

`stop_pos`: shall be a scalar integer expression of the kind
`bits_kind`. It is an `intent(in)` argument.

`status` (optional): shall be a scalar default integer variable. It is
an `intent(out)` argument. If present it shall have one of the values:

* `success` - no problems found

* `index_invalid_error` - `start_pos` was less than zero or `stop_pos`
  was greater than `bits(old)-1`.

#### Example

```fortran
    program demo_extract
        use stdlib_bitsets
        type(bitset_large) :: set0, set1
        call set0 % init(166)
        call set0 % set(100,150)
        call extract( set1, set0, 100, 150)
        if ( set1 % bits() == 51 ) &
            write(*,*) 'SET1 has the proper size.'
        if ( set1 % all() ) write(*,*) 'SET1 has the proper values.'
    end program demo_extract
```

### `flip` - flip the values of a sequence of one or more bits

#### Status

Experimental

#### Description

Flip the values of  a sequence of one or more bits.
* If only `pos` is present flip the bit value with position `pos` in
  `self`.
* If `start_pos` and `end_pos` are present with `end_pos >= start_pos`
flip the bit values with positions from `start_pos` to `end_pos` in
`self`.
* If `end_pos < start_pos` then `self` is unmodified.


#### Syntax

`call self % [[bitset_type(class):flip(bound)]] (pos)`

or

`call self % [[bitset_type(class):flip(bound)]] (start_pos, end_pos)`

#### Class

Elemental subroutine.

#### Arguments

`self`: shall be a scalar class `bitset_type` variable It is an
`intent(inout)` argument.

`pos`: shall be a scalar integer expression of kind `bits_kind`. It is
an `intent(in)` argument.

`start_pos`: shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument.

`end_pos`: shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument.

#### Example

```fortran
    program demo_flip
        use stdlib_bitsets
        type(bitset_large) :: set0
        call set0 % init(166)
        if ( set0 % none() ) write(*,*) 'SET0 is properly initialized.'
        call set0 % flip(165)
        if ( set0 % test(165) ) write(*,*) 'Bit 165 is flipped.'
        call set0 % flip(0,164)
        if ( set0 % all() ) write(*,*) 'All bits are flipped.'
    end program demo_flip
```

### `from_string` - initializes a bitset from a binary literal

#### Status

Experimental

#### Description

Initializes the bitset `self` from `string`, treating `string` as a
binary literal.

#### Syntax

`call self % [[bitset_type(class):from_string(bound)]](string[, status])`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar  class `bitset_type` variable. It is an
`intent(out)` argument.

`string`: shall be a scalar default character expression. It is an
`intent(in)` argument. It shall consist only of the characters "0",
and "1".

`status` (optional): shall be a scalar default integer variable. It is
an `intent(out)` argument. If present, on return its value shall be
one of the error codes defined in this module. If absent, and its
value would not have been `success`, then processing will stop with an
informative text as its stop code. It shall have one of the error
codes:

* `success` - if no problems were found,

* `alloc_fault` - if allocation of the bitset failed

* `char_string_too_large_error` - if `string` was too large, or 

* `char_string_invalid_error` - if string had an invalid character.


#### Example

```fortran
    program demo_from_string
        use stdlib_bitsets
        character(*), parameter :: &
            bits_all = '111111111111111111111111111111111'
        type(bitset_64) :: set0
        call set0 % from_string( bits_all )
        if ( bits(set0) /= 33 ) then
            error stop "FROM_STRING failed to interpret " // &
                'BITS_ALL's size properly."
        else if ( .not. set0 % all() ) then
            error stop "FROM_STRING failed to interpret" // &
                "BITS_ALL's value properly."
        else
            write(*,*) "FROM_STRING transferred BITS_ALL properly" // &
                " into set0."
        end if
    end program demo_from_string
```

### `init` - `bitset_type` initialization routines.

#### Status

Experimental

#### Description

`bitset_type` initialization routine.

#### Syntax

`call [[stdlib_bitsets(module):init(interface)]] (self, bits [, status])`

#### Class

Subroutine.

#### Arguments

`self`: shall be a scalar `bitset_64` or `bitset_large` variable. It
is an `intent(out)` argument.

`bits` (optional): shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument that if present
specifies the number of bits in `set`. A negative value, or a value
greater than 64 if `self` is of type `bitset_64`, is an error.

`status` (optional): shall be a scalar default integer variable. It is
an `intent(out)` argument that, if present, returns an error code
indicating any problem found in processing `init`, and if absent and
an error was found result in stopping processing with an informative
stop code. It can have any of the following error codes:

* `success` - no problem found

* `alloc_fault` - `self` was of type `bitset_large` and memory
  allocation failed

* `array_size_invalid_error` - bits was present with either a negative
  value, or a value greater than 64 when `self` was of type
  `bitset_64`.

#### Example

```fortran
    program demo_init
        use stdlib_bitsets
        type(bitset_large) :: set0
        call set0 % init(166)
        if ( set0 % bits() == 166 ) &
            write(*,*) `SET0 has the proper size.'
        if ( set0 % none() ) write(*,*) 'SET0 is properly initialized.'
    end program demo_init
```

### `input` - reads a bitset from an unformatted file

#### Status

Experimental

#### Description

Reads a bitset from its binary representation in an unformatted
file.

#### Syntax

`call self % [[bitset_type(class):input(bound)]] (unit [, status])`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar variable of class `bitset_64` or
`bitset_large`. It is an `intent(out)` argument.

`unit`: shall be a scalar default integer expression. It is an
`intent(in)` argument. Its value must be that of a logical unit
number for an open unformatted file with `READ` or `READWRITE`
access positioned at the start of a BITSET value written by a
`bitset_type` `output` subroutine by the same processor.

`status` (optional): shall be a scalar default integer variable. If
present its value shall be of one of the error codes defined in this
module. If absent and it would have had a value other than `success`
processing will stop with an informative stop code. Allowed error code
values for this `status` are:

* `success` - no problem found

* `alloc_fault` - `self` was of type `bitset_large` and allocation of
  memory failed.

* `array_size_invalid_error` - if the number of bits read from `unit`
  is either negative or greater than 64, if class of `self` is
  `bitset_64`.

* `read_failure` - failure during a read statement

#### Example

```fortran
    program demo_input
        character(*), parameter :: &
            bits_0   = '000000000000000000000000000000000', &
            bits_1   = '000000000000000000000000000000001', &
            bits_33  = '100000000000000000000000000000000'
        integer :: unit
        type(bitset_64) :: set0, set1, set2, set3, set4, set5
        call set0 % from_string( bits_0 )
        call set1 % from_string( bits_1 )
        call set2 % from_string( bits_33 )
        open( newunit=unit, file='test.bin', status='replace', &
            form='unformatted', action='write' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        close( unit )
        open( newunit=unit, file='test.bin', status='old', &
            form='unformatted', action='read' )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        close( unit )
        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop 'Transfer to and from units using ' // &
                ' output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'output and input succeeded.'
        end if
    end program demo_input
```

### `none` - determines whether no bits are set

#### Status

Experimental

#### Description

Determines whether no bits are set in `self`.

#### Syntax

`result = self % [[bitset_type(class):none(bound)]] ()`

#### Class

Elemental function.

#### Argument

`self`: shall be a scalar expression of class `bitset_type`. It is an
  `intent(in)` argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if no bits in `self` are set, otherwise it is
`.false.`.

#### Example

```fortran
    program demo_none
        use stdlib_bitsets
        character(*), parameter :: &
            bits_0 = '0000000000000000000'
        type(bitset_large) :: set0
        call set0 % from_string( bits_0 )
        if ( set0 % none() ) then
            write(*,*) "FROM_STRING interpreted " // &
                "BITS_0's value properly."
        end if
        call set0 % set(5)
        if ( .not. set0 % none() ) then
            write(*,*) "NONE interpreted SET0's value properly."
        end if
    end program demo_none
```

### `not` - Performs the logical complement on a bitset

#### Status

Experimental

#### Description

Performs the logical complement on the bits of `self`.

#### Syntax

`call self % [[bitset_type(class):not(bound)]] ()`

#### Class

Elemental subroutine.

#### Argument

`self` shall be a scalar variable of class `bitset_type`. It is an
  `intent(inout)` argument. On return its bits shall be the logical
  complement of their values on input.

#### Example

```fortran
    program demo_not
        use stdlib_bitsets
        type(bitset_large) :: set0
        call set0 % init( 155 )
        if ( set0 % none() ) then
            write(*,*) "FROM_STRING interpreted " // &
                "BITS_0's value properly."
        end if
        call set0 % not()
        if ( set0 % all() ) then
            write(*,*) "ALL interpreted SET0's value properly."
        end if
    end program demo_not
```

### `or` - Bitwise OR of the bits of two bitsets.

#### Status

Experimental

#### Description

Replaces the original bits of `set1` with the bitwise `or` of those
bits with the bits of `set2`. Note `set1` and `set2` must have the
same number of bits, otherwise the result is undefined.

#### Syntax

`call [[stdlib_bitsets(module):or(interface)]](set1, set2)`

#### Class

Elemental subroutine.

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` variable. It
 is an `intent(inout)` argument. On return the values of the bits in
 `setf` are the bitwise `or` of the original bits in `set1` with the
 corresponding bits in `set2`.

`set2`: shall be a scalar expression of the same type as `set1`. It is
an `intent(in)` argument. Note `bits(set2)` must equal `bits(set1)`
otherwise the results are undefined.

#### Example

```fortran
    program demo_or
        use stdlib_bitsets
        type(bitset_large) :: set0, set1
        call set0 % init(166)
        call set1 % init(166)
        call or( set0, set1 ) ! none none
        if ( none(set0) ) write(*,*) 'First test of OR worked.'
        call set0 % not()
        call or( set0, set1 ) ! all none
        if ( all(set0) ) write(*,*) 'Second test of OR worked.'
        call set0 % not()
        call set1 % not()
        call or( set0, set1 ) ! none all
        if ( all(set0) ) write(*,*) 'Third test of OR worked.'
        call set0 % not()
        call or( set0, set1 ) ! all all
        if ( all(set0) ) write(*,*) 'Fourth test of OR worked.'
    end program demo_or
```

### `output` - Writes a binary representation of a bitset to a file

#### Status

Experimental

#### Description

Writes a binary representation of a bitset to an unformatted file.

#### Syntax

`call self % [[bitset_type(class):output(bound)]] (unit[, status])`

#### Class

Subroutine.

#### Arguments

`self`: shall be a scalar expression of class `bitset_64` or
`bitset_large`. It is an `intent(in)` argument.

`unit`: shall be a scalar default integer expression. It is an
`intent(in)` argument. Its value must be that of an I/O unit number
for an open unformatted file with `WRITE` or `READWRITE` access.

`status` (optional): shall be a scalar default integer variable. It is
an `intent(out)` argument. If present on return it will have the value
of `success` or `write_failure`. If absent and it would not have the
value of `success` then processing will stop with an informative stop
code. The two code values have the meaning:

* `success` - no problem found

* `write_failure` - a failure occurred in a write statement.

#### Example

```fortran
    program demo_output
        character(*), parameter :: &
            bits_0   = '000000000000000000000000000000000', &
            bits_1   = '000000000000000000000000000000001', &
            bits_33  = '100000000000000000000000000000000'
        integer :: unit
        type(bitset_64) :: set0, set1, set2, set3, set4, set5
        call set0 % from_string( bits_0 )
        call set1 % from_string( bits_1 )
        call set2 % from_string( bits_33 )
        open( newunit=unit, file='test.bin', status='replace', &
            form='unformatted', action='write' )
        call set2 % output(unit)
        call set1 % output(unit)
        call set0 % output(unit)
        close( unit )
        open( newunit=unit, file='test.bin', status='old', &
            form='unformatted', action='read' )
        call set5 % input(unit)
        call set4 % input(unit)
        call set3 % input(unit)
        close( unit )
        if ( set3 /= set0 .or. set4 /= set1 .or. set5 /= set2 ) then
            error stop 'Transfer to and from units using ' // &
                ' output and input failed.'
        else
            write(*,*) 'Transfer to and from units using ' // &
                'output and input succeeded.'
        end if
    end program demo_output
```

### `read_bitset` - initializes `self` with the value of a *bitset_literal*

#### Status

Experimental

#### Description

Reads a *bitset-literal* and initializes `self` with the corresponding
value.


#### Syntax

`call self % [[bitset_type(class):read_bitset(bound)]](string[, status])`

or

`call self % [[bitset_type(class):read_bitset(bound)]](unit[, advance, status])`


#### Class

Subroutine

#### Arguments

`self`: shall be a scalar variable of class `bitset_type`. It is an
`intent(out)` argument. Upon a successful return it is initialized with
the value of a *bitset-literal*.

`string` (optional): shall be a scalar default character
expression. It is an `intent(in)` argument. It will consist of a left 
justified *bitset-literal*, terminated by either the end of the string
or a blank.

`unit` (optional): shall be a scalar default integer expression. It is
an `intent(in)` argument. Its value must be that of an I/O unit number
for an open formatted file with `READ` or `READWRITE` access
positioned at the start of a *bitset-literal*.

`advance` (optional): shall be a scalar default character
expression. It is an `intent(in)` argument. It is the `advance`
specifier for the final read of `unit`. If present it should have
the value `'yes'` or `'no'`. If absent it has the default value of
`'yes'`.

`status` (optional): shall be a scalar default integer variable. It is
an `intent(out)` argument. If present on return it shall have the
value of one of the error codes of this module. If absent and it would
not have had the value `success` processing will stop with a message
as its error code. The possible error codes are:

* `success` - no problems found;

* `alloc_fault` - if `self` is of class `bitset_large` and allocation
  of the bits failed;

* `array_size_invalid_error` - if the *bitset-literal* has a bits
  value greater than 64 and `self` is of class `bitset_64`;

* `char_string_invalid_error` - if the `bitset-literal` has an invalid
  character;

* `char_string_too_small_error` - if `string` ends before all the bits
  are read;

* `eof_failure` -  if a `read` statement reached an end-of-file before
   completing the read of the bitset literal,

* `integer_overflow_error` - if the *bitset-literal* has a `bits`
  value larger than `huge(0_bits_kind)`; or

* `read_failure` - if a read statement failed.

#### Example

```fortran
    program demo_read_bitset
        character(*), parameter :: &
            bits_0   = 'S33B000000000000000000000000000000000', &
            bits_1   = 'S33B000000000000000000000000000000001', &
            bits_33  = 'S33B100000000000000000000000000000000'
        character(:), allocatable :: test_0, test_1, test_2
        integer :: unit
        type(bitset_64) :: set0, set1, set2, set3, set4, set5
        call set0 % read_bitset( bits_0, status )
        call set1 % read_bitset( bits_1, status )
        call set2 % read_bitset( bits_2, status )
        call set0 % write_bitset( test_0, status )
        call set1 % write_bitset( test_1, status )
        call set2 % write_bitset( test_2, status )
        if ( bits_0 == test_0 .and. bits_1 == test_1 .and. &
            bits_2 == test_2 ) then
            write(*,*) 'READ_BITSET to WRITE_BITSET strings worked.'
        end if
        open( newunit=unit, file='test.txt', status='replace', &
            form='formatted', action='write' )
        call set2 % write_bitset(unit, advance='no')
        call set1 % write_bitset(unit, advance='no')
        call set0 % write_bitset(unit)
        close( unit )
        open( newunit=unit, file='test.txt', status='old', &
            form='formatted', action='read' )
        call set3 % read_bitset(unit, advance='no')
        call set4 % read_bitset(unit, advance='no')
        call set5 % read_bitset(unit)
        if ( set3 == set0 .and. set4 == set1 .and. set5 == set2 ) then
            write(*,*) WRITE_BITSET to READ_BITSET through unit worked.'
        end if
    end program demo_read_bitset
```

### `set` - sets a sequence of one or more bits to 1.

#### Status

Experimental

#### Description

Sets a sequence of one or more bits in `self` to 1.

* If `start_pos` and `end_pos` are absent sets the bit at position
`pos` in `self` to 1.

* If `start_pos` and `end_pos` are present with `end_pos >= start_pos`
set the bits at positions from `start_pos` to `end_pos` in `self` to 1.

* If `start_pos` and `end_pos` are present with `end_pos < start_pos`
`self` is unchanged.

* Positions outside the range 0 to `bits(self)` are ignored.


#### Syntax

`call self % [[bitset_type(class):set(bound)]] (POS)`

or

`call self % [[bitset_type(class):set(bound)]] (START_POS, END_POS)`

#### Class

Elemental subroutine

#### Arguments

`self`: shall be a scalar variable of class `bitset_type`. It is an
  `intent(inout)` argument.

`pos` (optional): shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument.

`start_pos` (optional): shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument.

`end_pos` (optional): shall be a scalar integer expression of kind
`bits_kind`. It is an `intent(in)` argument.

#### Example

```fortran
    program demo_set
        use stdlib_bitsets
        type(bitset_large) :: set0
        call set0 % init(166)
        if ( set0 % none() ) write(*,*) 'SET0 is properly initialized.'
        call set0 % set(165)
        if ( set0 % test(165) ) write(*,*) 'Bit 165 is set.'
        call set0 % set(0,164)
        if ( set0 % all() ) write(*,*) 'All bits are set.'
    end program demo_set
```

### `test` - determine whether a bit is set

#### Status

Experimental

#### Descriptions

Determine whether the bit at position `pos` is set to 1 in `self`.


#### Syntax

`result = self % [[bitset_type(class):test(bound)]](pos)`

#### Class

Elemental function.

#### Arguments

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

`pos`: shall be a scalar integer expression of kind `bits_kind`. It is
an `intent(in)` argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if the bit at `pos` in `self` is set,
otherwise it is `.false.`. If `pos` is outside the range
`0... bits(self)-1` the result is `.false.`.

#### Example

```fortran
    program demo_test
        use stdlib_bitsets
        type(bitset_large) :: set0
        call set0 % init(166)
        call set0 % not()
        if ( set0 % all() ) write(*,*) 'SET0 is properly initialized.'
        call set0 % clear(165)
        if ( .not. set0 % test(165) ) write(*,*) 'Bit 165 is cleared.'
        call set0 % set(165)
        if ( set0 % test(165) ) write(*,*) 'Bit 165 is set.'
    end program demo_test
```

### `to_string` - represent a bitset as a binary literal

### Status

Experimental

#### Description

Represents the value of `self` as a binary literal in `string`.

#### Syntax

`call self % [[bitset_type(class):to_string(bound)]](string[, status])`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

`string`: shall be a scalar default character variable of allocatable
length. It is an `intent(out)` argument. On return it shall hav a
*binary-literal* representation of the bitset `self`.

`status` (optional): shall be a scalar default integer variable. It is
an `intent(out)` argument. If present it shall have either the value
`success` or `alloc_fault`. If absent and it would have had the value
`alloc_fault` then processing will stop with an informative test as
the stop code. The values have the following meanings:

`success` - no problem found.

`alloc_fault` - allocation of `string` failed.


#### Example

```fortran
    program demo_to_string
        use stdlib_bitsets
        character(*), parameter :: &
            bits_all = '111111111111111111111111111111111'
        type(bitset_64) :: set0
        character(:), allocatable :: new_string
        call set0 % init(33)
        call set0 % not()
        call set0 % to_string( new_string )
        if ( new_string == bits_all ) then
            write(*,*) "TO_STRING transferred BITS0 properly" // &
                " into NEW_STRING."
        end if
    end program demo_to_string
```

### `value` - determine the value of a bit

#### Status

Experimental

#### Description

Determines the value of the bit at position, `pos`, in `self`.

#### Syntax

`result = self % [[bitset_type(class):value(bound)]](pos)`

#### Class

Elemental function.

#### Arguments

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

`pos`: shall be a scalar integer expression of kind `bits_kind`. It is
an `intent(in)` argument.

#### Result value

The result is a default integer scalar.
The result is one if the bit at `pos` in `self` is set, otherwise it
is zero. If `pos` is outside the range `0... bits(set)-1` the result
is zero.

#### Example

```fortran
    program demo_value
        use stdlib_bitsets
        type(bitset_large) :: set0
        call set0 % init(166)
        call set0 % not()
        if ( set0 % all() ) write(*,*) 'SET0 is properly initialized.'
        call set0 % clear(165)
        if ( set0 % value(165) == 0 ) write(*,*) 'Bit 165 is cleared.'
        call set0 % set(165)
        if ( set0 % value(165) == 1 ) write(*,*) 'Bit 165 is set.'
    end program demo_value
```

### `write_bitset` - writes a *bitset-literal*

#### Status

Experimental

#### Description

Writes a *bitset-literal* representing `self`'s current value to a
character string or formatted file.


#### Syntax

`call self % [[bitset_type(class):write_bitset(bound)]](string[, status])`

or

`call self % [[bitset_type(class):write_bitset(bound)]] (unit[, advance, status])`

#### Class

Subroutine

#### Arguments

`self`: shall be a scalar expression of class `bitset_type`. It is an
`intent(in)` argument.

`string` (optional): shall be a scalar default character variable of
allocatable length. It is an `intent(out)` argument.

`unit` (optional): shall be a scalar default logical expression. It is
an `intent(in)` argument. Its value must be that of a I/O unit number
for an open formatted file with `write` or `readwrite` access.

`advance` (optional): shall be a scalar default character
expression. It is an `intent(in)` argument. It is the `advance` 
specifier for the write to `unit`. If present it must have the value
`'yes'` or `'no'`. It has the default value of `'yes'`.

* if `advance` is not present or is present with a value of `'no'`
  then the bitset's *bitset-literal* is written to `unit`
  followed by a blank, and the current record is not advanced.

* If `advance` is present with a value of `'yes'` then the
  bitset's *bitset-literal* is written to `unit` and the
  record is immediately advanced.

`status` (optional): shall be a scalar default integer variable. It is
an `intent(out)` argument. If present on return it shall have the
value of one of the module's error codes. If absent and a problem was
found processing will stop with an informative stop code. It may have
the following error code values:

* `success` - no problem was found

* `alloc_fault` - allocation of the string failed

* `write_failure` - the `write` to the `unit` failed

#### Example

```fortran
    program demo_write_bitset
        character(*), parameter :: &
            bits_0   = 'S33B000000000000000000000000000000000', &
            bits_1   = 'S33B000000000000000000000000000000001', &
            bits_33  = 'S33B100000000000000000000000000000000'
        character(:), allocatable :: test_0, test_1, test_2
        integer :: unit
        type(bitset_64) :: set0, set1, set2, set3, set4, set5
        call set0 % read_bitset( bits_0, status )
        call set1 % read_bitset( bits_1, status )
        call set2 % read_bitset( bits_2, status )
        call set0 % write_bitset( test_0, status )
        call set1 % write_bitset( test_1, status )
        call set2 % write_bitset( test_2, status )
        if ( bits_0 == test_0 .and. bits_1 == test_1 .and. &
            bits_2 == test_2 ) then
            write(*,*) 'READ_BITSET to WRITE_BITSET strings worked.'
        end if
        open( newunit=unit, file='test.txt', status='replace', &
            form='formatted', action='write' )
        call set2 % write_bitset(unit, advance='no')
        call set1 % write_bitset(unit, advance='no')
        call set0 % write_bitset(unit)
        close( unit )
        open( newunit=unit, file='test.txt', status='old', &
            form='formatted', action='read' )
        call set3 % read_bitset(unit, advance='no')
        call set4 % read_bitset(unit, advance='no')
        call set5 % read_bitset(unit)
        if ( set3 == set0 .and. set4 == set1 .and. set5 == set2 ) then
            write(*,*) WRITE_BITSET to READ_BITSET through unit worked.'
        end if
    end program demo_write_bitset
```

### `xor` - bitwise exclusive `or`

#### Status

Experimental

#### Description

Replaces `set1`'s bitset with the bitwise exclusive `or` of the
original bits of `set1` and `set2`. Note `set1` and `set2` must have
the samee number of bits, otherwise the result is undefined.

#### Syntax

`result = [[stdlib_bitsets(module):xor(interface)]] (set1, set2)`

#### Class

Elemental subroutine

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` variable. It
is an `intent(inout)` argument. On return the values of the bits in
`set1` are the bitwise exclusive `or` of the original bits in `set1`
with the corresponding bits in `set2`.

`set2` shall be a scalar expression of the same type as `set1`. It is
  an `intent(in)` argument. Note `set1` and `set2` must have the
samee number of bits, otherwise the result is undefined.

#### Example

```fortran
    program demo_xor
        use stdlib_bitsets
        type(bitset_large) :: set0, set1
        call set0 % init(166)
        call set1 % init(166)
        call xor( set0, set1 ) ! none none
        if ( none(set0) ) write(*,*) 'First test of XOR worked.'
        call set0 % not()
        call xor( set0, set1 ) ! all none
        if ( all(set0) ) write(*,*) 'Second test of XOR worked.'
        call set0 % not()
        call set1 % not()
        call xor( set0, set1 ) ! none all
        if ( all(set0) ) write(*,*) 'Third test of XOR worked.'
        call set0 % not()
        call xor( set0, set1 ) ! all all
        if ( none(set0) ) write(*,*) 'Fourth test of XOR worked.'
    end program demo_xor
```

## Specification of the `stdlib_bitsets` operators

### `==` - compare two bitsets to determine whether the bits have the same value

#### Status

Experimental

#### Description

Returns `.true.` if all bits in `set1` and `set2` have the same value,
`.false.`  otherwise.

#### Syntax

`result = set1 [[stdlib_bitsets(module):==(interface)]] set2

#### Class

Elemental operator

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` expression. It
is an `intent(in)` argument.

`set2`: shall be a scalar expression of the same type as `self`. It
will have the same number of bits as `set1`. It is an `intent(in)`
argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if the bits in both bitsets are set
to the same value, otherwise the result is `.FALSE.`.

#### Example

```fortran
    program demo_equality
        use stdlib_bitsets
        type(bitset_64) :: set0, set1, set2
        call set0 % init( 33 )
        call set1 % init( 33 )
        call set2 % init( 33 )
        call set1 % set( 0 )
        call set2 % set( 32 )
        if ( set0 == set0 .and. set1 == set1 .and. set2 == set2 .and. &
            .not. set0 == set1 .and. .not. set0 == set2 .and. .not.   &
            set1 == set2 ) then
            write(*,*) 'Passed 64 bit equality tests.'
        else
            error stop 'Failed 64 bit equality tests.'
        end if
    end program demo_equality
```

### `/=` - compare two bitsets to determine whether any bits differ in value

#### Status

Experimental

#### Description

Returns `.true.` if any bits in `self` and `set2` differ in value,
`.false.`  otherwise.


#### Syntax

`result = set1 [[stdlib_bitsets(module):/=(interface)]] set2`

#### Class

Elemental function

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` expression. It
is an `intent(in)` argument.

`set2`: shall be a scalar expression of the same type as `self`. It
will have the same number of bits as `set1`. It is an `intent(in)`
argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if any bits in both bitsets differ, otherwise
the result is `.false.`.

#### Example

```fortran
    program demo_inequality
        use stdlib_bitsets
        type(bitset_64) :: set0, set1, set2
        call set0 % init( 33 )
        call set1 % init( 33 )
        call set2 % init( 33 )
        call set1 % set( 0 )
        call set2 % set( 32 )
        if ( set0 /= set1 .and. set0 /= set2 .and. set1 /= set2 .and. &
            .not. set0 /= set0 .and. .not. set1 /= set1 .and. .not.   &
            set2 /= set2 ) then
            write(*,*) 'Passed 64 bit inequality tests.'
        else
            error stop 'Failed 64 bit inequality tests.'
        end if
    end program demo_inequality
```

### `>=` - compare two bitsets to determine whether the first is greater than or equal to the second

#### Status

Experimental

#### Description

Returns `.true.` if the bits in `set1` and `set2` are the same or the
highest order different bit is set to 1 in `set1` and to 0 in `set2`,
`.false.`.  otherwise. The sets must be the same size otherwise the
results are undefined


#### Syntax

`result = set1 [[stdlib_bitsets(module):>=(interface)]] set2`

#### Class

Elemental operator

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` expression. It
is an `intent(in)` argument.

`set2`: shall be a scalar expression of the same type as `self`. It
will have the same number of bits as `set1`. It is an `intent(in)`
argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if the bits in `set1` and `set2` are the same
or the highest order different bit is set to 1 in `set1` and to 0 in
`set2`, `.false.`  otherwise.

#### Example

```fortran
    program demo_ge
        use stdlib_bitsets
        type(bitset_64) :: set0, set1, set2
        call set0 % init( 33 )
        call set1 % init( 33 )
        call set2 % init( 33 )
        call set1 % set( 0 )
        call set2 % set( 32 )
        if ( set1 >= set0 .and. set2 >= set1 .and. set2 >= set0 .and. &
            set0 >= set0 .and. set1 >= set1 .and. set2 >= set2 .and. &
            .not. set0 >= set1 .and. .not. set0 >= set2 .and. .not.   &
            set1 >= set2 ) then
            write(*,*) 'Passed 64 bit greater than or equals tests.'
        else
            error stop 'Failed 64 bit greater than or equals tests.'
        end if
    end program demo_ge
```

### `>` - compare two bitsets to determine whether the first is greater than the other

#### Status

Experimental

#### Description

Returns `.true.` if the bits in `set1` and `set2` differ and the
highest order different bit is set to 1 in `set1` and to 0 in `set2`,
`.false.`  otherwise. The sets must be the same size otherwise the
results are undefined

#### Syntax

`result = set1 [[stdlib_bitsets(module):>(interface)]] set2`

#### Class

Elemental operator

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` expression. It
is an `intent(in)` argument.

`set2`: shall be a scalar expression of the same type as `self`. It
will have the same number of bits as `set1`. It is an `intent(in)`
argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if the bits in `set1` and `set2` differ and the
highest order different bit is set to 1 in `set1` and to 0 in `set2`,
`.false.`.  otherwise.

#### Example

```fortran
    program demo_gt
        use stdlib_bitsets
        type(bitset_64) :: set0, set1, set2
        call set0 % init( 33 )
        call set1 % init( 33 )
        call set2 % init( 33 )
        call set1 % set( 0 )
        call set2 % set( 32 )
        if ( set1 > set0 .and. set2 > set1 .and. set2 > set0 .and. &
            .not. set0 > set0 .and. .not. set0 > set1 .and. .not.   &
            set1 > set2 ) then
            write(*,*) 'Passed 64 bit greater than tests.'
        else
            error stop 'Failed 64 bit greater than tests.'
        end if
    end program demo_gt
```

### `<=` - compare two bitsets to determine whether the first is less than or equal to the other

#### Status

Experimental

#### Description

Returns `.true.` if the bits in `set1` and `set2` are the same or the
highest order different bit is set to 0 in `set1` and to 1 in `set2`,
`.false.`  otherwise. The sets must be the same size otherwise the
results are undefined


#### Syntax

`result = set1 [[stdlib_bitsets(module):<=(interface)]] set2`

#### Class

Elemental operator

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` expression. It
is an `intent(in)` argument.

`set2`: shall be a scalar expression of the same type as `self`. It
will have the same number of bits as `set1`. It is an `intent(in)`
argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if the bits in `set1` and `set2` are the same
or the highest order different bit is set to 0 in `set1` and to 1 in
`set2`, `.false.`  otherwise.

#### Example

```fortran
    program demo_le
        use stdlib_bitsets
        type(bitset_64) :: set0, set1, set2
        call set0 % init( 33 )
        call set1 % init( 33 )
        call set2 % init( 33 )
        call set1 % set( 0 )
        call set2 % set( 32 )
        if ( set0 <= set1 .and. set1 <= set2 .and. set0 <= set2 .and. &
            set0 <= set0 .and. set1 <= set1 .and. set2 <= set2 .and. &
            .not. set1 <= set0 .and. .not. set2 <= set0 .and. .not.   &
            set2 <= set1 ) then
            write(*,*) 'Passed 64 bit less than or equal tests.'
        else
            error stop 'Failed 64 bit less than or equal tests.'
        end if
    end program demo_le
```

### `<` - compare two bitsets to determine whether the first is less than the other

#### Status

Experimental

#### Description

Returns `.true.` if the bits in `set1` and `set2` differ and the
highest order different bit is set to 0 in `set1` and to 1 in `set2`,
`.false.`  otherwise. The sets must be the same size otherwise the
results are undefined


#### Syntax

`result = set1 [[stdlib_bitsets(module):<(interface)]] set2`

#### Class

Elemental operator

#### Arguments

`set1`: shall be a scalar `bitset_64` or `bitset_large` expression. It
is an `intent(in)` argument.

`set2`: shall be a scalar expression of the same type as `self`. It
will have the same number of bits as `set1`. It is an `intent(in)`
argument.

#### Result value

The result is a default logical scalar.
The result is `.true.` if the bits in `set1` and `set2` differ and the
highest order different bit is set to 0 in `set1` and to 1 in `set2`,
`.false.`  otherwise.

#### Example

```fortran
    program demo_lt
        use stdlib_bitsets
        type(bitset_64) :: set0, set1, set2
        call set0 % init( 33 )
        call set1 % init( 33 )
        call set2 % init( 33 )
        call set1 % set( 0 )
        call set2 % set( 32 )
        if ( set0 < set1 .and. set1 < set2 .and. set0 < set2 .and. &
            .not. set0 < set0 .and. .not. set2 < set0 .and. .not.   &
            set2 < set1 ) then
            write(*,*) 'Passed 64 bit less than tests.'
        else
            error stop 'Failed 64 bit less than tests.'
        end if
    end program demo_lt
```
