---
title: hash
---

# The `stdlib_hash_32bit` and `stdlib_hash_64bit` modules

[TOC]

## Overview of hash procedures

The comparison of lexical entities or other objects for equality
can be computationally expensive.
This cost is often reduced by computing a near unique integer value,
termed a hash code, from the structure of the object using a procedure
termed a hash function.
Equality of hash codes is a necessary, but not sufficient, condition
for the original objects to be equal.
As integer comparisons are very efficient, performing an initial
comparison of hash codes and then performing a detailed comparison
only if the hash codes are equal can improve performance.
The hash codes, in turn, can be mapped to a smaller set of integers,
that can be used as an index, termed a hash index, to a rank-1
array, often termed a hash table.
This mapping will be known as a scalar hash.
The use of a hash table reduces the number of hash codes that need to
be compared, further improving performance.
A hash function can also be used to generate a checksum to verify that
data has not changed.
The Fortran Standard Library therefore provides procedures to compute
hash codes and scalar hashes.
This document only discusses the hash codes and scalar hashes in the
library.

## Licensing

The Fortran Standard Library is distributed under the MIT License.
However components of the library may be based on code released under a
different license. In particular, the hash codes are often based
on algorithms considered as public domain (`Fibonacci Hash`, `Universal
Multiplicative Hash)`or released under a different license than the
MIT license (`FNV-1 Hash`, `FNV-1A Hash`, `nmhash32`, `nmhash32x`,
`waterhash`, `pengyhash` and `SpookyHash`)
The licensing status of the algorithms are discussed below.

`fibonacci_hash` is a scalar hash. It is an implementation in Fortran
2008 and signed two's complement integers of the Fibonacci Hash
described in D. E. Knuth, "The Art of
Computer Programming, Second Edition, Volume 3, Sorting and
Searching", Addison-Wesley, Upper Saddle River, NJ,
pp. 517-518, 1998. The algorithms in that source are considered public
domain, and its use is unrestricted.

`universal_mult_hash` is a scalar hash. It is an implementation in
Fortran 2008 and signed two's complement integers of the
universal multiplicative hash algorithm of M. Dietzfelbinger,
T. Hagerup, J. Katajainen, and M. Penttonen, "A Reliable Randomized
Algorithm for the Closest-Pair Problem," J. Algorithms, Vol. 25,
No. 1, Oct. 1997, pp. 19-51. Because of its publication in the Journal
of Algorithms, the universal multiplicative hash algorithm is public
domain.

`fnv_1_hash` and `fnv_1a_hash` are translations to Fortran 2008 and
signed two's complement integers of the
`FNV-1` and `FNV-1a` hash functions of Glenn Fowler, Landon Curt Noll,
and Phong Vo, that has been released into the public
domain. Permission has been granted, by Landon Curt Noll, for the use
of these algorithms in the Fortran Standard Library. A description of
these functions is available at
<https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function>.
These functions have been modified from their normal forms to also
encode the structure size in the output hash.

Similarly `spooky_hash` and associated procedures are translations to
Fortran 2008 and signed two's complement integers of the unsigned 64
bit version 2 `SpookyHash` functions of Bob
Jenkins <https://burtleburtle.net/bob/hash/spooky.html> to signed 64
bit operations. Version 2 was chosen over version 1 as it has better
performance and fewer bad seeds
Bob Jenkins has also put this code in the public
domain and has given permission to treat this code as public domain in
the USA, provided the code can be used under other licenses and he is
given appropriate credit.

`nmhash32` and `nmhash32x` are translations to Fortran 2008 and signed
two's complement integers of the unsigned 32-bit
hashes of James Z. M. Gao's `nmhash32` and `nmhash32x` version of 0.2,
<https://github.com/gzm55/hash-garage/blob/a8913138bdb3b7539c202edee30a7f0794bbd835/nmhash.h>
James Z. M. Gao has released his code under the BSD 2 Clause
License. The BSD 2-Clause license is as follows:

    BSD 2-Clause License

    Copyright (c) 2021, James Z.M. Gao
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
       this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright notice,
       this list of conditions and the following disclaimer in the documentation
       and/or other materials provided with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
    AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
    IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
    ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
    LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
    CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
    SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
    INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
    CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
    ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.

`water_hash` is a translation to Fortran 2008  and signed two's
complement integers of the `waterhash` algorithm
of Tommy Ettinger. This algorithm is inspired by the Wy Hash of
Wang Yi. Tommy Ettinger's original C++ code, `waterhash.h`,
is available at URL: <https://github.com/tommyettinger/waterhash> under
the `unlicense`,
<https://github.com/tommyettinger/waterhash/blob/master/LICENSE>.
The `unlicense` reads as follows:

    This is free and unencumbered software released into the public domain.
    Anyone is free to copy, modify, publish, use, compile, sell, or
    distribute this software, either in source code form or as a compiled
    binary, for any purpose, commercial or non-commercial, and by any
    means.

    In jurisdictions that recognize copyright laws, the author or authors
    of this software dedicate any and all copyright interest in the
    software to the public domain. We make this dedication for the benefit
    of the public at large and to the detriment of our heirs and
    successors. We intend this dedication to be an overt act of
    relinquishment in perpetuity of all present and future rights to this
    software under copyright law.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
    OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
    ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.

    For more information, please refer to <http://unlicense.org>

`pengy_hash` is a translation to Fortran 2008 and signed two's
complement arithmetic of the `pengyhash` algorithm of Alberto Fajardo,
copyright 2020. Alberto Fajardo's original C code, `pengyhash.c`, is
available at the URL:
https://github.com/tinypeng/pengyhash/blob/master/pengyhash.c
under the BSD 2-Clause License:
https://github.com/tinypeng/pengyhash/blob/master/LICENSE

The BSD 2-Clause license is as follows:

    BSD 2-Clause License

    pengyhash
    Copyright (c) 2020 Alberto Fajardo
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following
    disclaimer in the documentation and/or other materials provided
    with the distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
    CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
    INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
    BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
    ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
    TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
    THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
    SUCH DAMAGE.

## Glossary

There are a few words used in this document that may not be familiar to
readers of this document:

* Key - a value to be used to find entries in a hash table typically
  using its hashed value for the initial search;

* Salt - see seed, and;

* Seed - an additional argument to a hash function that changes its
  output making some attacks impractical.


## The hash codes modules

### Overview of the modules

The Standard Library provides two modules implementing hash
functions and scalar hashes.
The `stdlib_hash_32bit` module provides procedures to
compute 32-bit integer hash codes and a scalar hash.
The hash codes can be used for tables of up to `2**30` entries, and
for keys with a few hundred elements, but performance has only been
tested for tables up to `2**16` entries and performance may degrade
for larger numbers of entries.
The `stdlib_hash_64bit` module provides hash procedures to
compute 64-bit integer hash codes and a scalar hash.
The hash codes can, in principle, be used for tables of up to `2**62`
entries, and for keys with a few thousand elements, but testing of
performance has only been been for tables up to `2**16`elements and
performance may degrade for larger numbers of entries.
While one of the codes in `stdlib_hash_64bit`,
`SPOOKY_HASH`, can also be used to calculate 128 bit hash codes, none
of the current codes can be used to calculate 256 bit hash codes.
Such larger hash codes are useful for larger hash tables and keys, and
for checksums.
Such larger keys and tables are little used, if used at all, in
current
Fortran codes, but the larger hash codes may be added to the library
if there is a demand for them.

Hash functions are often divided into two categories
"cryptographic" and "non-cryptographic".
Cryptographic hash functions produce codes that are infeasible to
reverse without additional information beyond the identity of
the hash function used to generate the code and the resulting codes.
Non-cryptographic codes, in some circumstances, are believed to be
reversible.
The modules only implement hash
functions that are considered non-cryptographic, with
implementations available in the public domain.

There are a number of algorithms available for the computation of
non-cryptographic 32 and 64-bit hash codes that differ in their
computational complexity,
their relative performance on different size keys, and the
expected uniqueness (randomness) of the resulting hash codes.
Their relative performance in the analysis of text, in particular,
can depend on the compiler, character set, language, and content.
The quality of a hash function is often evaluated using
the SMHasher test suite, originally written by
[Austin Appleby](https://github.com/aappleby/smhasher), but greatly
extended by [Reini Urban](https://github.com/rurban/smhasher).
All except the simplest, `FNV_1` and `FNV_1A`, of the hash functions
defined in the modules perform well on the tests in Reini Urban's
version of SMHasher.

There are two problems in implementing hash functions in Fortran.
First, the static typing of Fortran makes it awkward to define general
purpose hash functions.
Instead hash functions are defined for some of the more common
objects: character strings and rank-1 arrays of integers.
Other objects can, in principle, be hashed by using `transfer` to
map their contents to an integer array, typically one of kind `int8`.
The other problem is that hash codes are typically defined using
modular unsigned integer arithmetic.
As such integers are not part of the current Fortran standard,
workarounds have to be used.
These can take two forms.
In one, the operations are emulated by using an integer of a
larger size, or, for the larger integers, by dividing the integer into
two lower and higher order halves,
and performing the operations on each half separately using
the larger integers.
In the second, the unsigned integers may be replaced directly by
the corresponding signed integers, but
otherwise not modifying the code logic.
The first should be standard conforming on current compilers, but
is more computationally intensive unless the compilers recognize
underlying idioms that are rarely used in Fortran codes. The second is
not standard conforming as bit operations involving the sign are
undefined,
but should yield equivalent results with fewer operations on
compilers with two's complement integers that do not trap on over
or under flow. The codes currently use the second method.

In order to compile the hash function modules, the compilers must
implement much of Fortran 2003, and selected components of Fortran
2008: submodules, 64-bit integers, and some bit intrinsics.
The main limitation on valid compilers is whether they
implement the submodules enhancement of Fortran 2008.
In order to properly run the hash functions, the compilers must
use two's complement integers, and be able to execute them with
wraparound semantics and no integer overflow exceptions.
Current Fortran 2003+ compilers solely use two's complement
integers, and appear to be able to turn off overflow detection,
so the modules use signed integer arithmetic. For that reason
trapping on signed arithmetic must be disabled. The command line
flags to disable overflow detection for compilers implementing
submodules are summarized in the table below.
Note that FLANG, gfortran (since version 10), ifort, and NAG all default to
integer overflow wrapping.

|Compiler|Legal flag|Illegal flag|Default|
|---------|----------|------------|-------|
| ARM Fortran | NA? | NA? | overflow wrapping? |
| Cray Fortran | NA? | NA? | overflow wrapping? |
| FLANG/PGI | -fwrapv | -ftrapv | -fwrapv |
| gfortran | -fwrapv | -ftrapv | -fwrapv |
| IBM Fortran | NA? | NA? | overflow wrapping? |
| ifort| NA? | NA? | overflow wrapping |
| NAG Fortran | -C=none | -C=intovf | -C=none |
| NEC Fortran | NA? | NA? | overflow wrapping? |
| NVIDIA Fortran | NA? | NA? | overflow wrapping? |

All of the modules' hash functions take one or two arguments.
All of them have as their first argument the object to be hashed,
termed a *key*.
Most have a second argument, termed a *seed*, that sets the initial
value of the hash code changing the hash function behavior.
In particular, inputs that hash to the same hash index with a given
seed, will often hash to different indexes with a different seed.
This difference in behavior makes algorithms that use a seed much
more resistant to denial of service attacks that use the properties
of a known hash to increase the number of hash table collisions.
This additional integer must be kept the same for all hashes
in a given hash table, but can be changed and the objects rehashed
if collisions are unusually common.
The *seed* can be either a scalar or a two-element array.
Some of the hash functions have alternatives that allow incremental
hashing.

|Algorithm|Seed|Result|
|---------|----|------|
|FNV-1|None|32 or 64-bit integer|
|FNV-1a|None|32 or 64-bit integer|
|nmhash32 |32-bit scalar integer|32-bit integer|
|nmhash32x |32-bit scalar integer|32-bit integer|
|pengyhash |32-bit scalar integer|64-bit integer|
|Spooky Hash|64-bit two element vector|64-bit two element vector|
|waterhash|64-bit scalar integer|32-bit integer|

The hash function modules each provide at least five algorithms for
hash functions: two optimized for small (< 32 `int8` integer elements)
keys, and three optimized for large (> 100 `int8` integer elements)
keys.
The core implementation for each algorithm is for keys that are
vectors of `int8` integers.
These core implementations are then used in wrappers for keys
that are vectors of `int16`, `int32` and `int64` integers, or default
character strings, in the expectation that inlining will eliminate the
overhead of transferring the other keys to `int8` integer vectors.

The `stdlib_hash_32bit` module provides
implementations of five hash code algorithms:
the *FNV_1* and *FNV_1A* variants of Glenn Fowler,
Landon Curt Noll, and Kiem-Phong Vo;
the *nmhash32* and *nmhash32x* of James Z. M. Gao;
and the *waterhash*  of Tommy Ettinger.
The detailed implementation of each algorithm is handled in a separate
submodule: `stdlib_hash_32bit_fnv`,
`stdlib_hash_32bit_nm`, and `stdlib_hash_32bit_water`,
respectively. The `nmhash32`, `nmhash32x`, and `waterhash` algorithms
require seeds. The submodules provide separate seed generators
for each algorithm.
The module itself
implements two scalar hash functions, `fibonacci_hash` and
`universal_mult_hash`.
It also implements the subroutine, `odd_random_integer`, for
generating seeds for `universal_mult_hash`.
All assume a two's complement sign bit, and no out of
range checks.

The `stdlib_hash_64bit` module also provides
implementations of four hash code algorithms:
the *FNV_1* and *FNV_1A* variants of Glenn Fowler,
Landon Curt Noll, and Kiem-Phong Vo;
the *pengyhash* of Alberto Fajardo;
and the *SpookyHash*  of Bob Jenkins.
The detailed implementation of each algorithm is handled in a separate
submodule: `stdlib_hash_64bit_fnv`,
`stdlib_hash_64bit_pengy`, and `stdlib_hash_64bit_spooky`,
respectively.
The `pengyhash`, and `Spooky Hash` algorithms
require seeds. The submodules provide separate seed generators
for each algorithm.
The module itself implements two scalar hash functions,
`fibonacci_hash` and `universal_mult_hash`.
It also implements the subroutine, `odd_random_integer`, for
generating seeds for `universal_mult_hash`.
All assume a two's complement sign bit, and no out of
range checks.

The `stdlib_hash_32bit_fnv` and `stdlib_hash_64bit_fnv`
submodules each provide implementations of the FNV-1 and FNV-1A
algorithms in the form of two separate overloaded functions: `FNV_1`
and `FNV_1A`.
The FNV-1 and FNV-2 algorithms differ in their order of the
multiplication and exclusive or operations.
They differ from their normal implementation in that they also
encode the structure size in the hash code.
The 32 and 64-bit algorithms differ in their initial offsets and in
their multiplicative constants.
Analysis suggests that `FNV_1A` should be better at randomizing the
input, but tests with hash tables show negligible difference.
These algorithms have the reputation of being particularly useful for
small byte strings, i.e., strings of less than 32 bytes.
While they do not at all perform well on the SMHasher test suite,
usage indicates that this has little impact on the
performance of small hash tables, and the small size of the functions
allows their quick loading and retainment in the instruction cache,
giving a performance boost where the hashing is intermittent.
(See the
[SMHasher discussion](https://github.com/rurban/smhasher/README.md)
and [S. Richter, V. Alvarez, and J. Dittrich. 2015. A Seven-Dimensional Analysis of Hashing Methods and its Implications on Query Processing, Proceedings of the VLDB Endowment, Vol. 9, No. 3.](https://bigdata.uni-saarland.de/publications/p249-richter.pdf) [https://doi.org/10.14778/2850583.2850585](https://doi.org/10.14778/2850583.2850585).

The `stdlib_hash_32bit_nm` submodule provides implementations
of James Z.M. Gao's `nmhash32` and `nmhash32x` algorithms,
version 0.2,
in the form of the overloaded functions, `nmhash32` and `nmhash32x`.
The implementations are based on the scalar versions of Gao's
algorithms and not the vector versions that require access to
the vector instructions of some compilers.
Both algorithms perform well on the SMHasher tests, and have no known
bad seeds. The vector versions of both codes perform well on large
keys, with the `nmhash32x` faster on short keys. To provide randomly
generated seeds for the two functions the submodule also defines the
subroutines `new_nmhash32_seed` and `new_nmhash32x_seed`. Gao claims
that `nmhash32x` is significantly faster than `nmhash32` on short
seeds, but slower on long seeds, but our limited testing so far shows
`nmhash32x` to be significantly faster on short seeds and slightly
faster on long seeds.

The `stdlib_hash_32bit_water` submodule provides implementations
of Tommy Ettinger's `waterhash` algorithm in the form of the overloaded
function, `water_hash`. Water Hash has not been tested by Reini Urban,
but Tommy Ettinger has tested it with Urban's SMHasher and presents
results that shows Water Hash passing all the tests. So far his
testing hasn't found any bad seeds for the algorithm. To provide
randomly generated seeds for the hash function the submodule also
defines the subroutine `new_water_hash_seed`.

The `stdlib_hash_64bit_pengy` submodule provides implementations of
Alberto Fajardo's `pengyhash` in the form of the overloaded function,
`pengy_hash`. Reini Urban's testing shows that PengyHash passes all
the tests and has no bad seeds.  To provide randomly generated seeds
for the hash function the submodule also defines the subroutine
`new_pengy_hash_seed`.

The `stdlib_hash_64bit_spooky` submodule provides implementations
of Bob Jenkins' SpookyHash in the form of the overloaded function,
`spooky_hash`. Future implementations may provide the SpookyHash
incremental hashing procedures.
SpookyHash is optimized for large objects and should give excellent
performance for objects greater than about 96 byes, but has
significant overhead for smaller objects.
The code was designed for little-endian compilers, and will give
different results on big-endian compilers, but the hash quality on
those compilers is probably just as good.
SpookyHash version 2 passes all of Reini Urban's SMHasher tests, and
has one bad seed only when reduced to a 32-bit output.
Its only potential problem is undefined behavior if the key is
misaligned.

## The `stdlib_hash_32bit` module

### Overview of the module

Thirty two bit hash functions are primarily useful for generating hash
codes and hash indices for hash tables.
They tend to be less useful for generating checksums, which generally
benefit from having a larger number of bits.
The `stdlib_hash_32bit` module defines five public overloaded
32-bit hash code functions, `FNV_1`, `FNV-1A`, `nmhash32`, `nmhash32x`
and `water_hash`, two scalar hash functions, `fibonacci_hash` and
`universal_mult_hash`, four seed generators, `odd_random_integer` for
`universal_mult_hash`, and `new_nmhash32_seed`, `new_nmhash32x_seed`,
and `new_water_hash_seed`, for their respective hash code
functions. It also defines the integer kind constant, `int_hash`, and
a logical constant, `little_endian`, used to deal with one aspect of
the machine dependence of the hash codes.

### The `int_hash` parameter

It is necessary to define the kind of integer used to return the hash
code.
As `stdlib_hash_32bit` deals exclusively with 32-bit hash codes,
`int_hash` is an alias for the integer kind `int32`.

### The `little_endian` parameter

In implementing hash functions it is sometimes necessary to know the
"endianess" of the compiler's integers. To this end the
`stdlib_hash_32bit` module defines the logical parameter
`little_endian` that, if true, indicates that the compiler has
little-endian integers, and that if false indicates that the integers
are big-endian.

### Specifications of the `stdlib_hash_32bit` procedures

#### `fibonacci_hash` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 32-bit integer. This is useful
in mapping hash codes into small arrays.

##### Syntax

`code = ` [[stdlib_hash_32bit:fibonacci_hash]] `( key, nbits )`

##### Class

Elemental function

##### Arguments

`key`: Shall be a scalar integer expression of kind `int32`. It is an
`intent(in)` argument.

`nbits` Shall be a scalar default integer expression with `0 < nbits <
32`. It is an `intent(in)` argument.

##### Result

The result is an integer of kind `int32` with at most the lowest
`nbits` nonzero, mapping to a range 0 to `nbits-1`.

##### Note

`fibonacci_hash` is an implementation of the Fibonacci Hash of Donald
E. Knuth. It multiplies the `key` by the odd valued approximation to
`2**32/phi`, where `phi` is the golden ratio 1.618..., and returns the
`nbits` upper bits of the product as the lowest bits of the result.

##### Example

```fortran
{!example/hash_procedures/example_fibonacci_hash.f90!}
```

#### `fnv_1_hash`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32-bit hash code from a rank-1 integer array or a default
character string.

##### Syntax

`code = ` [[stdlib_hash_32bit:fnv_1_hash]] `( key )`

##### Class

Pure/elemental function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank-1 integer array expression of kind `int8`, `int16`,
`int32`, or `int64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int32`.

##### Note

`fnv_1_hash` is an implementation of the original FNV-1 hash code of Glenn
Fowler, Landon Curt Noll, and Phong Vo.
It differs from typical implementations in that it also encodes the
size of the structure in the hash code.
This code is relatively fast on short keys, and is small enough that it
will often be retained in the instruction cache if hashing is
intermittent.
As a result it should give good performance for typical hash table
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.


##### Example

```fortran
{!example/hash_procedures/example_fnv_1_hash.f90!}
```


#### `fnv_1a_hash`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32-bit hash code from a rank-1 integer array or a default
character string.

##### Syntax

`code = ` [[stdlib_hash_32bit:fnv_1a_hash]] `( key )`

##### Class

Pure/elemental function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank-1 integer array expression of kind `int8`, `int16`,
`int32`, or `int64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int32`.

##### Note

`fnv_1a_hash` is an implementation of the alternative FNV-1a hash code of
Glenn Fowler, Landon Curt Noll, and Phong Vo.
It differs from typical implementations in that it also encodes the
size of the structure in the hash code.
This code is relatively fast on short keys, and is small enough that it
will often be retained in the instruction cache if hashing is
intermittent.
As a result it should give good performance for typical hash table
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.

##### Example

```fortran
{!example/hash_procedures/example_fnv_1a_hash.f90!}
```


#### `new_nmhash32_seed`- returns a valid input seed for `nmhash32`

##### Status

Experimental

##### Description

Calculates a 32-bit "random" integer that is believed to be a valid
seed for `nmhash32` and is also different from the input seed.

##### Syntax

`call ` [[stdlib_hash_32bit:new_nmhash32_seed]] `( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `int32`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `nmhash32`, but if any are
identified the procedure will be revised so that they cannot be
returned. This subroutine uses Fortran's intrinsic
 `random_number` and the values returned can be changed by calling the
 intrinsic `random_init`.

##### Example

See the example for `nmhash32`.


#### `new_nmhash32x_seed`- returns a valid input seed for `nmhash32x`

##### Status

Experimental

##### Description

Calculates a 32-bit "random" integer that is believed to be a valid
seed for `nmhash32x` and is also different from the input seed.

##### Syntax

`call ` [[stdlib_hash_32bit:new_nmhash32x_seed]] `( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `int32`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `nmhash32x`, but if any are
identified the procedure will be revised so that they cannot be
returned. This subroutine uses Fortran's intrinsic
 `random_number` and the values returned can be changed by calling the
 intrinsic `random_init`.

##### Example

See the example for `nmhash32x`.


#### `new_water_hash_seed`- returns a valid input seed for `water_hash`

##### Status

Experimental

##### Description

Calculates a 64-bit "random" integer that is believed to be a valid
seed for `water_hash` and is also different from the input seed.

##### Syntax

`call ` [[stdlib_hash_32bit:new_water_hash_seed]] `( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `int64`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `water_hash`, but if any
are identified the procedure will be revised so that they cannot be
returned. This subroutine uses Fortran's intrinsic
 `random_number` and the values returned can be changed by calling the
 intrinsic `random_init`.


##### Example

See the example for `water_hash`.


#### `nmhash32`- calculates a hash code from a key and a seed

##### Status

Experimental

##### Description

Calculates a 32-bit hash code from a rank-1 integer array or a default
character string, and the input `seed`.

##### Syntax

`code = ` [[stdlib_hash_32bit:nmhash32]] `( key, seed )`

##### Class

Pure/elemental function

##### Arguments

`key`: Shall be a deferred length default character scalar expression
or a rank-1 integer array expression of kind `int8`, `int16`,
`int32`, or `int64`.
It is an `intent(in)` argument.

`seed`: shall be an integer scalar expression of kind `int32`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int32`.

##### Note

`nmhash32` is an implementation of the `nmhash32` hash code of
James Z. M. Gao.
This code has good, but not great, performance on long keys, poorer
performance on short keys.
As a result it should give fair performance for typical hash table
applications.
This code passes the SMHasher tests, and has no known bad seeds.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.

##### Example

```fortran
{!example/hash_procedures/example_nmhash32.f90!}
```


#### `nmhash32x`- calculates a hash code from a key and a seed

##### Status

Experimental

##### Description

Calculates a 32-bit hash code from a rank-1 integer array or a default
character string, and the input `seed`.

##### Syntax

`code = ` [[stdlib_hash_32bit:nmhash32x]] `( key, seed )`

##### Class

Pure/elemental function

##### Arguments

`key`: Shall be a deferred length default character scalar expression
or a rank-1 integer array expression of kind `int8`, `int16`,
`int32`, or `int64`.
It is an `intent(in)` argument.

`seed`: shall be an integer scalar expression of kind `int32`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int32`.

##### Note

`nmhash32x` is an implementation of the `nmhash32x` hash code of
James Z. M. Gao.
This code has good, but not great, performance on long keys, poorer
performance on short keys.
As a result it should give fair performance for typical hash table
applications.
This code passes the SMHasher tests, and has no known bad seeds.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.

##### Example

```fortran
{!example/hash_procedures/example_nmhash32x.f90!}
```

#### `odd_random_integer` - returns an odd integer

##### Status

Experimental

##### Description

Returns a random 32-bit integer distributed uniformly over the odd values.

##### Syntax

`call ` [[stdlib_hash_32bit:odd_random_integer]] `( harvest )`

##### Class

Subroutine

##### Argument

`harvest`: Shall be a scalar integer variable of kind `int32`. It is
an `intent(out)` argument.

##### Note

`odd_random_integer` is intended to generate seeds for
 `universal_mult_hash`. `odd_random_integer` uses Fortran's intrinsic
 `random_number` and the values returned can be changed by calling the
 intrinsic `random_init`.

##### Example

See `universal_mult_hash`.


#### `universal_mult_hash` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 32-bit integer. This is useful
in mapping a hash value to a range 0 to `2**nbits-1`.

##### Syntax

`code = ` [[stdlib_hash_32bit:universal_mult_hash]] `( key, seed, nbits )`

##### Class

Elemental function

##### Arguments

`key`: Shall be a scalar integer expression of kind `int32`. It is an
`intent(in)` argument.

`seed`: Shall be a scalar integer expression of kind `int32`. It is an
`intent(in)` argument. It must have an odd value.

`nbits` Shall be a scalar default integer expression with `0 < nbits <
32`. It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int32` with at most the lowest
`nbits` nonzero.

##### Note

`universal_mult_hash` is an implementation of the Universal
Multiplicative Hash of M. Dietzfelbinger, et al.
It multiplies the `key` by `seed`, and returns the
`nbits` upper bits of the product as the lowest bits of the result.

##### Example

```fortran
{!example/hash_procedures/example_universal_mult_hash.f90!}
```

#### `water_hash`- calculates a hash code from a key and a seed

##### Status

Experimental

##### Description

Calculates a 32-bit hash code from a rank-1 integer array or a default
character string, and the input `seed`.

##### Syntax

`code = ` [[stdlib_hash_32bit:water_hash]] `( key, seed )`

##### Class

Pure/elemental function

##### Arguments

`key`: Shall be a deferred length default character scalar expression
or a rank-1 integer array expression of kind `int8`, `int16`,
`int32`, or `int64`.
It is an `intent(in)` argument.

`seed`: shall be an integer scalar expression of kind `int64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int32`.

##### Note

`water_hash` is an implementation of the `waterhash` hash code of
Tommy Ettinger.
This code has excellent performance on long keys, and good performance
on short keys.
As a result it should give reasonable performance for typical hash
table applications.
This code passes the SMHasher tests.
The `waterhash` is based on the `wyhash` of Wang Yi.
While `wyhash` has a number of bad seeds, where randomization of the
output is poor,
so far testing has not found any bad seeds for `waterhash`.
It can have undefined behavior if the key is not word aligned,
i.e. some computer processors can only process a given size integer if
the address of the integer is a multiple of the integer size.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.

##### Example

```fortran
{!example/hash_procedures/example_water_hash.f90!}
```

## The `stdlib_hash_64bit` module

### Overview of the module

Sixty-four bit hash functions are generally overkill for hash table
applications, and are primarily useful for check sums and related
applications.
As checksums often have to deal with extremely large files or
directories, it is often useful to use incremental hashing as well as
direct hashing, so 64-bit and higher hash algorithms often provide
multiple implementations. The current module, for simplicity of API,
doesn't provide any incremental hashes.
The `stdlib_hash_64bit` module defines several public
overloaded 64-bit hash procedures, `FNV_1`, `FNV-1A`,
`pengy_hash`, and `spooky_hash`, two scalar hash functions,
`fibonacci_hash` and
`universal_mult_hash`, a seed generator, `odd_random_integer`, for the
`universal_mult_hash`, and two seed generators, `new_pengy_hash_seed`
and `new_spooky_hash_seed` for their respective hash functions. It
also defines the integer kind constant, `int_hash`, used to specify
the kind of the hash function results, and a logical constant,
`little_endian`, used to deal with one aspect of the machine
dependence of the hash codes.
Note that while SpookyHash can be used as a sixty-four bit hash
algorithm, its algorithms actually returns two element integer arrays
of kind `int64`, so it can also be used as a 128 bit hash.

### The `int_hash` parameters

It is necessary to define the kind of integer used to return the hash
code.
As `stdlib_haash_64bit` deals exclusively with 64-bit hash codes,
`int_hash` is an alias for the integer kind `int64`.

### The `little_endian` parameter

In implementing hash functions it is sometimes necessary to know the
"endianess" of the compiler's integers. To this end the
`stdlib_hash_64bit` module defines the logical parameter
`little_endian` that if true indicates that the compiler has
little-endian integers, and that if false indicates that the integers
are big-endian.


### Specifications of the `stdlib_hash_64bit` procedures

#### `fibonacci_hash` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 64-bit integer. This is useful
in mapping hash codes into small arrays.

##### Syntax

`code = ` [[stdlib_hash_64bit:fibonacci_hash]] `( key, nbits )`

##### Class

Elemental function

##### Arguments

`key`: Shall be a scalar integer expression of kind `int64`. It is an
`intent(in)` argument.

`nbits` Shall be a scalar default integer expression with `0 < nbits <
64`. It is an `intent(in)` argument.

##### Result

The result is an integer of kind `int64` with at most the lowest
`nbits` nonzero, mapping to a range 0 to `nbits-1`.

##### Note

`fibonacci_hash` is an implementation of the Fibonacci Hash of Donald
E. Knuth. It multiplies the `key` by the odd valued approximation to
`2**64/phi`, where `phi` is the golden ratio 1.618..., and returns the
`nbits` upper bits of the product as the lowest bits of the result.

##### Example

```fortran
{!example/hash_procedures/example_fibonacci_hash_64.f90!}
```

#### `FNV_1`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 64-bit hash code from a rank-1 integer array or a default
character string.

##### Syntax

`code = ` [[stdlib_hash_64bit:fnv_1_hash]] `( key )`

##### Class

Pure/elemental function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank-1 integer array expression of kind `int8`, `int16`,
`int32`, or `int64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int64`.

##### Note

`FNV_1` is an implementation of the original FNV-1 hash code of Glenn
Fowler, Landon Curt Noll, and Phong Vo.
It differs from typical implementations in that it also ecodes the
size of the structure in the hash code.
This code is relatively fast on short keys, and is small enough that it
will often be retained in the instruction cache if hashing is
intermittent.
As a result it should give good performance for typical hash table
applications, although it is rare for them to need 64 bits.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.


##### Example

```fortran
{!example/hash_procedures/example_fnv_1_hash_64.f90!}
```


#### `FNV_1A`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 64-bit hash code from a rank-1 integer array or a default
character string.

##### Syntax

`code = ` [[stdlib_hash_64bit:fnv_1a_hash]] `( key )`

##### Class

Pure/elemental function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank-1 integer array expression of kind `int8`, `int16`,
`int32`, or `int64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `int32`.

##### Note

`FNV_1A` is an implementation of the alternative FNV-1a hash code of
Glenn Fowler, Landon Curt Noll, and Phong Vo.
It differs from typical implementations in that it also encodes the
size of the structure in the hash code.
This code is relatively fast on short keys, and is small enough that it
will often be retained in the instruction cache if hashing is
intermittent.
As a result it should give good performance for typical hash table
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.

##### Example

```fortran
{!example/hash_procedures/example_fnv_1a_hash_64.f90!}
```


#### `new_pengy_hash_seed`- returns a valid input seed for `pengy_hash`

##### Status

Experimental

##### Description

Calculates a 32-bit "random" integer that is believed to be a valid
seed for `pengy_hash` and is also different from the input seed.

##### Syntax

`call ` [[stdlib_hash_64bit:new_pengy_hash_seed]] `( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `int32`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `pengy_hash`, but if any are
identified the procedure will be revised so that they cannot be
returned.  This subroutine uses Fortran's intrinsic
 `random_number` and the values returned can be changed by calling the
 intrinsic `random_init`.

##### Example

See the example for `pengy_hash`.


#### `new_spooky_hash_seed`- returns a valid input seed for `spooky_hash`

##### Status

Experimental

##### Description

Calculates a 32-bit two element vector of "random" integer values that
is believed to be a valid seed for `spooky_hash` and is also different
from the input seed.

##### Syntax

`call ` [[stdlib_hash_64bit:new_spooky_hash_seed]] `( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined two element integer vector variable of kind
`int32`. It is an `intent(inout)` argument. On input `seed` should be
defined, and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `spooky_hash`, but if any are
identified the procedure will be revised so that they cannot be
returned.  This subroutine uses Fortran's intrinsic
 `random_number` and the values returned can be changed by calling the
 intrinsic `random_init`.

##### Example

See the example for `spooky_hash`.


#### `odd_random_integer` - returns odd integer

##### Status

Experimental

##### Description

Returns a random 64-bit integer distributed uniformly over the odd values.

##### Syntax

`call ` [[stdlib_hash_64bit:odd_random_integer]] `( harvest )`

##### Class

Subroutine

##### Argument

`harvest`: Shall be an integer of kind `int64`. It is an `intent(out)`
argument.

##### Note

`odd_random_integer` is intended to generate seeds for
 `universal_mult_hash`. `odd_random_integer` uses Fortran's intrinsic
 `random_number` and the values returned can be changed by calling the
 intrinsic `random_init`.

##### Example

See `universal_mult_hash`.


#### `pengy_hash` - maps a character string or integer vector to an integer

##### Status

Experimental

##### Description

Maps a character string or integer vector to a 64-bit integer whose
value also depends on a scalar 32-bit integer, `seed`.

##### Syntax

`code = ` [[stdlib_hash_64bit:pengy_hash]] `( key, seed )`

#####  Class

Pure/elemental function

##### Arguments

`key`: shall be a scalar  expression of type default character or a
rank-1 integer vector expression of kind `int8`, `int16`, `int32`, or
`int64`. It is an `intent(in)` argument.

`seed`: shall be an integer expression of kind `int64`. It is
an `intent(in)` argument.

##### Result

The result is an integer of kind `int64`.

##### Note

`pengy_hash` is an implementation of the 64-bit `pengyhash` of Alberto
Fajardo. The hash has acceptable performance on small keys, and good
performance on long keys. It passes all the SMHasher tests, and has
no known bad seeds.
It is a *pure* function for integer arrays, and an *elemental*
function for character strings.

##### Example

```fortran
{!example/hash_procedures/example_pengy_hash.f90!}
```


#### `spooky_hash` - maps a character string or integer vector to an integer

##### Status

Experimental

##### Description

Maps a character string or integer vector to a 64-bit integer whose
value also depends on a two element vector,  `seed`.

##### Syntax

`code = ` [[stdlib_hash_64bit:spooky_hash]] `( key, seed )`

#####  Class

Function

##### Arguments

`key`: shall be a scalar of type default character expression or a
rank-1 integer vector expression of kind `int8`, `int16`, `int32`, or
`int64`. It is an `intent(in)` argument.

`seed`: shall be a two element integer vector expression of kind
`int64`. It is an `intent(in)` argument.

##### Result

The result is a two element integer vector of kind `int64`.

##### Note

`spooky_hash` is an implementation of the 64-bit version 2 of
SpookyHash of Bob Jenkins. The code was designed for little-endian
compilers. The output is different on big-endian compilers, but still
probably as good quality. It is often used as a 64-bit hash using the
first element of the returned value, but can be used as a 128 bit
hash. This version of `spooky_hash` has good performance on small keys
and excellent performance on long keys. It passes all the SMHasher tests
and has no known bad seeds.

##### Example

```fortran
{!example/hash_procedures/example_spooky_hash.f90!}
```

#### `universal_mult_hash` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 64-bit integer. This is useful
in mapping a hash value to a range 0 to `2**nbits-1`.

##### Syntax

`code = ` [[stdlib_hash_64bit:universal_mult_hash]] `( key, seed, nbits )`

##### Class

Elemental function

##### Arguments

`key`: Shall be an integer of kind `int64`. It is an `intent(in)`
argument.

`seed`: Shall be an integer of kind `int64`. It is an `intent(in)`
argument. It should be an odd value.

`nbits` Shall be a default integer with `0 < nbits < 64`. It is an
`intent(in)` argument.  It must be an odd integer.

##### Result

The result is an integer of kind `int64` with at most the lowest
`nbits` nonzero.

##### Note

`universal_mult_hash` is an implementation of the Universal
Multiplicative Hash of M. Dietzfelbinger, et al.
It multiplies the `key` by `seed`, and returns the
`nbits` upper bits of the product as the lowest bits of the result.

##### Example


```fortran
{!example/hash_procedures/example_universal_mult_hash_64.f90!}
```


### Test Codes

The Fortran Standard Library provides two categories of test
codes. One category is tests of the relative performance of the
various hash functions. The other is a comparison of the outputs of
the Fortran hash functions, with the outputs of the C and C++ hash
procedures that are the inspiration for the Fortran hash functions.

In the `test/hash_functions_perf` subdirectory, the Fortran Standard
Library provides two performance test codes for
the hash functions of `stdlib_hash_32bit` and
`stdlib_hash_64bit`, `test_32_bit_hash_performance` and
`test_64_bit_hash_performance` respectively. These are primarily set
up to test runtime performance of the functions. They take a sample of
`2**18` integers of kind `int8` and break it up into vectors of size
1, 2, 4, 8, 16, 64,  256, and 1024 elements, yielding `2**18`,
`2**17`, `2**16`, `2**15`, `2**14`, `2**12`, `2**10`, and `2**8`
vectors respectively. These are then processed by the hash functions
4 times, and the time for processing is reported. Testing so far has
been on a MacBook Pro with a 2.3 GHz Quad-Core Intel Core i5 and 8 GB
2133 MHz LPDDR3 of RAM, using GNU Fortran (GCC) 11.1.0 to compile the
code. The results for `test_32_bit_hash_performance` is given by the
following table:

| Algorithm  | Key Size Bytes  | Key #      | Time (s) |
|------------|:---------:|:----------:|:--------:|
|     FNV-1  |       1   |    1048576 |  0.02949 |
|     FNV-1  |       2   |     524288 |  0.02361 |
|     FNV-1  |       4   |     262144 |  0.02016 |
|     FNV-1  |       8   |     131072 |  0.01806 |
|     FNV-1  |      16   |      65536 |  0.01867 |
|     FNV-1  |      64   |      16384 |  0.01717 |
|     FNV-1  |     256   |       4096 |  0.01759 |
|     FNV-1  |    1024   |       1024 |  0.01659 |
|    FNV-1a  |       1   |    1048576 |  0.02897 |
|    FNV-1a  |       2   |     524288 |  0.02472 |
|    FNV-1a  |       4   |     262144 |  0.02025 |
|    FNV-1a  |       8   |     131072 |  0.01901 |
|    FNV-1a  |      16   |      65536 |  0.01898 |
|    FNV-1a  |      64   |      16384 |  0.01784 |
|    FNV-1a  |     256   |       4096 |  0.01723 |
|    FNV-1a  |    1024   |       1024 |  0.01673 |
|  nmhash32  |       1   |    1048576 |  0.31092 |
|  nmhash32  |       2   |     524288 |  0.16230 |
|  nmhash32  |       4   |     262144 |  0.07815 |
|  nmhash32  |       8   |     131072 |  0.04176 |
|  nmhash32  |      16   |      65536 |  0.09261 |
|  nmhash32  |      64   |      16384 |  0.04587 |
|  nmhash32  |     256   |       4096 |  0.07238 |
|  nmhash32  |    1024   |       1024 |  0.07263 |
| nmhash32x  |       1   |    1048576 |  0.04294 |
| nmhash32x  |       2   |     524288 |  0.02937 |
| nmhash32x  |       4   |     262144 |  0.01096 |
| nmhash32x  |       8   |     131072 |  0.00911 |
| nmhash32x  |      16   |      65536 |  0.01291 |
| nmhash32x  |      64   |      16384 |  0.00859 |
| nmhash32x  |     256   |       4096 |  0.07373 |
| nmhash32x  |    1024   |       1024 |  0.07618 |
|     water  |       1   |    1048576 |  0.12560 |
|     water  |       2   |     524288 |  0.06302 |
|     water  |       4   |     262144 |  0.04020 |
|     water  |       8   |     131072 |  0.01999 |
|     water  |      16   |      65536 |  0.01459 |
|     water  |      64   |      16384 |  0.00923 |
|     water  |     256   |       4096 |  0.00816 |
|     water  |    1024   |       1024 |  0.00792 |

while for `test_64_bit_hash_performance` the results are:

| Algorithm  | Key Size Bytes | Key #      | Time (s) |
|------------|:---------:|:----------:|:--------:|
|     FNV-1  |       1   |    1048576 |  0.02981 |
|     FNV-1  |       2   |     524288 |  0.02697 |
|     FNV-1  |       4   |     262144 |  0.02275 |
|     FNV-1  |       8   |     131072 |  0.02431 |
|     FNV-1  |      16   |      65536 |  0.02158 |
|     FNV-1  |      64   |      16384 |  0.02007 |
|     FNV-1  |     256   |       4096 |  0.01932 |
|     FNV-1  |    1024   |       1024 |  0.02089 |
|    FNV-1a  |       1   |    1048576 |  0.03226 |
|    FNV-1a  |       2   |     524288 |  0.03076 |
|    FNV-1a  |       4   |     262144 |  0.02359 |
|    FNV-1a  |       8   |     131072 |  0.02542 |
|    FNV-1a  |      16   |      65536 |  0.02364 |
|    FNV-1a  |      64   |      16384 |  0.02130 |
|    FNV-1a  |     256   |       4096 |  0.01962 |
|    FNV-1a  |    1024   |       1024 |  0.01966 |
|     Pengy  |       1   |    1048576 |  0.24294 |
|     Pengy  |       2   |     524288 |  0.12066 |
|     Pengy  |       4   |     262144 |  0.06205 |
|     Pengy  |       8   |     131072 |  0.03138 |
|     Pengy  |      16   |      65536 |  0.01608 |
|     Pengy  |      64   |      16384 |  0.00669 |
|     Pengy  |     256   |       4096 |  0.00387 |
|     Pengy  |    1024   |       1024 |  0.00295 |
|    Spooky  |       1   |    1048576 |  0.11920 |
|    Spooky  |       2   |     524288 |  0.07478 |
|    Spooky  |       4   |     262144 |  0.03185 |
|    Spooky  |       8   |     131072 |  0.01468 |
|    Spooky  |      16   |      65536 |  0.01503 |
|    Spooky  |      64   |      16384 |  0.00440 |
|    Spooky  |     256   |       4096 |  0.00290 |
|    Spooky  |    1024   |       1024 |  0.00177 |

As the tested function will typically reside in the instruction cache
these results do not include the costs of reloading the procedure if
hashing is intermittent. If hashing is intermittent then that can more
severely impact the performance of  `nmhash32`, `nmhash32x`,
`water_hash`, `pengy_hash`, and `spooky_hash` relative to
`fnv_1_hash` and `fnv_1a_hash`.

In the `test/hash_functions` subdirectory, the Fortran
Standard Library contains codes to test the validity of
the Fortran codes against the original C and C++ codes. It consists of one
executable `test_hash_functions` that
1) generates a random sequence of 2048
integers of kind `int8`, and stores that sequence in the binary file
`key_array.bin`;
2) reads the values in `key_array.bin`, and, for each complicated C/C++-coded
hash procedure, generates a corresponding binary file containing 2049 hash
values generated from the values in `key_array.bin`., and
3) reads the binary files, and, for each complicated C/C++-coded hash procedure,
compares the contents of the binary file
with the results of calculating hash values using the corresponding
Fortran hash procedure on the same keys.
