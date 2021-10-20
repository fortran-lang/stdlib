---
title: Hash codes
---

# The `stdlib_32_bit_hash_functions` and `stdlib_64_bit_hash_functions` modules

(TOC)

## Overview of hash functions

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
that can be used as an index, termed a hash index, to a rank one
array, often termed a hash table.
This mapping will be known as a scalar hash.
The use of a hash table reduces the number of hash codes that need to
be compared, further improving performance.
A hash function can also be used to generate a checksum to verify that
data has not changed.
The Fortran Standard Library therefore provides procedures to compute
hash codes and scalar hashes, and derived types implementing hash
tables.
This document only discusses the hash codes and scalar hashes in the
library.

## Licensing

The Fortran Standard Library is distributed under the MIT License.
However components of the library may be based on code with additional
licensing restrictions. In particular, the hash codes are often based
on algorithms with additional restrictions on distribution.
The algorithms with such restrictions (`Fibonacci Hash`, `Universal
Multiplicative Hash`,
`FNV-1 Hash`, `FNV-1A Hash`, `nmhash32`, `nmhash32x`, `waterhash`,
`pengyhash` and `SpookyHash`) are discussed below.

`FIBONACCI_HASH` is a scalar hash. It is an implementation in Fortran
2008 and signed two's complement integers of the Fibonacci Hash
described in D. E. Knuth, "The Art of
Computer Programming, Second Edition, Volume 3, Sorting and
Searching", Addison-Wesley, Upper Saddle River, NJ,
pp. 517-518, 1998. The algorithms in that source are considered public
domain.

`UNIVERSAL_MULT_HASH` is a scalar hash. It is an implementation in
Fortran 2008 and signed two's complement integers of the
universal multiplicative hash algorithm of M. Dietzfelbinger,
T. Hagerup, J. Katajainen, and M. Penttonen, "A Reliable Randomized
Algorithm for the Closest-Pair Problem," J. Algorithms, Vol. 25,
No. 1, Oct. 1997, pp. 19-51. Because of its publication in the Journal
of Algorithms, the universal multiplicative hash algorithm is public
domain.

`FNV_1_HASH` and `FNV_1A_HASH` are translations to Fortran 2008 and
signed two's complement integers of the
`FNV-1` and `FNV-1a` hash functions of Glenn Fowler, Landon Curt Noll,
and Phong Vo, that has been released into the public
domain. Permission has been granted, by Landon Curt Noll, for the use
of these algorithms in the Fortran Standard Library. A description of
these functions is available at
<https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function>.
These functions have been modified from their normal forms to also
encode the structure size in the output hash.

Similarly `SPOOKY_HASH` and associated procedures are translations to
Fortran 2008 and signed two's complement integers of the unsigned 64
bit version 2 `SpookyHash` functions of Bob
Jenkins <https://burtleburtle.net/bob/hash/spooky.html> to signed 64
bit operations. Version 2 was chosen over version 1 as it has better
performance and fewer bad seeds
Bob Jenkins has also put this code in the public
domain and has given permission to treat this code as public domain in
the USA, provided the code can be used under other licenses and he is
given appropriate credit.

`NMHASH32` and `NMHASH32x` are translations to Fortran 2008 and signed
two's complement integers of the unsigned 32 bit
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

`WATER_HASH` is a translation to Fortran 2008  and signed two's
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

`PENGY_HASH` is a translation to Fortran 2008 and signed two's
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


## The hash codes modules

### Overview of the modules

The Standard Library provides two modules implementing hash
functions and scalar hashes.
The `stdlib_32_bit_hash_functions` module provides procedures to
compute 32 bit integer hash codes and a scalar hash. 
The hash codes are useful for tables of up to `2**15` entries, and
for keys with a few hundred elements.
The `stdlib_64_bit_hash_functions` module provides hash procedures to
compute 64 bit integer hash codes and a scalar hash.
The hash codes are useful for tables of up to `2**30` entries, and
for keys with a few thousand elements.
While one of the codes in `stdlib_64_bit_hash_functions`,
`SPSOOKY_HASH`, can also be used to calculate 128 bit hash codes, none
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
functions that are believed to be non-cryptographic, with
implementations available in the public domain.

There are a number of algorithms available for the computation of
non-cryptographic 32 and 64 bit hash codes that differ in their
computational complexity,
their relative performance on different size keys, and the
expected uniqueness (randomness) of the resulting hash codes.
Their relative performance in the analysis of text, in particular,
can depend on the processor, character set, language, and content.
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
Instead hash functions are defined for some of the more common objects
that are sufficiently complicated that a direct comparison is costly
and common enough that a general procedure is useful:
character strings and rank one arrays of integers.
Other objects can, in principle, be hashed by using `transfer` to
map their contents to an integer array, typically one of  kind `INT8`.
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
In the other, the unsigned integers may be replaced directly by
the corresponding signed integers, but
otherwise not modifying the the code logic.
The first should be standard conforming on current processors, but
is more computationally intensive unless the processors recognize 
underlying idioms that are rarely used in Fortran codes. The second is
not standard conforming as bit operations involving the sign are
undefined,
but should yield equivalent results with fewer operations on
processors with two's complement integers that do not trap on over
or under flow. The codes currently use the second method.

In order to compile the hash function modules, the processors must
implement much of Fortran 2003, and selected components of Fortran
2008: submodules, 64 bit integers, and some bit intrinsics.
The main limitation on valid processors is whether they
implement the submodules enhancement of Fortran 2008.
In order to properly run the hash functions, the compilers must
use two's complement integers, and be able to execute them with
wraparound semantics and no integer overflow exceptions.
Current Fortran 2003+ processors solely use two's complement
integers, and appear to be able to turn off overflow detection,
so the modules use signed integer arithmetic. For that reason
trapping on signed arithmetic must be disabled. The command line
flags to disable overflow detection for processors implementing
submodules are summarized in the table below.
Note that FLANG, gfortran, ifort, and NAG all default to
integer overflow wrapping.

|Processor|Legal flag|Illegal flag|Default|
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
The *seed* can be either a scalar or a two element array.
Some of the hash functions have alternatives that allow incremental
hashing. 

|Algorithm|Seed|Result|
|---------|----|------|
|FNV-1|None|32 or 64 bit integer|
|FNV-1a|None|32 or 64 bit integer|
|nmhash32 |32 bit scalar integer|32 bit integer|
|nmhash32x |32 bit scalar integer|32 bit integer|
|pengyhash |32 bit scalar integer|64 bit integer|
|Spooky Hash|64 bit two element vector|64 bit two element vector|
|waterhash|64 bit scalar integer|32 bit integer|

The hash function modules each provide at least five algorithms for
hash functions: two optimized for small (< 32 `INT8` integer elements)
keys, and three optimized for large (> 100 `INT8` integer elements)
keys.
The core implementation for each algorithm is for keys that are
vectors of `INT8` integers.
These core implementations are then used in wrappers for keys
that are vectors of `INT16`, `INT32` and `INT64` integers, or default
character strings, in the expectation that inlining will eliminate the
overhead of transferring the other keys to `INT8` integer vectors.

The `stdlib_32_bit_hash_functions` module provides
implementations of five hash code algorithms:
the *FNV_1* and *FNV_1A* variants of Glenn Fowler, 
Landon Curt Noll, and Kiem-Phong Vo;
the *nmhash32* and *nmhash32x* of James Z. M. Gao;
and the *waterhash*  of Tommy Ettinger.
The detailed implementation of each algorithm is handled in a separate
submodule: `stdlib_32_bit_fnv_hashes`,
`stdlib_32_bit_nmhashes`, and `stdlib_32_bit_water_hashes`,
respectively. The `nmhash32`, `nmhash32x`, and `waterhash` algorithms
require seeds. The submodules provide separate seed generators
for each algorithm.
The module itself
implements two scalar hash functions, `FIBONACCI_HASH` and
`UNIVERSAL_MULT_HASH`. 
It also implements the subroutine, `ODD_RANDOM_INTEGER`, for
generating seeds for `UNIVERSAL_MULT_HASH`.
All assume a two's complement sign bit, and no out of
range checks.

The `stdlib_64_bit_hash_functions` module also provides
implementations of four hash code algorithms:
the *FNV_1* and *FNV_1A* variants of Glenn Fowler, 
Landon Curt Noll, and Kiem-Phong Vo;
the *pengynash* of Alberto Fajardo;
and the *SpookyHash*  of Bob Jenkins.
The detailed implementation of each algorithm is handled in a separate
submodule: `stdlib_64_bit_fnv_hashes`,
`stdlib_64_bit_pengy_hashes`, and `stdlib_64_bit_spooky_hashes`,
respectively.
The `pengyhash`, and `Spooky Hash` algorithms
require seeds. The submodules provide separate seed generators
for each algorithm.
The module itself implements two scalar hash functions,
`FIBONACCI_HASH` and `UNIVERSAL_MULT_HASH`.
It also implements the subroutine, `ODD_RANDOM_INTEGER`, for
generating seeds for `UNIVERSAL_MULT_HASH`.
All assume a two's complement sign bit, and no out of
range checks.

The `stdlib_32_bit_fnv_hashes` and `stdlib_64_bits_fnv_hashes`
submodules each provide implementations of ths FNV-1  and FNV-1A
algorithms in the form of two separate overloaded functions: `FNV_1`
and `FNV_1A`.
The FNV-1 and FNV-2 algorithms differ in their order of the
multiplication and exclusive or operations.
They differ from their normal implementation in that they also
encode the structure size in  the hash code.
The 32 and 64 bit algorithms differ in their initial offsets and in
their multiplicative constants.
Analysis suggests that `FNV_1A` should be better at randomizing the
input, but tests with hash tables show negligible difference.
These algorithms have the reputation of being particularly useful for
small byte strings, i.e, strings of less than 32 bytes.
While they do not at all perform well on the SMHasher test suite,
usage indicates that that that this has little impact on the
performance of small hash tables, and the small size of the functions
allows their quick loading and retainment in the instruction cache,
givng a performance boost where the hashing is intermittent.
(See the
[SMHasher discussion](https://github.com/rurban/smhasher/README.md)
and S. Richter, V. Alvarez, and J. Dittrich,
["A Seven-Dimensional Analysis of Hashing Methods and its Implications on Query Processing"](https://bigdata.uni-saarland.de/publications/p249-richter.pdf).

The `stdlib_32_bit_nmhashes` submodule provides implementations
of James Z.M. Gao's `nmhash32` and `nmhash32x` algorithms,
version 0.2, 
in the form of the overloaded functions, `NMHASH32` and `NMHASH32X`.
The implementations are based on the scalar versions of Gao's
algorithms and not the vector versions that require access to
the vector instructions of some processors.
Both algorithms perform well on the SMHasher tests, and have no known
bad seeds. The vector versions of both codes perform well on large
keys, with the `nmhash32x` faster on short keys. To provide randomly
generated seeds for the two functions the submodule also defines the
subroutines `NEW_NMHASH32_SEED` and `NEW_NMHASH32X_SEED`. Gao claims
that `NMHASH32X` is significantly faster than `NMHASH32` on short
seeds, but slower on long seeds, but our limited testing so far shows
`NMHASH32X` to be significantly faster on short seeds and slightly
faster on long seeds.

The `stdlib_32_bit_water_hashes` submodule provides implementations
of Tommy Ettinger's `waterhash` algorithm in the form of the overloaded
function, `WATER_HASH`. Water Hash has not been tested by Reini Urban,
but Tommy Ettinger has tested it with Urban's SMHasher and presents
results that shows Water Hash passing all the tests. So far his
testing hasn't found any bad seeds for the algorithm. To provide
randomly generated seeds for the hash function the submodule also
defines the subroutine `NEW_WATER_HASH_SEED`.

The `stdlib_64_bit_pengy_hashes` submodule provides implementations of
Alberto Fajardo's `pengyhash` in the form of the overloaded function,
`PENGY_HASH`. Reini Urban's testing shows that PengyHash passes all
the tests and has no bad seeds.  To provide randomly generated seeds
for the hash function the submodule also defines the subroutine
`NEW_PENGY_HASH_SEED`. 

The `stdlib_64_bit_spooky_hashes` submodule provides implementations
of Bob Jenkins' SpookyHash in the form of the overloaded function,
`SPOOKY_HASH`. Future implementations may provide the SpookyHash
incremental hashing procedures.
SpookyHash is optimized for large objects and should give excellent 
performance for objects greater than about 96 byes, but has
significant overhead for smaller objects.
The code was designed for Little Endian processors, and will give
different results on Big Endian processors, but the hash quality on
those processors is probably just as good.
SpookyHash version 2 passes all of Reini Urban's SMHasher tests, and
has one bad seed only when reduced to a 32 bit output.
Its only potential problem is undefined behavior if the key is
misaligned.

## The `stdlib_32_bit_hash_codes` module

### Overview of the module

Thirty two bit hash functions are primarily useful for generating hash
codes for hash tables.
Checksums generally benefit from having a larger number of bits.
The `stdlib_32_bit_hash_codes` module defines five public overloaded
32 bit hash code functions, `FNV_1`, `FNV-1A`, `NMHASH32`, `NMHASH32x`
and `WATER_HASH`, two scalar hash functions, `FIBONACCI_HASH` and
`UNIVERSAL_MULT_HASH`, four seed generators, `ODD_RANDOM_INTEGER` for
`UNIVERSAL_MULT_HASH`, and `NEW_NMHASH32_SEED`, `NEW_NMHASH32X_SEED`,
and `NEW_WATER_HASH_SEED`, for their respective hash code
functions. It also defines the integer kind constant, `INT_HASH`, and
a logical constant, `LITTLE_ENDIAN`, used to deal with one aspect of
the machine dependence of the hash codes.

### The `INT_HASH` parameter

It is necessary to define the kind of integer used to return the hash
code.
As `stdlib_32_bit_hash_codes` deals exclusively with 32 bit hash codes,
`INT_HASH` is an alias for the integer kind `INT32`.

### The `LITTLE_ENDIAN` parameter

In implementing hash functions it is sometimes necessary to know the
"endianess" of the processor's integers. To this end the
`stdlib_32_bit_hash_codes` module defines the logical parameter
`LITTLE_ENDIAN` that, if true, indicates that the processor has little
endian integers, and that if false indicates that the integers are big
endian.

### Specifications of the `stdlib_32_bit_hash_codes` procedures

#### `FIBONACCI_HASH` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 32 bit integer.

##### Syntax

`code = [[stdlib_32_bit_hash_codes:fibonacci_hash]]( key, nbits )`

##### Class

Pure function

##### Arguments

`key`: Shall be a scalar integer expression of kind `INT32`. It is an
`intent(in)` argument.

`nbits` Shall be a scalar default integer expression with `0 < nbits <
32`. It is an `intent(in)` argument.

##### Result

The result is an integer of kind `INT32` with at most the lowest
`nbits` nonzero.

##### Note

`FIBONACCI_HASH` is an implementation of the Fibonacci Hash of Donald
E. Knuth. It multiplies the `KEY` by the odd valued approximation to
`2**32/phi`, where `phi` is the golden ratio 1.618..., and returns the
`NBITS` upper bits of the product as the lowest bits of the result.

##### Example

```fortran
    program demo_fibonacci_hash
      use stdlib_32_bit_hash_codes, only: fibonacci_hash
      use iso_fortran_env, only: int32 
      implicit none
      integer, allocatable :: array1(:)
      integer(int32) :: hash, source
      allocate( array1(0:2**6-1) )
      array1(:) = 0
      source = int(Z'1FFFFFF', int32)
      hash = fibonacci_hash(source, 6)
      azray1(hash) = source
      print *, hash
    end program demo_fibonacci_hash
```

#### `FNV_1_HASH`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from a rank 1 integer array or a default
character string.

##### Syntax

`code = [[stdlib_32_bit_hash_codes:fnv_1_hash]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank 1 integer array expression of kind `INT8`, `INT16`,
`INT32`, or `INT64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT32`.

##### Note

`FNV_1_HASH` is an implementation of the original FNV-1 hash code of Glenn
Fowler, Landon Curt Noll, and Phong Vo.
It differs from typical implementations in that it also ecodes the
size of the structure in the hash code.
This code is relatively fast on short keys, and is small enough that it
will often be retained in the instruction cache if hashing is
intermittent.
As a result it should give good performance for typical hash table
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.


##### Example

```fortran
    program demo_fnv_1_hash
      use stdlib_32_bit_hash_codes, only: fnv_1_hash
      use iso_fortran_env, only: int32 
      implicit none
      integer, allocatable :: array1(:)
      integer(int32) :: hash
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      hash = fnv_1_hash(array1)
      print *, hash
    end program demo_fnv_1_hash
```


#### `FNV_1A_HASH`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from a rank 1 integer array or a default
character string.

##### Syntax

`code = [[stdlib_32_bit_hash_codes:fnv_1a_hash]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank 1 integer array expression of kind `INT8`, `INT16`,
`INT32`, or `INT64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT32`.

##### Note

`FNV_1A_HASH` is an implementation of the alternative FNV-1a hash code of
Glenn Fowler, Landon Curt Noll, and Phong Vo.
It differs from typical implementations in that it also ecodes the
size of the structure in the hash code.
This code is relatively fast on short keys, and is small enough that it
will often be retained in the instruction cache if hashing is
intermittent.
As a result it should give good performance for typical hash table
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.

##### Example

```fortran
    program demo_fnv_1a_hash
      use stdlib_32_bit_hash_codes, only: fnv_1a_hash
      use iso_fortran_env, only: int32 
      implicit none
      integer, allocatable :: array1(:)
      integer(int32) :: hash
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      hash = fnv_1a_hash(array1)
      print *, hash
    end program demo_fnv_1a_hash
```


#### `NEW_NMHASH32_SEED`- returns a valid input seed for `NMHASH32`

##### Status

Experimental

##### Description

Calculates a 32 bit "random" integer that is believed to be a valid
seed for `NMHASH32` and is also different from the input seed.

##### Syntax

`code = call [[stdlib_32_bit_hash_codes:new_nmhash32_seed]]( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `INT32`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `NMHASH32`, but if any are
identified the procedure will be revised so that they cannot be
returned.  This subroutine uses Fortran's intrinsic
 `RANDOM_NUMBER` and the values returned can be changed by calling the
 intrinsic `RANDOM_INIT`.

##### Example

See the example for `NMHASH32`.


#### `NEW_NMHASH32X_SEED`- returns a valid input seed for `NMHASH32X`

##### Status

Experimental

##### Description

Calculates a 32 bit "random" integer that is believed to be a valid
seed for `NMHASH32X` and is also different from the input seed.

##### Syntax

`code = call [[stdlib_32_bit_hash_codes:new_nmhash32x_seed]]( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `INT32`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `NMHASH32X`, but if any are
identified the procedure will be revised so that they cannot be
returned.  This subroutine uses Fortran's intrinsic
 `RANDOM_NUMBER` and the values returned can be changed by calling the
 intrinsic `RANDOM_INIT`.

##### Example

See the example for `NMHASH32X`.


#### `NEW_WATER_HASH_SEED`- returns a valid input seed for `WATER_HASH`

##### Status

Experimental

##### Description

Calculates a 64 bit "random" integer that is believed to be a valid
seed for `WATER_HASH` and is also different from the input seed.

##### Syntax

`code = call [[stdlib_32_bit_hash_codes:new_water_hash_seed]]( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `INT64`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `WATER_HASH`, but if any
are identified the procedure will be revised so that they cannot be
returned. This subroutine uses Fortran's intrinsic
 `RANDOM_NUMBER` and the values returned can be changed by calling the
 intrinsic `RANDOM_INIT`.
 

##### Example

See the example for `WATER_HASH`.


#### `NMHASH32`- calculates a hash code from a key and a seed

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from a rank 1 integer array or a default
character string, and the input `seed`.

##### Syntax

`code = [[stdlib_32_bit_hash_codes:nmhash32]]( key, seed )`

##### Class

Pure function

##### Arguments

`key`: Shall be a deferred length default character scalar expression
or a rank 1 integer array expression of kind `INT8`, `INT16`,
`INT32`, or `INT64`.
It is an `intent(in)` argument.

`seed`: shall be an integer scalar expression of kind `INT32`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT32`.

##### Note

`NMHASH32` is an implementation of the `nmhash32` hash code of
James Z. M. Gao.
This code has good, but not great, performance on long keys, poorer
performance on short keys.
As a result it should give fair performance for typical hash table
applications.
This code passes the SMHasher tests, and has no known bad seeds:

##### Example

```fortran
    program demo_nmhash32
      use stdlib_32_bit_hash_codes, only: nmhash32, &
          new_nmhash32_seed
      use iso_fortran_env, only: int32 
      implicit none
      integer, allocatable :: array1(:)
      integer(int32) :: hash
      integer(int32) :: seed = int(Z'11111111`, int32)
      call new_nmhash32_seed(seed)
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      hash = nmhash32(array1, seed)
      print *, seed, hash
    end program demo_nmhash32
```


#### `NMHASH32X`- calculates a hash code from a key and a seed

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from a rank 1 integer array or a default
character string, and the input `seed`.

##### Syntax

`code = [[stdlib_32_bit_hash_codes:nmhash32x]]( key, seed )`

##### Class

Pure function

##### Arguments

`key`: Shall be a deferred length default character scalar expression
or a rank 1 integer array expression of kind `INT8`, `INT16`,
`INT32`, or `INT64`.
It is an `intent(in)` argument.

`seed`: shall be an integer scalar expression of kind `INT32`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT32`.

##### Note

`NMHASH32X` is an implementation of the `nmhash32x` hash code of
James Z. M. Gao.
This code has good, but not great, performance on long keys, poorer
performance on short keys.
As a result it should give fair performance for typical hash table
applications.
This code passes the SMHasher tests, and has no known bad seeds:

##### Example

```fortran
    program demo_nmhash32x
      use stdlib_32_bit_hash_codes, only: nmhash32x, &
	  new_nmhash32x_seed
      use iso_fortran_env, only: int32 
      implicit none
      integer, allocatable :: array1(:)
      integer(int32) :: hash
      integer(int32) :: seed = int(Z'11111111`, int32)
      call new_nmhash32x_seed(seed)
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      hash = nmhash32x(array1, seed)
      print *, seed, hash
    end program demo_nmhash32x
```

#### `ODD_RANDOM_INTEGER` - returns an odd integer

##### Status

Experimental

##### Description

Returns a random 32 bit integer distributed uniformly over the odd values.

##### Syntax

`call [[stdlib_32_bit_hash_codes:odd_random_integer]]( harvest )`

##### Class

Subroutine

##### Argument

`harvest`: Shall be a scalar integer variable of kind `INT32`. It is
an `intent(out)` argument.

##### Note

`ODD_RANDOM_INTEGER` is intended to generate seeds for
 `UNIVERSAL_MULT_HASH`. `ODD_RANDOM_NUMBER` uses Fortran's intrinsic
 `RANDOM_NUMBER` and the values returned can be changed by calling the
 intrinsic `RANDOM_INIT`.
 
##### Example

See `UNIVERSAL_MULT_HASH`.


#### `UNIVERSAL_MULT_HASH` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 32 bit integer.

##### Syntax

`code = [[stdlib_32_bit_hash_codes:universal_mult_hash]]( key, seed, nbits )`

##### Class

Pure function

##### Arguments

`key`: Shall be a scalar integer expression of kind `INT32`. It is an
`intent(in)` argument.

`seed`: Shall be a scalar integer expression of kind `INT32`. It is an
`intent(in)` argument. It must have an odd value.

`nbits` Shall be a scalar default integer expression with `0 < nbits <
32`. It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT32` with at most the lowest
`nbits` nonzero.

##### Note

`UNIVERSAL_MULT_HASH` is an implementation of the Universal
Multiplicative Hash of M. Dietzfelbinger, et al.
It multiplies the `KEY` by `SEED`, and returns the
`NBITS` upper bits of the product as the lowest bits of the result.

##### Example

```fortran
    program demo_universal_mult_hash
      use stdlib_32_bit_hash_codes, only: odd_random_integer, &
	  universal_mult_hash
      use iso_fortran_env, only: int32 
      implicit none
      integer, allocatable :: array1(:)
      integer(int32) :: hash, i, seed, source
      seed = 0
      allocate( array1(0:2**6-1) )
      do i = 0, 2**6-1
          array(i) = i
      end do
      call odd_random_integer( seed )
      source = int(Z'1FFFFFF', int32)
      hash = universal_mult_hash(source, seed, 6)
      azray1(hash) = source
      print *, seed, hash, array1
    end program demo_odd_random_integer
```

#### `WATER_HASH`- calculates a hash code from a key and a seed

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from a rank 1 integer array or a default
character string, and the input `seed`.

##### Syntax

`code = [[stdlib_32_bit_hash_codes:water_hash]]( key, seed )`

##### Class

Pure function

##### Arguments

`key`: Shall be a deferred length default character scalar expression
or a rank 1 integer array expression of kind `INT8`, `INT16`,
`INT32`, or `INT64`.
It is an `intent(in)` argument.

`seed`: shall be an integer scalar expression of kind `INT64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT32`.

##### Note

`WATER_HASH` is an implementation of the `waterhash` hash code of
Tommy Ettinger.
This code has excellent performance on long keys, and good performance
on short keys.
As a result it should give reasonable performance for typical hash
table applications.
This code passes the SMHasher tests.
The `waterhash` is based on the `wyhash` of Wang Yi.
While `wyhash` has a number of bad seeds, depending on the version,
so far testing has not found any bad seeds for `waterhash`.
It can have undefined behavior if the key is not word aligned. 

##### Example

```fortran
    program demo_water_hash
      use stdlib_32_bit_hash_codes, only: water_hash, &
	  new_water_hash_seed
      use iso_fortran_env, only: int32, int64
      implicit none
      integer, allocatable :: array1(:)
      integer(int32) :: hash
      integer(int64) :: seed = int(Z'11111111`, int64)
      call new_water_hash_seed( seed )
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      hash = water_hash(array1, seed)
      print *, hash, seed
    end program demo_water_hash
```

## The `stdlib_64_bit_hash_codes` module

### Overview of the module

Sixty four bit hash functions are generally overkill for hash table
applications, and are primarily useful for check sums and related
applications.
As checksums often have to deal with extremely large files or
directories, it is often useful to use incremental hashing as well as
direct hashing, so 64 bit and higher hash algorithms often provide
multiple implementations. The current module, for simplicity of API,
doesn't provide any incremental hashes.
The `stdlib_64_bit_hash_codes` module defines several public
overloaded 64 bit hash procedures, `FNV_1`, `FNV-1A`,
`PENGY_HASH`, and `SPOOKY_HASH`, two scalar hash functions,
`FIBONACCI_HASH` and 
`UNIVERSAL_MULT_HASH`, a seed generator, `ODD_RANDOM_INTEGER`, for the
`UNIVERSAL_MULT_HASH`, and two seed generators, `NEW_PENGY_HASH_SEED`
and `NEW_SPOOKY_HASH_SEED` for their respective hash functions. It
also defines the integer kind constant, `INT_HASH`, used to specify
the kind of the hash function results, and a logical constant,
`LITTLE_ENDIAN`, used to deal with one aspect of the machine
dependence of the hash codes. 
Note that while SpookyHash can be used as a sixty four bit hash
algorithm, its algorithms actually returns two element integer arrays
of kind `INT64`, so it can also be used as a 128 bit hash.

### The `INT_HASH` parameters

It is necessary to define the kind of integer used to return the hash
code.
As `stdlib_64_bit_hash_codes` deals exclusively with 64 bit hash codes,
`INT_HASH` is an alias for the integer kind `INT64`.

### The `LITTLE_ENDIAN` parameter

In implementing hash functions it is sometimes necessary to know the
"endianess" of the processor's integers. To this end the
`stdlib_64_bit_hash_codes` module defines the logical parameter
`LITTLE_ENDIAN` that if true indicates that the processor has little
endian integers, and that if false indicates that the integers are big
endian.


### Specifications of the `stdlib_64_bit_hash_codes` procedures

#### `FIBONACCI_HASH` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 64 bit integer.

##### Syntax

`code = [[stdlib_64_bit_hash_codes:fibonacci_hash]]( key, nbits )`

##### Class

Pure function

##### Arguments

`key`: Shall be a scalar integer expression of kind `INT64`. It is an
`intent(in)` argument.

`nbits` Shall be a scalar default integer expression with `0 < nbits <
64`. It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT64` with at most the lowest
`nbits` nonzero.

##### Note

`FIBONACCI_HASH` is an implementation of the Fibonacci Hash of Donald
E. Knuth. It multiplies the `KEY` by the odd valued approximation to
`2**64/phi`, where `phi` is the golden ratio 1.618..., and returns the
`nbits` upper bits of the product as the lowest bits of the result.

##### Example

```fortran
    program demo_fibonacci_hash
      use stdlib_64_bit_hash_codes, only: fibonacci_hash
      use iso_fortran_env, only: int64 
      implicit none
      integer, allocatable :: array1(:)
      integer(int64) :: hash, source
      allocate( array1(0:2**6-1) )
      array1(:) = 0
      source = int(Z'1FFFFFFFF', int64)
      hash = fibonacci_hash(source, 6)
      azray1(hash) = source
      print *, hash
    end program demo_fibonacci_hash
```

#### `FNV_1`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 64 bit hash code from a rank 1 integer array or a default
character string.

##### Syntax

`code = [[stdlib_64_bit_hash_codes:fnv_1]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank 1 integer array expression of kind `INT8`, `INT16`,
`INT32`, or `INT64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT64`. 

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


##### Example

```fortran
    program demo_fnv_1_hash
      use stdlib_64_bit_hash_codes, only: fnv_1_hash
	  use iso_fortran_env, only: int64
      implicit none
      integer, allocatable :: array1(:)
      integer(int64) :: hash
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      hash = fnv_1_hash(array1)
      print *, hash
    end program demo_fnv_1_hash
```


#### `FNV_1A`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 64 bit hash code from a rank 1 integer array or a default
character string.

##### Syntax

`code = [[stdlib_64_bit_hash_codes:fnv_1a]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a deferred length default character scalar expression
or a rank 1 integer array expression of kind `INT8`, `INT16`,
`INT32`, or `INT64`.
It is an `intent(in)` argument.

##### Result

The result is a scalar integer of kind `INT32`.

##### Note

`FNV_1A` is an implementation of the alternative FNV-1a hash code of
Glenn Fowler, Landon Curt Noll, and Phong Vo.
It differs from typical implementations in that it also ecodes the
size of the structure in the hash code.
This code is relatively fast on short keys, and is small enough that it
will often be retained in the instruction cache if hashing is
intermittent.
As a result it should give good performance for typical hash table
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.

##### Example

```fortran
    program demo_fnv_1a_hash
      use stdlib_64_bit_hash_codes, only: fnv_1a_hash
      use iso_fortran_env, only: int64
      implicit none
      integer, allocatable :: array1(:)
      integer(int64) :: hash
      array1 = [ 5, 4, 3, 1, 10, 4, 9]
      hash = fnv_1a_hash(array1)
      print *, hash
    end program demo_fnv_1a_hash
```


#### `NEW_PENGY_HASH_SEED`- returns a valid input seed for `PENGY_HASH`

##### Status

Experimental

##### Description

Calculates a 32 bit "random" integer that is believed to be a valid
seed for `PENGY_HASH` and is also different from the input seed.

##### Syntax

`code = call [[stdlib_32_bit_hash_codes:new_pengy_hash_seed]]( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined integer scalar variable of kind `INT32`.
It is an `intent(inout)` argument. On input `seed` should be defined,
and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `PENGY_HASH`, but if any are
identified the procedure will be revised so that they cannot be
returned.  This subroutine uses Fortran's intrinsic
 `RANDOM_NUMBER` and the values returned can be changed by calling the
 intrinsic `RANDOM_INIT`.

##### Example

See the example for `PENGY_HASH`.


#### `NEW_SPOOKY_HASH_SEED`- returns a valid input seed for `SPOOKY_HASH`

##### Status

Experimental

##### Description

Calculates a 32 bit two element vector of "random" integer values that
is believed to be a valid seed for `SPOOKY_HASH` and is also different
from the input seed. 

##### Syntax

`code = call [[stdlib_32_bit_hash_codes:new_spooky_hash_seed]]( seed )`

##### Class

Subroutine

##### Argument

`seed`: shall be a defined two element integer vector variable of kind
`INT32`. It is an `intent(inout)` argument. On input `seed` should be
defined, and on output it will be different from the input `seed`.

##### Note

Currently there are no known bad seeds for `SPOOKY_HASH`, but if any are
identified the procedure will be revised so that they cannot be
returned.  This subroutine uses Fortran's intrinsic
 `RANDOM_NUMBER` and the values returned can be changed by calling the
 intrinsic `RANDOM_INIT`.

##### Example

See the example for `SPOOKY_HASH`.


#### `ODD_RANDOM_INTEGER` - returns odd integer

##### Status

Experimental

##### Description

Returns a random 64 bit integer distributed uniformly over the odd values.

##### Syntax

`call [[stdlib_64_bit_hash_codes:odd_random_integer]]( harvest )`

##### Class

Subroutine

##### Argument

`harvest`: Shall be an integer of kind `INT64`. It is an `intent(out)`
argument.

##### Note

`ODD_RANDOM_INTEGER` is intended to generate seeds for
 `UNIVERSAL_MULT_HASH`. `ODD_RANDOM_NUMBER` uses Fortran's intrinsic
 `RANDOM_NUMBER` and the values returned can be changed by calling the
 intrinsic `RANDOM_INIT`.

##### Example

See `UNIVERSAL_MULT_HASH`.


#### `PENGY_HASH` - maps a character string or integer vector to an integer

##### Status

Experimental

##### Description

Maps a character string or integer vector to a 64 bit integer whose
value also depends on a scalar 32 bit integer, `seed`.

##### Syntax

`code = [[stdlib_64_bit_hash_codes:pengy_hash]]( key, seed )`

#####  Class

Pure function

##### Arguments

`key`: shall be a scalar  expression of type default character or a
Rank 1 integer vector expression of kind `INt8`, `INT16`, `INT32`, or
`INTT64`. It is an `intent(in)` argument.

`seed`: shall be an integer ex of kind `INT64`. It ispression
an `intent(in)` argument.

##### Result

The result is an integer of kind `INT64`.

##### Note

`PENGY_HASH` is an implementation of the 64 bit `pengyhash` of Alberto
Fajardo. The hash has acceptable performance on small keys, and good
performance on long keys. It passes all the SMHasher tests, and has
no known bad seeds.

##### Exampl

```fortran
    program demo_pengy_hash
      use stdlib_64_bit_hash_codes, only: new_pengy_hash_seed, pengy_hash
      use iso_fortran_env, only: int64 
      implicit none
      integer, allocatable :: key(:)
      integer(int64) :: hash
      integer(int32)  ::  seed
      key = [ 0_int64, 1_int64, 2_int64, 3_int64 ]
      seed = 0_int32
      call new_pengy_hash_seed( seed )
      hash = pengy_hash( key, seed )
      print *, seed, hash
    end program demo_pengy_hash
```


#### `SPOOKY_HASH` - maps a character string or integer vector to an integer

##### Status

Experimental

##### Description

Maps a character string or integer vector to a 64 bit integer whose
value also depends on a two element vector,  `seed`.

##### Syntax

`code = [[stdlib_64_bit_hash_codes:spooky_hash]]( key, seed )`

#####  Class

Pure function

##### Arguments

`key`: shall be a scalar of type default character expression or a
Rank 1 integer vector expression of kind `INt8`, `INT16`, `INT32`, or
`INTT64`. It is an `intent(in)` argument.

`seed`: shall be a two element integer vector expression of kind
`INT64`. It is an `intent(in)` argument.

##### Result

The result is a two element integer vector of kind `INT64`.

##### Note

`SPOOKY_HASH` is an implementation of the 64 bit version 2 of
SpookyHash of Bob Jenkins. The code was designed for Little-Endian
processors. The output is different on Big Endian processors, but still
probably as good quality. It is often used as a 64 bit hash using the
first element of the returned value, but can be used as a 128 bit
hash. This version of `SPOOKY_HASH` has good performance on small keys
and excellent performance on long keys. It passes all the SMHasher tests
and has no known bad seeds.

##### Example

```fortran
    program demo_spooky_hash
      use stdlib_64_bit_hash_codes, only: new_spooky_hash_seed, &
          spooky_hash
      use iso_fortran_env, only: int64 
      implicit none
      integer, allocatable :: key(:)
      integer(int64) :: hash(2), seed(2), source
      key = [ 0_int64, 1_int64, 2_int64, 3_int64 ]
      seed = [ 119_int64, 2_int64**41-1 ]
      call new_spooky_hash_seed( seed )
      hash = spooky_hash( key, seed )
      print *, seed, hash
    end program demo_spooky_hash
```

#### `UNIVERSAL_MULT_HASH` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

Calculates an `nbits` hash code from a 64 bit integer.

##### Syntax

`code = [[stdlib_64_bit_hash_codes:universal_mult_hash]]( key, seed, nbits )`

##### Class

Pure function

##### Arguments

`key`: Shall be an integer of kind `INT64`. It is an `intent(in)`
argument.

`seed`: Shall be an integer of kind `INT64`. It is an `intent(in)`
argument. It should be an odd value.

`nbits` Shall be a default integer with `0 < nbits < 64`. It is an
`intent(in)` argument.  It must be an odd integer.

##### Result

The result is an integer of kind `INT64` with at most the lowest
`nbits` nonzero.

##### Note

`UNIVERSAL_MULT_HASH` is an implementation of the Universal
Multiplicative Hash of M. Dietzfelbinger, et al.
It multiplies the `KEY` by `SEED`, and returns the
`NBITS` upper bits of the product as the lowest bits of the result.

##### Example


```fortran
    program demo_universal_mult_hash
      use stdlib_32_bit_hash_codes, only: odd_random_integer, &
          universal_mult_hash
      use iso_fortran_env, only: int64
      implicit none
      integer, allocatable :: array1(:)
      integer(int64) :: hash, i, seed, source
      seed = 0
      allocate( array1(0:2**6-1) )
      do i = 0, 2**6-1
          array(i) = i
      end do
      call odd_random_integer( seed )
      source = int(Z'1FFFFFF', int64)
      hash = universal_mult_hash(source, seed, 6)
      azray1(hash) = source
      print *, seed, hash, array1
    end program demo_universal_mult_hash
```


### Test Codes

The Fortran Standard Library provides two test codes for the hash
functions of `stdlib_32_bit_hash_functions` and
`stdlib_64_bit_hash_functions`, `test_32_bit_hash_performance` and
`test_64_bit_hash_performance` respectively. These are primarilly set
up to test runtime performance of the functions. They take a sample of
`2**18` integers of kind `INT8` and break it up into vectors of size
1, 2, 4, 8, 16, 64,  256, and 1024 elements, yielding `2**18`,
`2**17`, `2**16`, `2**15`, `2**14`, `2**12`, `2**10`, and `2**8`
vectors respectively. These are then processed by the hash functions
4 times, and the time for processing is reported. Testing so far has
been on a MacBook Pro with a 2.3 GHz Quad-Core Intel Core i5 and 8 GB
2133 MHz LPDDR3 of RAM, using GNU Fortran (GCC) 11.1.0 to compile the
code. The results for `test_32_bit_hash_performance` is given by the
following table:

| Algorithm  | Key Size  | Key #      | Time (s) |
|            | Bytes     |            |          |
|------------|-----------|------------|----------|
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

| Algorithm  | Key Size  | Key #      | Time (s) |
|            | Bytes     |            |          |
|------------|-----------|------------|----------|
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
