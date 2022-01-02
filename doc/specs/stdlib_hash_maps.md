---
title: Hash maps
---

# The `stdlib_hashmap_wrappers`, `stdlib_hashmap_chaining` and `stdlib_hashmap_open` modules

(TOC)

## Overview of hash maps

A hash map (hash table) is a data structure that maps *keys* to
*values*. It uses a hash function to compute a hash code from the *key*
that serves as an index into a linear array of *slots* (buckets) from
which the desired *value* can be extracted.
Each key ideally maps to a unique slot, but most hash functions are
imperfect and can map multiple keys to the same *slot* resulting in
collisions. Hash maps differ in how they deal with such collisions.
This document discusses the hash maps in the Fortran Standard Library.

## Licensing

The Fortran Standard Library is distributed under the MIT License.
However components of the library should be evaluated as to whether
they are compatible with the MTI License.
The current hash maps were inspired by an
[implementation](http://chasewoerner.org/src/hasht/) of David
Chase. While the code has been greatly modified from his
implementation, he has give permission for the unrestricted use of
his code.


## The hash map modules

The Fortran Standard Library provides three modules for the
implementation of simple hash maps. These maps only accept hash
functions with a single argument, the key, and yield a 32 bit
hash code. The modules will need to be modified if it is desired to
use hash functions with a different API. The three modules are:
`stdlib_hashmap_wrappers`, `stdlib_hashmap_chaining` and
`stdlib_hashmap_open`, corresponding to the files:
`stdlib_hashmap_wrappers.f90`, `stdlib_hashmap_chaining.f90`,
and `stdlib_hashmap_open.f90`. The module
`stdlib_hashmap_wrappers` provides an interface to the 32 bit
hash functions of the Standard Library module,
`stdlib_hash_32bit`, and provides wrappers to some of the
hash functions so that they no longer need to be supplied seeds. The
module `stdlib_hashmap_chaining` defines a datatype,
`chaining_hashmap_type`, implementing a simple separate chaining hash
map noted more for its diagnostics than its performance. Finally the
module `stdlib_hashmap_open` defines a datatype,
`open_hashmap_type`, implementing a simple open addressing hash
map noted more for its diagnostics than its performance.

These maps use separate chaining with linked lists and linear open
addressing, respectively, to deal with hash index collisions, and are
largely defined in the separated submodules, `stdlib_hashmap_chainings`
and `stdlib_hashmap_opens`, respectively.
In `chaining_hashmap_type` the colliding indices are handled by using
linked lists with their roots at the hash index.
In `open_hashmap_type`, the colliding indices are handled by searching
from the initial hash index in increasing
steps of one (modulo the hash map size) for an open map bin.

The maps share many attributes in common. The two types share a
common Application Programers Interface (API). The maps use powers of
two for their slot sizes, so that the function, `fibonacci_hash`, can
be used to map the hash codes to indices in the map. This is
expected to be more efficient than prime number mapping using a
modulo operation, and reduces the requirement that the hash
function need to do a good job randomizing its lower order bits.
This requires a good randomizing hash method for good performance.
Both adjust the map size to reduce collisions, based on 
the ratio of the number of hash map probes to the number of subroutine 
calls.
The maps make extensive use of pointers internally, but a private
finalization subroutine avoids memory leaks.
The maps can take entry keys of type `key_type`.
Both maps allow the addition and lookup of entries, and the inclusion
of data in addition to the entry key.
The `chaining_hashmap_type` also allows the selective removal of
entries.

## The `stdlib_hashmap_wrappers` module

The `stdlib_hashmap_wrappers` module provides data types to
represent keys and associated data stored in a module, but is also, a
wrapper for the `stdlib_hash_32bit` module. It allows
direct access to the `stdlib_hash_32bit` procedures:
`fibonacci_hash`, `fnv_1_hasher`, `fnv_1a_hasher`; and provides
wrapper functions, `seeded_nmhash32_hasher`,
`seeded_nmhash32x_hasher`, and `seeded_water_hasher` to the hashing
functions: `nmhash32`, `nmhash32x`, and `water_hash`, respectively. It
defines an interface, `hasher_fun`, compatible with the hash functions
that take a `non-scalar key`. It defines one integer constant used
as a kind value,`int_hash`. It also defines two types, `key_type` and
`other_type`, and associated procedures, for storing and manipulating
keys and their associated data.

### The `stdlib_hashmap_wrappers` constant, `int_hash`

The constant `int_hash` is used to define the integer kind value for
the returned hash codes and variables used to access them. It
currently has the value, `int32`.

### The `stdlib_hashmap_wrappers` module derived types

The `stdlib_hashmap_wrappers` module defines two derived types:
`key_type`, and `other_type`. The `key_type` is intended to be used
for the search keys of hash tables.  The `other_type` is intended to
store additional data associated with a key. Both types are
opaque. Their current representations are as follows

```fortran
    type :: key_type
        private
        integer(int8), allocatable :: value(:)
    end type key_type

    type :: other_type
        private
        class(*), allocatable :: value
    end type other_type
```

The  module also defines seven procedures for those types: `copy_key`,
`copy_other`, `equal_keys`, `free_key`, `free_other`, `get`, and `set`
for use by the hash maps to manipulate or inquire of components of
those types.

### Table of `stdlib_hashmap_wrappers` procedures

The  `stdlib_hashmap_wrappers` module provides procedures in
several categories: procedures to manipulate data of the `key_type`;
procedures to manipulate data of the `other_type`, and 32 bit hash
functions for keys. The procedures in each category are listed below.

Procedures to manipulate `key_type` data:

* `copy_key( key_in, key_out )` - Copies the contents of the key,
  `key_in`, to contents of the key, `key_out`.

* `equal_keys( key1, key2 )` - compares two keys for equality. 

* `get( key, value )` - extracts the contents of `key` into `value`,
  an `int8` array or character string.

* `free_key( key )` - frees the memory in `key`.

* `set( key, value )` - sets the content of `key` to `value`.

Procedures to manipulate `other_type` data:

* `copy_other( other_in, other_out )` - Copies the contents of the
  other data, `other_in`, to the contents of the other data,
  `other_out`.

* `get( other, value )` - extracts the contents of `other` into the
  class(*) variable `value`.

* `set( other, value )` - sets the content of `other` to the class(*)
  variable `value`. 

* `free_other( other )` - frees the memory in `other`.

Procedures to hash keys to 32 bit integers:

* `fnv_1_hasher( key )` - hashes a `key` using the FNV-1 algorithm.

* `fnv_1a_hasher( key )` - hashes a `key` using the FNV-1a algorithm.

* `seeded_nmhash32_hasher( key )` - hashes a `key` using the nmhash32
  algorithm.

* `seeded_nmhash32x_hasher( key )` - hashes a `key` using the nmhash32x
  algorithm.

* `seeded_water_hasher( key )` - hashes a `key` using the waterhash
  algorithm.

### Specifications of the `stdlib_hashmap_wrappers` procedures

#### `copy_key` - Returns a copy of the key

##### Status

Experimental

##### Description

Returns a copy of an input of type `key_type`.

##### Syntax

`call [[stdlib_hashmap_wrappers:copy_key]]( key_in, key_out )`

##### Class

Subroutine.

##### Arguments

`key_in`: shall be a scalar expression of type `key_type`. It
is an `intent(in)` argument.

`key_out`: shall be a scalar variable of type `key_type`. It
is an `intent(out)` argument.

##### Example

```fortran
    program demo_copy_key
      use stdlib_hashmap_wrappers, only: &
          copy_key, equal_keys, key_type
      use iso_fortran_env, only: int8
      implicit none
      integer(int8), allocatable :: value(:)
      type(key_type) :: key_in, key_out
      integer(int_8) :: i
      allocate( value(1:15) )
      do i=1, 15
          value(i) = i
      end do
      call set( key_in, value )
      call copy_key( key_in, key_out )
      print *, "key_in == key_out = ", equal_keys( key_in, key_out )
    end program demo_copy_key
```

#### `copy_other` - Returns a copy of the other data

##### Status

Experimental

##### Description

Returns a copy of an input of type `other_type`.

##### Syntax

`call [[stdlib_hashmap_wrappers:copy_other]]( other_in, other_out )`

##### Class

Subroutine.

##### Arguments

`other_in`: shall be a scalar expression of type `other_type`. It
is an `intent(in)` argument.

`other_out`: shall be a scalar variable of type `other_type`. It
is an `intent(out)` argument.

##### Example

```fortran
    program demo_copy_other
      use stdlib_hashmap_wrappers, only: &
          copy_other, get, other_type, set
      use iso_fortran_env, only: int8
      implicit none
      type(other_type) :: other_in, other_out
      integer(int_8) :: i
	  class(*), allocatable :: dummy
	  type dummy_type
          integer(int8) :: value(15)
      end type
	  type(dummy_type) :: dummy_val
      do i=1, 15
          dummy_val % value1(i) = i
      end do
      allocate(other_in % value, source=dummy_val)
      call copy_other( other_in, other_out )
      select type(other_out)
	  type(dummy_type)
          print *, "other_in == other_out = ", &
            all( dummy_val % value == other_out % value )
      end select
    end program demo_copy_other
```

#### `equal_keys` - Compares two keys for equality

##### Status

Experimental

##### Description

Returns `.true.` if two keys are equal, and `.false.` otherwise.

##### Syntax

`test = [[stdlib_hashmap_wrappers:equal_keys]]( key1, key2 )`

##### Class

Pure function.

##### Arguments

`key1`: shall be a scalar expression of type `key_type`. It
is an `intent(in)` argument.

`key2`: shall be a scalar expression of type `key_type`. It
is an `intent(in)` argument.

##### Result character

The result is a value of type default `logical`.

##### Result value

The result is `.true.` if the keys are equal, otherwise `.falss`.

##### Example

```fortran
    program demo_equal_keys
      use stdlib_hashmap_wrappers, only: &
          copy_key, equal_keys, key_type, set
      use iso_fortran_env, only: int8
      implicit none
      integer(int8), allocatable :: value(:)
      type(key_type) :: key_in, key_out
      integer(int_8) :: i
      allocate( value(1:15) )
      do i=1, 15
          value(i) = i
      end do
      call set( key_in, value )
      call copy_key( key_in, key_out )
      print *, "key_in == key_out = ", equal_keys( key_in, key_out )
    end program demo_equal_keys
```


#### `fibonacci_hash` - maps an integer to a smaller number of bits

##### Status

Experimental

##### Description

`fibonacci_hash` is just a re-export of the function of the same name
implemented in
[`stdlib_hash_32bit`](https://stdlib.fortran-lang.org/page/spec/stdlib_hash_functions.html#fibonacci_hash-maps-an-integer-to-a-smaller-number-of-bits).
It reduces the value of a 32 bit integer to a smaller number of bits.


#### `fnv_1_hasher`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from an input of type `key_type`.

##### Syntax

`code = [[stdlib_hashmap_wrappers:fnv_1_hasher]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a scalar expression of type `key_type`.
It is an `intent(in)` argument.

##### Result character

The result is a scalar integer of kind `int32`.

##### Result value

The result is a hash code created using the FNV-1 algorithm.

##### Note

`fnv_1_hasher` is an implementation of the original FNV-1 hash code of
Glenn Fowler, Landon Curt Noll, and Phong Vo.
This code is relatively fast on short keys, and is small enough that
it will often be retained in the instruction cache if hashing is
intermitent.
As a result it should give good performance for typical hash map
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.


##### Example

```fortran
    program demo_fnv_1_hasher
      use stdlib_hashmap_wrappers, only: &
          fnv_1_hasher, key_type, set
      use iso_fortran_env, only: int32 
      implicit none
      integer(int8), allocatable :: array1(:)
      integer(int32) :: hash
      type(key_type) :: key
      array1 = [ 5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8 ]
      call set( key, array1 )
      hash = fnv_1_hasher(key)
      print *, hash
    end program demo_fnv_1_hasher
```


#### `fnv_1a_hasher`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from an input of type `key_type`.

##### Syntax

`code = [[stdlib_hashmap_wrappers:fnv_1a_hasher]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a scalar expression of type `key_type`.
It is an `intent(in)` argument.

##### Result character

The result is a scalar integer of kind `int32`.

##### Result value

The result is a hash code created using the FNV-1a algorithm.

##### Note

`fnv_1a_hasher` is an implementation of the original FNV-1A hash code
of Glenn Fowler, Landon Curt Noll, and Phong Vo.
This code is relatively fast on short keys, and is small enough that
it will often be retained in the instruction cache if hashing is
intermitent.
As a result it should give good performance for typical hash map
applications.
This code does not pass any of the SMHasher tests, but the resulting
degradation in performance due to its larger number of collisions is
expected to be minor compared to its faster hashing rate.


##### Example

```fortran
    program demo_fnv_1a_hasher
      use stdlib_hashmap_wrappers, only: &
         fnv_1a_hasher, key_type, set
      use iso_fortran_env, only: int32 
      implicit none
      integer(int8), allocatable :: array1(:)
      integer(int32) :: hash
      type(key_type) :: key
      array1 = [ 5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8 ]
      call set( key, array1 )
      hash = fnv_1a_hasher(key)
      print *, hash
    end program demo_fnv_1a_hasher
```

#### `free_key` - frees the memory associated with a key

##### Status

Experimental

##### Description

Deallocates the memory associated with an variable of type
`key_type`.

##### Syntax

`call [[stdlib_hashmap_wrappers:free_key]]( key )`

##### Class

Subroutine.

##### Argument

`key`: shall be a scalar variable of type `key_type`. It
is an `intent(out)` argument.

##### Example

```fortran
    program demo_free_key
      use stdlib_hashmap_wrappers, only: &
          copy_key, free_key, key_type, set
      use iso_fortran_env, only: int8
      implicit none
      integer(int8), allocatable :: value(:)
      type(key_type) :: key_in, key_out
      integer(int_8) :: i
      allocate( value(1:15) )
      do i=1, 15
        value(i) = i
      end do
      call set( key_in, value )
      call copy_key( key_in, key_out )
      call free_key( key_out )
    end program demo_free_key
```

#### `free_other` - frees the memory associated with other data

##### Status

Experimental

##### Description

Deallocates the memory associated with an variable of type
`other_type`.

##### Syntax

`call [[stdlib_hashmap_wrappers:free_other]]( other )`

##### Class

Subroutine.

##### Argument

`other`: shall be a scalar variable of type `other_type`. It
is an `intent(out)` argument.

##### Example

```fortran
    program demo_free_other
      use stdlib_hashmap_wrappers, only: &
          copy_other, free_other, other_type, set
      use iso_fortran_env, only: int8
      implicit none
      type dummy_type
          integer(int8) :: value(15)
      end type dummy_type
      typer(dummy_type) :: dummy_val
      type(other_type), allocatable :: other_in, other_out
      integer(int_8) :: i
      do i=1, 15
          dummy_val % value(i) = i
      end do
      allocate(other_in, source=dummy_val)
      call copy_other( other_in, other_out )
      call free_other( other_out )
    end program demo_free_other
```


#### `get` - extracts the data from a derived type

##### Status

Experimental

##### Description

Extracts the data from a `key_type` or `other_type` and stores it
in the variable `value`.

##### Syntax

`call [[stdlib_hashmap_wrappers:get]]( key, value )`

or

`call [[stdlib_hashmap_wrappers:get]]( other, value )`

##### Class

Subroutine.

##### Argument

`key`: shall be a scalar expression of type `key_type`. It
is an `intent(in)` argument.

`other`: shall be a scalar expression of type `other_type`. It
is an `intent(in)` argument.

`value`: if the the first argument is of `key_type` `value` shall be
an allocatable default character string variable, or 
an allocatable vector variable of type integer and kind `int8`,
otherwise the first argument is of `other_type` and `value` shall be
an allocatable of `class(*)`. It is an `intent(out)` argument.

##### Example

```fortran
    program demo_get
      use stdlib_hashmap_wrappers, only: &
          get, key_type, set
      use iso_fortran_env, only: int8
      implicit none
      integer(int8), allocatable :: value(:), result(:)
      type(key_type) :: key
      integer(int_8) :: i
      allocate( value(1:15) )
      do i=1, 15
        value(i) = i
      end do
      call set( key, value )
      call get( key, result )
      print *, `RESULT == VALUE = ', all( value  == result )
    end program demo_get
```


#### `hasher_fun`- serves aa a function prototype.

##### Status

Experimental

##### Description

Serves as a prototype for hashing functions with a single, `key`,
argument of type `key_type` returning an `int32` hash value.

##### Syntax

`type([[stdlib_hashmap_wrappers:hasher_fun]]), pointer :: fun_pointer`

##### Class

Pure function prototype

##### Argument

`key`: Shall be a rank one array expression of type `integer(int8)`.
It is an `intent(in)` argument.

##### Result character

The result is a scalar integer of kind `int32`.

##### Result value

The result is a hash code.

##### Note

`hasher_fun` is a prototype for defining dummy arguments and function
pointers intended for use as a hash function for the hash maps.

##### Example

```fortran
    program demo_hasher_fun
      use stdlib_hashmap_wrappers, only: &
          fnv_1a_hasher, hasher_fun, set
      use iso_fortran_env, only: int8, int32 
      implicit none
      type(hasher_fun), pointer :: hasher_pointer
      integer(int8), allocatable :: array1(:)
      integer(int32) :: hash
      type(key_type) :: key
      hasher_pointer => fnv_1a_hasher
      array1 = [ 5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8 ]
      call set( key, array1 )
      hash = hassher_pointer(key)
      print *, hash
    end program demo_hasher_fun
```

#### `seeded_nmhash32_hasher`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from an input of type `key_type`.

##### Syntax

`code = [[stdlib_hashmap_wrappers:seeded_nmhash32_hasher]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a scalar expression of type `key_type`.
It is an `intent(in)` argument.

##### Result character

The result is a scalar integer of kind `int32`.

##### Result value

The result is a hash code created using the `nmhash32` algorithm.

##### Note

`seeded_nmhash32_hasher` is a wrapper to the `NMHASH32_HASH` of the
module `stdlib_hash_32bit`, which supplies a fixed seed
to the wrapped function. `NMHASH32` is an implementation of the
`nmhash32` hash code of James Z. M. Gao.
This code has good, but not great, performance on long keys, poorer
performance on short keys.
As a result it should give fair performance for typical hash map
applications.
This code passes the SMHasher tests.


##### Example

```fortran
    program demo_seeded_nmhash32_hasher
      use stdlib_hashmap_wrappers, only: &
         seeded_nmhash32_hasher, key_type, set
      use iso_fortran_env, only: int32 
      implicit none
      integer(int8), allocatable :: array1(:)
      integer(int32) :: hash
      type(key_type) :: key
      array1 = [ 5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8 ]
      call set( key, array1 )
      hash = seeded_nmhash32_hasher (key)
      print *, hash
    end program demo_seeded_nmhash32_hasher
```

#### `seeded_nmhash32x_hasher`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from an input of type `key_type`.

##### Syntax

`code = [[stdlib_hashmap_wrappers:seeded_nmhash32x_hasher]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a scalar expression of type `key_type`.
It is an `intent(in)` argument.

##### Result character

The result is a scalar integer of kind `int32`.

##### Result value

The result is a hash code created using the `nmhash32x` algorithm.

##### Note

`seeded_nmhash32x_hasher` is a wrapper to the `nmhash32x_hash` of the
module `stdlib_hash_32bit`, which supplies a fixed seed
to the wrapped function. `nmhash32x` is an implementation of the
`nmhash32x` hash code of James Z. M. Gao.
This code has good, but not great, performance on long keys, poorer
performance on short keys.
As a result it should give fair performance for typical hash map
applications.
This code passes the SMHasher tests.

##### Example

```fortran
    program demo_seeded_nmhash32x_hasher
      use stdlib_hashmap_wrappers, only: &
         seeded_nmhash32x_hasher, key_type, set
      use iso_fortran_env, only: int32 
      implicit none
      integer(int8), allocatable :: array1(:)
      integer(int32) :: hash
      type(key_type) :: key
      array1 = [ 5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8 ]
      call set( key, array1 )
      hash = seeded_nmhash32x_hasher (key)
      print *, hash
    end program demo_seeded_nmhash32x_hasher
```

#### `seeded_water_hasher`- calculates a hash code from a key

##### Status

Experimental

##### Description

Calculates a 32 bit hash code from an input of type `key_type`.

##### Syntax

`code = [[stdlib_hashmap_wrappers:seeded_water_hasher]]( key )`

##### Class

Pure function

##### Argument

`key`: Shall be a scalar expression of type `key_type`.
It is an `intent(in)` argument.

##### Result character

The result is a scalar integer of kind `int32`.

##### Result value

The result is a hash code created using the `waterhash` algorithm.

##### Note

`seeded_water_hasher` is a wrapper to the `water_hash` of the
module `stdlib_hash_32bit`, which supplies a fixed seed
to the wrapped function. `water_hash` is an implementation of the
`waterhash` hash code of Tommy Ettinger.
This code has excellent performance on long keys, and good performance
on short keys.
As a result it should give reasonable performance for typical hash
table applications.
This code passes the SMHasher tests.


##### Example

```fortran
    program demo_seeded_water_hasher
      use stdlib_hashmap_wrappers, only: &
         seeded_water_hasher, key_type, set
      use iso_fortran_env, only: int32 
      implicit none
      integer(int8), allocatable :: array1(:)
      integer(int32) :: hash
      type(key_type) :: key
      array1 = [ 5_int8, 4_int8, 3_int8, 1_int8, 10_int8, 4_int8 ]
      call set( key, array1 )
      hash = seeded_water_hasher (key)
      print *, hash
    end program demo_seeded_water_hasher
```


#### `set` - places the data in a derived type

##### Status

Experimental

##### Description

Places the data from `value` in a `key_type` or an `other_type`.

##### Syntax

`call [[stdlib_hashmap_wrappers:set]]( key, value )`

or

`call [[stdlib_hashmap_wrappers:set]]( other, value )`


##### Class

Subroutine.

##### Argument

`key`: shall be a scalar variable of type `key_type`. It
is an `intent(out)` argument.

`other`: shall be a scalar variable of type `other_type`. It
is an `intent(out)` argument.

`value`: if the first argument is `key` `vaalue` shall be a default
character string expression, or a vector expression of type integer
and kind `int8`, while for a first argument of type `other` `value`
shall be of type `class(*)`. It is an `intent(in)` argument.

##### Example

```fortran
    program demo_set
      use stdlib_hashmap_wrappers, only: &
          get, key_type, set
      use iso_fortran_env, only: int8
      implicit none
      integer(int8), allocatable :: value(:), result(:)
      type(key_type) :: key
      integer(int_8) :: i
      allocate( value(1:15) )
      do i=1, 15
        value(i) = i
      end do
      call set( key, value )
      call get( key, result )
      print *, `RESULT == VALUE = ', all( value  == result )
    end program demo_set
```


## The `stdlib_hashmap_chaining` module

The `stdlib_hashmap_chaining` module provides access to all the
public entities in the `stdlib_hashmap_wrappers` module. It
also defines a public data type and associated procedures and
constants that implement a simple hash map using
separate chaining hashing. The derived type is
`chaining_hashmap_type`. It provides 
procedures to manipulate the structure of the hash map:
`init`, `map_entry`, `rehash`, `remove_entry`, and
`set_other_data`. It provides procedures to inquire about entries in
the hash map: `get_other_data`, `in_map`, `unmap` and `valid_index`.
Finally it provides procedures to inquire about the overall
structure and performance of the table:`calls`,  `entries`,
`get_other_data`, `loading`, `slots`, and `total_depth`. The module
also defines a number of public constants: `inmap_probe_factor`,
`map_probe_factor`, `default_bits`,
`max_bits`, `int_calls`, `int_depth`, `int_index`,
`int_probes`, `success`, `alloc_fault`, and `array_size_error`.

### The `stdlib_hashmap_chaining` module's public constants

The module defines several categories of public constants. Some are
used to parameterize the empirical slot expansion code. Others
parameterize the slots table size. Some are used to define
integer kind values for different applications. Finally, some are used
to report errors or success.

The constants `inmap_probe_factor`, and `map_probe_factor` are used to
parameterize the slot expansion code used to determine when in a
`inchain_map_call` the number 
of slots need to be increased to decrease the lengths of the linked
lists. The constant `inmap_probe_factor` is used to determine when
the ratio of the number of map probes to map calls is too large and
the slots need expansion. The constant `map_probe_factor` is used to
determine when inserting a new entry the ratio of the number of map
probes to map calls is too large and the slots need expansion.

The constants `default_bits`, and
`max_bits` are used to parameterize the table's slots size. The
`default_bits` constant defines the default initial number of slots
with a current value of 6 resulting in an initial `2**6 == 64`
slots. This may optionally be overridden on hash map creation. The
`max_bits` parameter sets the maximum table size as `2**max_bits` with
a default value for `max_bits` of 30. The table will not work for a
slots size greater than `2**30`.

The constants `int_calls`, `int_depth`, `int_index`, and `int_probes`
are used to define integer kind values for various contexts. The
number of calls are reported and stored in entities of kind
`int_calls`. Currently `int_calls` has the value of `int64`. The
total depth, the number of inquiries needed to access all elements
of the table, is reported and stored in entities of kind
`int_depth`. Currently `int_depth` has the value of `int64`. The
number of entries in the table, is reported and stored in entities of
kind `int_index`. Currently `int_index` has the value of `int32`.
The number of probes, hash map enquiries, are reported and stored in
entities of kind `int_probes`. Currently `int_probes` has the value of
`int64`.

Finally the error codes `success`, `alloc_fault`, and
`array_size_error` are used to report the error status of certain
procedure calls. The `succes` code indicates that no problems were
found. The `alloc_fault` code indicates that a memory allocation
failed. Finally the `array_size_error` indicates that on table
creation `slots_bits` is less than `default_bits` or
greater than `max_bits`.

### The `stdlib_hashmap_chaining` module's derived types

The `stdlib_hashmap_chaining` module defines several derived
types. The only public type is the `chaining_hashmap_type`. There are
three other private derived types used in the implementation of the
public type: `chaining_map_entry_type`, `chaining_map_entry_ptr`, and
`chaining_map_entry_pool`. Each of these is described below.

#### The `chaining_map_entry_type` derived type

Entities of the type `chaining_map_entry_type` are used to define
a linked list structure that stores the
key, its other data, the hash of the key, and the resulting index into
the inverse table. The type's definition is below:

```fortran
    type :: chaining_map_entry_type  ! Chaining hash map entry type
        private
        integer(int_hash)   :: hash_val ! Full hash value
        type(key_type)      :: key ! The entry's key
        type(other_type)    :: other ! Other entry data
        integer(int_index)  :: index ! Index into inverse table
        type(chaining_map_entry_type), pointer :: &
            next => null() ! Next bucket
    end type chaining_map_entry_type
```
Currently the `int_hash` and `int_index` have the value of `int32`.

#### The `chaining_map_entry_ptr` derived type

The type `chaining_map_entry_ptr` is used to define the elements of
the hash map that are either empty or link to the linked lists
containing the elements of the table. The type's definition is below:

```fortran
    type chaining_map_entry_ptr ! Wrapper for a pointer to a chaining
                                ! map entry type object
        type(chaining_map_entry_type), pointer :: target => null()
    end type chaining_map_entry_ptr
```

#### The `chaining_map_entry_pool` derived type

The type `chaining_map_entry_pool` is used to implement a pool of
allocated `chaining_map_entry_type` elements to save on allocation
costs. The type's definition is below: 

```fortran
    type :: chaining_map_entry_pool
    ! Type implementing a pool of allocated
    ! `chaining_map_entry_type` objects
        private
    ! Index of next bucket
        integer(int_index)                          :: next = 0
        type(chaining_map_entry_type), allocatable :: more_map_entries(:)
        type(chaining_map_entry_pool), pointer      :: lastpool => null()
    end type chaining_map_entry_pool
```


#### The `chaining_hashmap_type` derived type

The `chaining_hashmap_type` derived type implements a separate
chaining hash map. It provides the components `calls`, `probes`,
`total_probes`, `entries`, and `slots_bits` to keep track
of the hash map's usage. The array element `slots` serves as the
table proper. The array element `inverse` maps integers to
entries. The linked list entry, `free_list`, keeps track of freed
elements of type `chaining_map_entry_type`. The list element, `cache`,
stores pools of `chaining_map_entry_type` elements for reuse. The
component `hasher` is a pointer to the hash function. Finally the
type-bound procedure, `free_chaining_map`, serves as a finalizer for
objects of the type, `chaining_hashmap_type`.

```fortran
    type :: chaining_hashmap_type
        private
        integer(int_calls) :: calls = 0
        ! Number of calls
        integer(int_calls) :: probes = 0
        ! Number of probes since last expansion
        integer(int_calls) :: total_probes = 0
        ! Cumulative number of probes
`       integer(int_index) :: entries = 0
        ! Number of entries
        integer(int32)     :: slots_bits = default_bits
        ! Bits used for slots size
        type(chaining_map_entry_ptr), allocatable :: slots(:)
        ! Array of bucket lists Note # slots=size(slots)
        type(chaining_map_entry_ptr), allocatable :: inverse(:)
        ! Array of bucket lists (inverses) Note max_elts=size(inverse)
        type(chaining_map_entry_type), pointer    :: free_list => null()
        ! free list of map entries
        type(chaining_map_entry_pool), pointer    :: cache => null()
        ! Pool of allocated chaining_map_entry_type objects
        procedure(hasher_fun), pointer, nopass    :: hasher => fnv_1_hasher
        ! Hash function
    contains
        final                    :: free_chaining_map
    end type chaining_hashmap_type
```

### Table of `stdlib_hashmap_chaining` procedures

The `stdlib_hashmap_chaining` module provides procedures in
several categories: a procedure to initialize the map; a procedure to
modify the structure of a map; procedures to modify the content of a
map; procedures to report on the content of a map; and procedures
to report on the structure of the map. The procedures in each category
are listed below.

Procedure to initialize a chaining hash map:

* `init( map, hasher[, slots_bits, status] )` - Routine
  to initialize a chaining hash map.

Procedure to modify the structure of a map:

* `rehash( map, hasher )` - Routine to change the hash function
  for a map.

Procedures to modify the content of a map:

* `map_entry( map, inmap, key, other )` - Inserts an entry into the
  hash map.

* `remove_entry(map, inmap)` - Remove the entry, if any, at map %
  inverse(inmap).

* `set_other_data( map, inmap, other )` - Change the other data
  associated with the entry.

Procedures to report the content of a map:

* `get_other_data( map, inmap, other )` - Returns the other data
  associated with the inverse table index 

* `in_map( map, inmap, key )` - Returns the index into the `inverse`
  array associated with the `key` 

* `unmap( map, inmap, key )` - Returns a copy of the key associated
with an index to the inverse table.

* `valid_index(map, inmap)` - Returns a flag indicating whether `inmap`
  is a valid index.

Procedures to report on the structure of the map:

* `calls( map )` - the number of subroutine calls on the hash map.

* `entries( map )`- the number of entries in a hash map.

* `loading( map )` - the number of entries relative to the number of
  slots in a hash map.

* `map_probes( map )` - the total number of table probes on a hash
  map.

* `slots( map )` - Returns the number of allocated slots in a hash
  map.

* `total_depth( map )` - Returns the total number of one's based
offsets of slot entries from their slot index

### Specifications of the `stdlib_hashmap_chaining` procedures

#### `calls` - Returns the number of calls on a hash map

##### Status

Experimental

##### Description

Returns the number of procedure calls on a hash map.

##### Syntax

`value = [[stdlib_hashmap_chaining:calls]]( map )`

##### Class

Pure function

##### Argument

`map` - shall be an expression of type `chaining_hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be an integer of kind `int_calls`.

##### Result value

The result will be the number of procedure calls on the hash map.

##### Example

```fortran
    program demo_calls
      use stdlib_hashmap_chaining, only: &
         chaining_hashmap_type, calls, init, int_calls, &
         fnv_1_hasher
      implicit none
      type(chaining_hashmap_type) :: map
      type(int_calls) :: initial_calls
      call init( map, fnv_1_hasher )
      initial_calls = calls (map)
      print *, "INITIAL_CALLS =  ", initial_calls
    end program demo_calls
```


#### `entries` - Returns the number of entries in a hash map

##### Status

Experimental

##### Description

Returns the number of entries in a hash map.

##### Syntax

`value = [[stdlib_hashmap_chaining:entries]]( map )`

##### Class

Pure function

##### Argument

`map` - shall be an expression of type `chaining_hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be an integer of kind `int_index`.

##### Result value

The result will be the number of entries in the hash map.

##### Example

```fortran
    program demo_entries
      use stdlib_hashmap_chaining, only: &
         chaining_hashmap_type, entries, init, int_index, &
         fnv_1_hasher
      implicit none
      type(chaining_hashmap_type) :: map
      type(int_index) :: initial_entries
      call init( map, fnv_1_hasher )
      initial_entries = entries (map)
      print *, "INITIAL_ENTRIES =  ", initial_entries
    end program demo_entries
```


#### `get_other_data` - Returns other data belonging to the inverse table index

##### Status

Experimental

##### Description

Returns the other data associated with the inverse table index,

##### Syntax

`value = [[stdlib_hashmap_chaining:get_other_data)]]( map, inmap, other )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar expression of type
  `chaining_hashmap_type`. It is an `intent(in)` argument. It will be
  the hash map used to store and access the other data.

`inmap`: shall be a scalar integer expression of kind `int_index`. It
  is an `intent(in)` argument. It should be a non-zero `inmap`
  returned by either procedure `in_map` or `map_entry`.

* If `inmap` is zero, or the corresponding `key` has been deleted
from the map, `other` is undefined.

`other`: shall be a variable of type `other_data`.
  It is an `intent(out)` argument. It is the other data associated
  with the `inmap` index.

* The following is an example of the retrieval of other data
  associated with an inverse table index:


##### Example

```Fortran
    program demo_get_other_data
        use, intrinsic:: iso_fortran_env, only: &
            int8
        use stdlib_hashmap_chaining, only: &
            chaining_hashmap_type, fnv_1_hasher, get, get_other_data, &
            int_index, key_type, map_entry, other_type, set
        integer(int_index)           :: inmap
        type(key_type)               :: key
        type(other_type)             :: other
        type(chaining_hashmap_type) :: map
		integer(int8), allocatable :: data(:)
        call init( map, fnv_1_hasher )
        call set( key, [ 0_int8, 1_int8, 2_int8, 3_int8, 4_int8 ] )
        call set( other, [ 4_int8, 3_int8, 2_int8, 1_int8 ] )
        call map_entry( map, inmap, key, other )
        if ( inmap /= 0 ) then
            call get_other_data( map, inmap, other )
        else
            stop 'Invalid inmap'
        end if
        call get( other, data )
        print *, 'Other data = ', data
    end program demo_get_other_data
```


#### `in_map` - searches a map for the presence of a key

##### Status

Experimental

##### Description

Searches a hash map for the presence of a key and returns the
associated index into the inverse table.

##### Syntax

`call [[stdlib_hashmap_chaining:in_map]]( map, inmap, key )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type `chaining_hashmap_type`. It
  is an `intent(inout)` argument. It will be the hash map used to
  store and access the entries.

`inmap`: shall be a scalar integer variable of  kind `int_index`. It is
  an `intent(out)` argument. It will be 0 if `key` is not found,
  otherwise it will be the one's based index to the location of `key`
  in the hash map's inverse array.

`key`: shall be a scalar expression of type `key_type`. 
  It is an `intent(in)` argument. It is the entry's key to be searched
  for in the hash map.

* The following is an example of the retrieval of other data associated with
  a key:

##### Example

```Fortran
    program demo_in_map
        use, intrinsic:: iso_fortran_env, only: &
            int8
        use stdlib_hashmap_chaining, only: &
            chaining_hashmap_type, fnv_1_hasher, in_map, &
            int_index, key_type, map_entry, other_type, set
        integer(int_index)           :: inmap
        type(key_type)               :: key
        type(other_type)             :: other
        type(chaining_hashmap_type) :: map
        call init( map, fnv_1_hasher )
        call set( key, [ 0_int8, 1_int8, 2_int8, 3_int8, 4_int8 ] )
        call set( other, [ 4_int8, 3_int8, 2_int8, 1_int8 ] )
        call map_entry( map, inmap, key, other )
        if ( inmap /= 0 ) then
            call in_map( map, inmap, key
            if ( inmap \= 0 ) then
			   print *, 'INMAP = ', inmap
            else
                stop 'Invalid inmap from in_map call'
        else
            stop 'Invalid inmap from map_entry call'
        end if
    end program demo_in_map
```

#### init - initializes a hash map

##### Status

Experimental

##### Description

Initializes a `chaining_hashmap_type` object.

##### Syntax

`call [[stdlib_hashmap_chaining:init]](  map, hasher [, slots_bits, status ] ] )`

####@# Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type
  `chaining_hashmap_type`. It is an `intent(out)` argument. It will
  be a hash map used to store and access the entries.

`hasher`: shall be a procedure with interface `hash_fun`.
  It is an `intent(in)` argument. It is the procedure to be used to
  generate the hashes for the table from the keys of the entries.

`slots_bits` (optional): shall be a scalar default integer 
  expression. It is an `intent(in)` argument. The initial number of
  slots in the table will be `2**slots_bits`.

* `slots_bits` shall be a positive default integer less than
  `max_bits`, otherwise processing stops with an informative
  error code.

* If `slots_bits` is absent then the effective value for `slots_bits`
  is `default_slots_bits`.

`status` (optional): shall be a scalar integer variable of kind
`int32`. It is an `intent(out)` argument. On return if present it
shall have an error code value.

* If map was successfully initialized then `status` has the value
`success`.

* If allocation of memory for the `map` arrays fails then `status`
has the value `alloc_fault`.

* If `slot_bits < 6` or `slots_bits > max_bits` then `status`
  has the value of `array_size_error`.

* If `status` is absent, but `status` would have a value other than
`success`, then processing stops with an informative stop code.

##### Example

```fortran
    program demo_init
        use stdlib_hash_tables, only: &
            chaining_map_type, fnv_1_hasher &
            init
        type(fnv_1a_type)       :: fnv_1
        type(chaining_map_type) :: map
        call init( map,           &
                   fnv_1a,        &
                   slots_bits=10 )
    end program demo_init
```


#### `loading` - Returns the ratio of entries to slots

##### Status

Experimental

##### Description

Returns the ratio of the number of entries relative to the number of
slots in a hash map.

##### Syntax

`value = [[stdlib_hashmap_chaining:loading]]( map )`

##### Class

Pure function

##### Argument

`map` - shall be an expression of type `chaining_hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be a default real.

##### Result value

The result will be the ratio of the number of entries relative to the
number of slots in the hash map.

##### Example

```fortran
    program demo_loading
      use stdlib_hashmap_chaining, only: &
         chaining_hashmap_type, init, int_index, &
         fnv_1_hasher, loading
      implicit none
      type(chaining_hashmap_type) :: map
      real :: ratio
      call init( map, fnv_1_hasher )
      ratio = loading (map)
      print *, "Initial loading =  ", ratio
    end program demo_loading
```

#### `map_entry` - inserts an entry into the hash map

##### Status

Experimental

##### Description

Inserts an entry into the hash map if it is not already present.

##### Syntax

`call [[stdlib_hashmap_chaining:map_entry]]( map, inmap, key[, other ])`


##### Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type `chaining_hashmap_type`. It
is an `intent(inout)` argument. It is the hash map to receive the
entry.

`inmap`: shall be an integer scalar variable of kind `int_index`. It is
  an `intent(out)` argument. It is the index to the table's inverse array
  associated with the `key`.

`key`: shall be either a scalar expression of type `key_type`.
  It is an `intent(in)` argument. It is the key for the entry to be
  placed in the table.

`other` (optional): shall be a scalar expression of type `other_type`.
  It is an `intent(in)` argument. If present it is the other data to be
  associated with the `key`.

* If `key` is already present in `map` then the presence of `other` 
is ignored.

##### Example

```fortran
    program demo_map_entry
        use, intrinsic:: iso_fortran_env, only: &
            int8
        use stdlib_hashmap_chaining, only: &
		    chaining_hashmap_type, fnv_1_hasher, init, &
        	int_index, key_type, map_entry, other_type, set
        type(chaining_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,           &
                   fnv_1_hasher,  &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        print *, 'INMAP = ', inmap
    end program demo_map_entry
```

#### `map_probes` - returns the number of hash map probes

##### Status

Experimental

##### Description

Returns the total number of table probes on a hash map

##### Syntax

`Result = [[stdlib_hashmap_chaining:map_probes]]( map )`

##### Class

Pure function

##### Argument

`map`: shall be a scalar integer expression of type
`chaining_hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_probes`.

##### Result value

The result is the number of probes of `map`.

##### Example

```fortran
    program demo_probes
      use stdlib_hashmap_chaining, only: &
         chaining_hashmap_type, init, int_index, &
         fnv_1_hasher, probes
      implicit none
      type(chaining_hashmap_type) :: map
      real :: ratio
      call init( map, fnv_1_hasher )
      ratio = probes (map)
      print *, "Initial probes =  ", ratio
    end program demo_probes
```


#### rehash - changes the hashing function

##### Status

Experimental

##### Description

Changes the hashing function for the table entries to that of `hasher`.

##### Syntax

`call [[stdlib_hashmap_chaining:rehash]]( map, hasher )`

##### Class

Subroutine

##### Arguments

`map` : shall be a scalar variable of type `chaining_hashmap_type`.
It is an `intent(inout)` argument. It is the hash map whose hashing 
method is to be changed.

`hasher`: shall be a function of interface `hasher_fun`.
It is the hash method to be used by `map`.

##### Example

```fortran
    program demo_rehash
        use stdlib_hashmap_chaining, only: &
		    chaining_hashmap_type, fnv_1_hasher, fnv_1a_hasher,&
            init, int_index, key_type, map_entry, other_type, &
            rehash, set
        type(chaining_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,           &
                   fnv_1_hasher,  &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        call rehash( map, fnv_1a_hasher )
    end program demo_rehash
```

#### `remove_entry` - removes an entry from the hash map

##### Status

Experimental

##### Description

Removes an entry from a hash map, `map`.

##### Syntax

`call [[stdlib_hashmap_chaining:remove_entry]]( map, inmap )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type `chaining_hashmap_type`.
It is an `intent(inout)` argument. It is the hash map with the element 
to be removed.

`inmap`: shall be a scalar integer expression of  kind `int_index`. It
is an `intent(in)` argument. It is the non-zero index to the inverse
table returned by `in_map` or `map_entry` identifying the entry to be
removed.

* If `inmap` is zero, or the corresponding `key` has been deleted
from the map, or the `map` has been rehashed subsequent to the
generation of `inmap`, `other` is undefined.


##### Example

```fortran
    program demo_remove_entry
        use stdlib_hashmap_chaining, only: &
		    chaining_hashmap_type, fnv_1_hasher, fnv_1a_hasher,&
            init, int_index, key_type, map_entry, other_type, &
            remove_entry, set
        type(chaining_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,           &
                   fnv_1_hasher,  &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        call remove_entry( map, inmap )
    end program demo_remove_entry
```

#### `set_other_data` - replaces the other dataa for an entry

##### Status

Experimental

##### Description

Replaces the other data for the entry at index `inmap` in the 
inverse table.

##### Syntax

`call [[stdlib_hashmap_chaining:set_other_data]]( map, inmap, other )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type `chaining_hashmap_type`. It
is an `intent(inout)` argument. It will be a hash map used to store
and access the entry's data.

`inmap`: shall be a scalar integer expression of  kind `int_index`. It
is an `intent(in)` argument. It is the non-zero index in the inverse
table to the entry of interest as returned by `ìn_map` or `map_entry`.

`other`: shall be a scalar expression of type `other_type`.
It is an `intent(in)` argument. It is the data to be stored as
the other data for the entry at the `inmap` index.

* If  unable to set the other data associated with `inmap`, either
  because `inmap` is not associated with a valid entry or because of
  allocation problems, then processing will stop with an informative
  stop code.

##### Example

```fortran
    program demo_set_other_data
        use stdlib_hashmap_chaining, only: &
		    chaining_hashmap_type, fnv_1_hasher, fnv_1a_hasher,&
            init, int_index, key_type, map_entry, other_type, &
            set, set_other_data
        type(chaining_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,           &
                   fnv_1_hasher,  &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        Call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        call set( other, [ 17_int8, 5_int8, 6_int8, 15_int8, 40_int8 ] )
        call set_other_data( map, inmap, other )
    end program demo_set_other_data
```

#### `slots` - returns the number of hash map probes

##### Status

Experimental

##### Description

Returns the total number of slots on a hash map

##### Syntax

`Result = [[stdlib_hashmap_chaining:slots]]( map )`

##### Class

Pure function

##### Argument

`map`: shall be a scalar expression of type
`chaining_hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_index`.

##### Result value

The result is the number of slots in `map`.

##### Example

```fortran
    program demo_probes
      use stdlib_hashmap_chaining, only: &
         chaining_hashmap_type, init, int_index, &
         fnv_1_hasher, slots
      implicit none
      type(chaining_hashmap_type) :: map
      integer(int_index) :: initial_slots
      call init( map, fnv_1_hasher )
      initial_slots = slots (map)
      print *, "Initial slots =  ", initial_slots
    end program demo_probes
```


#### `total_depth` - returns the total depth of the hash map entries

##### Status

Experimental

##### Description

Returns the total number of one's based offsets of slot entries from
their slot index for a hash map

##### Syntax

`Result = [[stdlib_hashmap_chaining:total_depth]]( map )`

##### Class

Pure function

##### Argument

`map`: shall be a scalar expression of type
`chaining_hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_depth`.

##### Result value

The result is the total number of one's based offsets of slot entries
from their slot index the map.

##### Example

```fortran
    program demo_probes
      use stdlib_hashmap_chaining, only: &
         chaining_hashmap_type, init, int_index, &
         fnv_1_hasher, total_depth
      implicit none
      type(chaining_hashmap_type) :: map
      integer(int_depth) :: initial_depth
      call init( map, fnv_1_hasher )
      initial_depth = total_depth (map)
      print *, "Initial total depth =  ", initial_depth
    end program demo_probes
```


#### `unmap` - returns a copy of the key

##### Status

Experimental

##### Description

Returns a copy of the key associated with an index to the 
inverse table.

##### Syntax

`call [[stdlib_hashmap_chaining:unmap]]( map, inmap, key )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar expression of type `chaining_hashmap_type`.
It is an `intent(in)` argument. It is the hash map whose entry
is unmapped.

`inmap`: shall be a scalar integer expression of kind `int_index`. It
is an `intent(in)` argument. It is the non-zero index to the inverse
table identifying the unmapped entry as returned by `ìn_map` or
`map_entry`. 

* If `inmap` is zero or `key` has been eliminated from the `map`
subsequent to the generation of `inmap`, or `mp` has been rehashed
subsequent to the generation of `inmap` then `key` is undefined.

`key`: shall be a variable of type `key_type`
`int8`, or an allocatable length default character. It is an
`intent(out)` argument. It is the `key` associated with the entry at
index `inmap` in the inverse table.

##### Example

```fortran
    program demo_unmap
        use stdlib_hashmap_chaining, only: &
		    chaining_hashmap_type, fnv_1_hasher, fnv_1a_hasher,&
            init, int_index, key_type, map_entry, other_type, &
            set, unmap
        type(chaining_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,           &
                   fnv_1_hasher,  &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        call unmap( map, inmap, key )
    end program demo_unmap
```

#### `valid_index` - indicates whether `inmap` is a valid index

##### Status

Experimental

##### Description

Returns a flag indicating whether `inmap` is a valid index in the 
inverse table.

##### Syntax

`result = [[stdlib_hashmap_chaining:valid_index]]( map, inmap )`

##### Class

Pure function.

##### Arguments

`map`: shall be a scalar expression of type `chaining_hashmap_type`.
It is an `intent(in)` argument. It is the hash map whose inverse
table is examined.

`inmap`: shall be a scalar integer expression of kind `int_index`. It
is an `intent(in)` argument. It is an index to the inverse table whose
validity is being examined.

##### Result character

The result is a default logical scalar.

##### Result value

The result is `.true.` if `inmap` is a valid index to the inverse
table of `map` and `.false.` otherwise.

##### Example

```fortran
    program demo_valid_index
      use stdlib_hashmap_chaining, only: &
         chaining_hashmap_type, init, int_index, &
         fnv_1_hasher, valid_index
      implicit none
      type(chaining_hashmap_type) :: map
      integer(int_index) ::  inmap
      logocal :: valid
      call init( map, fnv_1_hasher )
      inmap = 10
      valid = valid_index (map, inmap)
      print *, "Initial index of 10 valid for empty map =  ", valid
    end program demo_valid_index
```


## The `stdlib_hashmap_open` module

The `stdlib_hashmap_open` module provides access to all the
public entities in the `stdlib_hashmap_wrappers` module. It
also defines a public data type and associated procedures and
constants that implement a simple hash map using
linear open addressing hashing. The derived type is
`open_hashmap_type`. It provides
procedures to manipulate the structure of the hash map:
`init`, `map_entry`, `rehash`, and `set_other_data`. It
provides procedures to inquire about entries in the hash map:
`get_other_data`, `in_map`, `unmap` and `valid_index`. Finally it
provides procedures to inquire about the overall structure and
performance of the table:`calls`, `entries`, `get_other_data`,
`loading`, `relative_loading`, `slots`, and `total_depth`. The module 
also defines a number of public constants: `inmap_probe_factor`,
`map_probe_factor`, `default_bits`, `max_bits`, `int_calls`,
`int_depth`, `int_index`, `int_probes`, `load_factor`, `success`,
`alloc_fault`, `array_size_error`, and `real_value_error`.

### The `stdlib_hashmap_open` module's public constants

The module defines several categories of public constants. Some are
used to parameterize the empirical slot expansion code. Others
parameterize the slots table size. Some are used to define
integer kind values for different applications. Finally, some are used
to report errors or success.

The constants `inmap_probe_factor`, `map_probe_factor`, and
`load_factor` are used to parameterize the slot expansion code
used to determine when in a call on the map the number 
of slots need to be increased to decrease the search lengths.
The constant `inmap_probe_factor` is used to determine when
the ratio of the number of map probes to map calls is too large and
the slots need expansion. The constant `map_probe_factor` is used to
determine when inserting a new entry the ratio of the number of map
probes to map calls is too large and the slots need expansion.
Finally, the 
`load_factor` determines the maximum number of entries allowed 
relative to the number of slots prior to automatically resizing the 
table upon entry insertion. The `load_factor` is a tradeoff between 
runtime performance and memory usage, with smaller values of 
`load_factor` having the best runtime performance and larger.values 
the smaller memory footprint, with common choices being `0.575 <=
load_factor <= 0.75`. The `load_factor` currently has a value 
of `0.5625`.

The constants `default_bits`, and
`max_bits` are used to parameterize the table's slots size. The
`default_bits` constant defines the default initial number of slots
with a current value of 6 resulting in an initial `2**6 == 64`
slots. This may optionally be overridden on hash map creation. The
`max_bits` sets the maximum table size as `2**max_bits`. The current
value of `max_bits` is 3o and the table will not work properly if that
value is exceeded.

The constants `int_calls`, `int_depth`, `int_index`, and `int_probes`
are used to define integer kind values for various contexts. The
number of calls are reported and stored in entities of kind
`int_calls`. Currently `int_calls` has the value of `int64`. The
total depth, the number of inquiries needed to access all elements
of the table, is reported and stored in entities of kind
`int_depth`. Currently `int_depth` has the value of `int64`. The
number of entries in the table, is reported and stored in entities of
kind `int_index`. Currently `int_index` has the value of `int32`.
The number of probes, hash map enquiries, are reported and stored in
entities of kind `int_probes`. Currently `int_probes` has the value of
`int64`.

Finally the error codes `success`, `alloc_fault`, and
`array_size_error` are used to report the error status of certain
procedure calls. The `succes` code indicates that no problems were
found. The `alloc_fault` code indicates that a memory allocation
failed. The `array_size_error` indicates that on table
creation `slots_bits` is less than `default_bits` or
greater than `max_bits`.

### The `stdlib_hashmap_open` module's derived types

The `stdlib_hashmap_open` module defines several derived
types. The only public type is the `open_hashmap_type`. There are
three other private derived types used in the implementation of the
public type: `open_map_entry_type`, and `open_map_entry_ptr`.

#### The `open_map_entry_type` derived type

Entities of the type `open_map_entry_type` are used to define
a linked list structure that stores the
key, its other data, the hash of the key, and the resulting index into
the inverse table. The type's definition is below:

```fortran
    type :: open_map_entry_type  ! Open hash map entry type
        private
        integer(int_hash)   :: hash_val ! Full hash value
        type(key_type)      :: key ! The entry's key
        type(other_type)    :: other ! Other entry data
        integer(int_index)  :: index ! Index into inverse table
    end type open_map_entry_type
```
Currently `int_hash` and `int_index` have the value of `int32`.

#### The `open_map_entry_ptr` derived type

The type `open_map_entry_ptr` is used to define the elements of
the hash map that are either empty or link to the linked lists
containing the elements of the table. The type's definition is below:

```fortran
    type open_map_entry_ptr ! Wrapper for a pointer to a open
                             ! map entry type object
        type(open_map_entry_type), pointer :: target => null()
    end type open_map_entry_ptr
```

#### The `open_hashmap_type` derived type

The `open_hashmap_type` derived type implements a separate
open hash map. It provides the elements `calls`, `probes`,
`total_probes`, `entries`, and `slots_bits` to keep track
of the hash map's usage. The array element `slots` serves as the
table proper. The array element `inverse` maps integers to
entries. The linked list entry, `free_list`, keeps track of freed
elements of type `open_map_entry_type`. The list element, `cache`,
stores pools of  `open_map_entry_type` elements for reuse. The
component `hasher` is a pointer to the hash function. Finally the
type-bound procedure, `free_open_map`, serves as a finalizer  for
objects of the type, `open_hashmap_type`.

```fortran
    type :: open_hashmap_type
        private
        integer(int_calls) :: calls = 0
        ! Number of calls
        integer(int_calls) :: probes = 0
        ! Number of probes since last expansion
        integer(int_calls) :: total_probes = 0
        ! Cumulative number of probes
        integer(int_index) :: entries = 0
        ! Number of entries
        integer(int_index) :: index_mask = 2_int_index**default_bits-1
        ! Mask used in linear addressing
        integer(int32)     :: slots_bits = default_bits
        ! Bits used for slots size
        type(open_map_entry_ptr), allocatable :: slots(:)
        ! Array of bucket lists Note # slots=size(slots)
        type(open_map_entry_ptr), allocatable :: inverse(:)
        ! Array of bucket lists (inverses) Note max_elts=size(inverse)
        procedure(hasher_fun), pointer, nopass    :: hasher => fnv_1_hasher
        ! Hash function
    contains
        final                    :: free_open_map
    end type open_hashmap_type
```

### Table of `stdlib_open_ehashmap` procedures

The `stdlib_hashmap_open` module provides procedures in
several categories: a procedure to initialize the map; a procedure to
modify the structure of a map; procedures to modify the content of a
map; procedures to report on the content of a map; and procedures
to report on the structure of the map. The procedures in each category
are listed below.

Procedure to initialize a chaining hash map:

* `init_map( map, hasher[, slots_bits, status]
  )` - Routine to initialize a chaining hash map.

Procedure to modify the structure of a map:

* `rehash( map, hasher )` - Routine to change the hash function
  for a map.

Procedures to modify the content of a map:

* `map_entry( map, inmap, key, other )` - Inserts an entry innto the
  hash map.

* `set_other_data( map, inmap, other )` - Change the other data
  associated with the entry.

Procedures to report the content of a map:

* `get_other_data( map, inmap, other )` - Returns the other data
  associated with the inverse table index 

* `in_map( map, inmap, key )` - Returns the index into the `inverse`
  array associated with the `key` 

* `unmap( map, inmap, key )` - Returns a copy of the key associated
with an index to the inverse table.

* `valid_index(map, inmap)` - Returns a flag indicating whether `inmap`
  is a valid index.

Procedures to report on the structure of the map:

* `calls( map )` - the number of subroutine calls on the hash map.

* `entries( map )`- the number of entries in a hash map.

* `loading( map )` - the number of entries relative to slots in a hash
  map.

* `map_probes( map )` - the total number of table probes on a hash
  map.

* `relative_loading` - the ratio of the map's loading to its
  `load_factor`. 

* `slots( map )` - Returns the number of allocated slots in a hash
  map.

* `total_depth( map )` - Returns the total number of one's based
offsets of slot entries from their slot index


### Specifications of the `stdlib_hashmap_open` procedures

#### `calls` - Returns the number of calls on a hash map

##### Status

Experimental

##### Description

Returns the number of procedure calls on a hash map.

##### Syntax

`value = [[stdlib_hashmap_open:calls]]( map )`

##### Class

Pure function

##### Argument

`map` - shall be an expression of type `open_hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be an integer of kind `int_calls`.

##### Result value

The result will be the number of procedure calls on the hash map.

##### Example

```fortran
    program demo_calls
      use stdlib_hashmap_open, only: &
         open_hashmap_type, calls, init, int_calls, &
         fnv_1_hasher
      implicit none
      type(open_hashmap_type) :: map
      type(int_calls) :: initial_calls
      call init( map, fnv_1_hasher )
      initial_calls = calls (map)
      print *, "INITIAL_CALLS =  ", initial_calls
    end program demo_calls
```


#### `entries` - Returns the number of entries in a hash map

##### Status

Experimental

##### Description

Returns the number of entries in a hash map.

##### Syntax

`value = [[stdlib_hashmap_open:entries]]( map )`

##### Class

Pure function

##### Argument

`map` - shall be an expression of type `open_hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be an integer of kind `int_index`.

##### Result value

The result will be the number of entries in the hash map.

##### Example

```fortran
    program demo_entries
      use stdlib_hashmap_open, only: &
         open_hashmap_type, entries, init, int_index, &
         fnv_1_hasher
      implicit none
      type(open_hashmap_type) :: map
      type(int_index) :: initial_entries
      call init( map, fnv_1_hasher )
      initisl_entries = entries (map)
      print *, "INITIAL_ENTRIES =  ", initial_entries
    end program demo_entries
```


#### `get_other_data` - Returns other data belonging to the inverse table index

##### Status

Experimental

##### Description

Returns the other data associated with the inverse table index,

##### Syntax

`value = [[stdlib_hashmap_open:get_other_data)]]( map, inmap, other )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar expression of type
  `open_hashmap_type`. It is an `intent(in)` argument. It will be
  the hash map used to store and access the other data.

`inmap`: shall be a scalar integer expression of kind `int_index`. It
  is an `intent(in)` argument. It should be the `inmap` returned by the
  procedure `in_map` or `map_entry`.

* If `inmap` is zero or `key` has been removed subsequent to the 
generation of `inmap`, or `map` hasbeen rehashed ssubsequent to the
generation of `inmap`, then `other` is undefined. 

`other`: shall be a variable of type `other_data`.
  It is an `intent(out)` argument. It is the other data associated
  with the `inmap` index.

* The following is an example of the retrieval of other data
  associated with an inverse table index:

##### Example

```Fortran
    program demo_get_other_data
        use, intrinsic:: iso_fortran_env, only: &
            int8
        use stdlib_hashmap_open, only: &
            open_hashmap_type, fnv_1_hasher, get, get_other_data, &
            int_index, key_type, map_entry, other_type, set
        integer(int_index)         :: inmap
        type(key_type)             :: key
        type(other_type)           :: other
        type(open_hashmap_type)   :: map
		integer(int8), allocatable :: data(:)
        call init( map, fnv_1_hasher )
        call set( key, [ 0_int8, 1_int8, 2_int8, 3_int8, 4_int8 ] )
        call set( other, [ 4_int8, 3_int8, 2_int8, 1_int8 ] )
        call map_entry( map, inmap, key, other )
        if ( inmap /= 0 ) then
            call get_other_data( map, inmap, other )
        else
            stop 'Invalid inmap'
        end if
		call get( other, data )
        print *, 'Other data = ', data
    end program demo_get_other_data
```


#### `in_map` - searches a map for the presence of a key

##### Status

Experimental

##### Description

Searches a hash map for the presence of a key and returns the
associated index into the inverse table.

##### Syntax

`call [[stdlib_hashmap_open:in_map]]( map, inmap, key )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type `open_hashmap_type`. It
  is an `intent(inout)` argument. It will be the hash map used to
  store and access the entries.

`inmap`: shall be a scalar integer variable of  kind `int_index`. It is
  an `intent(out)` argument. It will be 0 if `key` is not found,
  otherwise it will be the one's based index to the location of `key`
  in the hash map's inverse array.

`key`: shall be a scalar expression of type `key_type`. 
  It is an `intent(in)` argument. It is the entry's key to be searched
  for in the hash map.

* The following is an example of the retrieval of other data associated with
  a key:

##### Example

```Fortran
    program demo_in_map
        use, intrinsic:: iso_fortran_env, only: &
            int8
        use stdlib_hashmap_open, only: &
            open_hashmap_type, fnv_1_hasher, in_map, &
            int_index, key_type, map_entry, other_type, set
        integer(int_index)           :: inmap
        type(key_type)               :: key
        type(other_type)             :: other
        type(open_hashmap_type) :: map
        call init( map, fnv_1_hasher )
        call set( key, [ 0_int8, 1_int8, 2_int8, 3_int8, 4_int8 ] )
        call set( other, [ 4_int8, 3_int8, 2_int8, 1_int8 ] )
        call map_entry( map, inmap, key, other )
        if ( inmap /= 0 ) then
            call in_map( map, inmap, key
            if ( inmap \= 0 ) then
			   print *, 'INMAP = ', inmap
            else
                stop 'Invalid inmap from in_map call'
        else
            stop 'Invalid inmap from map_entry call'
        end if
    end program demo_in_map
```

#### init - initializes a hash map

##### Status

Experimental

##### Description

Initializes a `open_hashmap_type` object.

##### Syntax

`call [[stdlib_hashmap_open:init]](  map, hasher[, slots_bits, status ] ]`

####@# Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type
  `open_hashmap_type`. It is an `intent(out)` argument. It will
  be a hash map used to store and access the entries.

`hasher`: shall be a procedure with interface `hash_fun`.
  It is an `intent(in)` argument. It is the procedure to be used to
  generate the hashes for the table from the keys of the entries.

`slots_bits` (optional): shall be a scalar default integer 
  expression. It is an `intent(in)` argument. The initial number of
  slots in the table will be `2**slots_bits`.

* `slots_bits` shall be a positive default integer less than
  `max_slots_bits`, otherwise processing stops with an informative
  error code.

* If `slots_bits` is absent then the effective value for `slots_bits`
  is `default_slots_bits`.

`status` (optional): shall be a scalar integer variable of kind
`int32`. It is an `intent(out)` argument. On return, if present, it
shall have an error code value.

* If map was successfully initialized then `status` has the value
`success`.

* If allocation of memory for the `map` arrays fails then `status`
has the value `alloc_fault`.

* If `slot_bits < 6` or `slots_bits > max_bits` then `status`
  has the value of `array_size_error`.

* If `status` is absent, but `status` would have a value other than
`success`, then processing stops with an informative stop code.

##### Example

```fortran
    program demo_init
        use stdlib_hash_tables, only: &
            open_map_type, fnv_1_hasher &
            init
        type(fnv_1a_type)       :: fnv_1
        type(open_map_type) :: map
        call init( map,           &
                   fnv_1a,        &
                   slots_bits=10 )
    end program demo_init
```



#### `loading` - Returns the ratio of entries to slots

##### Status

Experimental

##### Description

Returns the ratio of the number of entries relative to the number of
slots in a hash map.

##### Syntax

`value = [[stdlib_hashmap_open:loading]]( map )`

##### Class

Pure function

##### Argument

`map` - shall be an expression of type `open_hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be a default real.

##### Result value

The result will be the ratio of the number of entries relative to the
number of slots in the hash map.

##### Example

```fortran
    program demo_loading
      use stdlib_hashmap_open, only: &
         open_hashmap_type, init, int_index, &
         fnv_1_hasher, loading
      implicit none
      type(open_hashmap_type) :: map
      real :: ratio
      call init( map, fnv_1_hasher )
      ratio = loading (map)
      print *, "Initial loading =  ", ratio
    end program demo_loading
```

#### `map_entry` - inserts an entry into the hash map

##### Status

Experimental

##### Description

Inserts an entry into the hash map if it is not already present.

##### Syntax

`call [[stdlib_hashmap_open:map_entry]]( map, inmap, key[, other ])`


##### Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type `open_hashmap_type`. It
is an `intent(inout)` argument. It is the hash map to receive the
entry.

`inmap`: shall be an integer scalar variable of kind `int_index`. It is
  an `intent(out)` argument. It is the index to the table's inverse array
  associated with the `key`.

`key`: shall be either a scalar expression of type `key_type`.
  It is an `intent(in)` argument. It is the key for the entry to be
  placed in the table.

`other` (optional): shall be a scalar expression of type `other_type`.
  It is an `intent(in)` argument. If present it is the other data to be
  associated with the `key`.

* If `key` is already present in `map` then the presence of `other` 
is ignored.

##### Example

```fortran
    program demo_map_entry
        use, intrinsic:: iso_fortran_env, only: &
            int8
        use stdlib_hashmap_open, only: &
		    open_hashmap_type, fnv_1_hasher, init, &
        	int_index, key_type, map_entry, other_type, set
        type(open_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,            &
                   fnv_1_hasher,   &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
		print *, 'INMAP = ', inmap
    end program demo_map_entry
```

#### `map_probes` - returns the number of hash map probes

##### Status

Experimental

##### Description

Returns the total number of table probes on a hash map

##### Syntax

`Result = [[stdlib_hashmap_open:map_probes]]( map )`

##### Class

Pure function

##### Argument

`map`: shall be a scalar integer expression of type
`open_hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_probes`.

##### Result value

The result is the number of probes of `map`.

##### Example

```fortran
    program demo_probes
      use stdlib_hashmap_open, only: &
         open_hashmap_type, init, int_index, &
         fnv_1_hasher, probes
      implicit none
      type(open_hashmap_type) :: map
      real :: ratio
      call init( map, fnv_1_hasher )
      ratio = probes (map)
      print *, "Initial probes =  ", ratio
    end program demo_probes
```


#### rehash - changes the hashing function

##### Status

Experimental

##### Description

Changes the hashing function for the table entries to that of `hasher`.

##### Syntax

`call [[stdlib_hashmap_open:rehash]]( map, hasher )`

##### Class

Subroutine

##### Arguments

`map` : shall be a scalar variable of type `open_hashmap_type`.
It is an `intent(inout)` argument. It is the hash map whose hashing 
method is to be changed.

`hasher`: shall be a function of interface `hasher_fun`.
It is the hash method to be used by `map`.

##### Example

```fortran
    program demo_rehash
        use stdlib_hashmap_open, only: &
		    open_hashmap_type, fnv_1_hasher, fnv_1a_hasher,&
            init, int_index, key_type, map_entry, other_type, &
            rehash, set
        type(open_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,           &
                   fnv_1_hasher,  &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        call rehash( map, fnv_1a_hasher )
    end program demo_rehash
```

#### `relative_loading` - Returns the ratio of `loading` to `load_factor`

##### Status

Experimental

##### Description

Returns the ratio of the loadings relative to the open hash map's
`load_factor`. 

##### Syntax

`value = [[stdlib_hashmap_open:relative_loading]]( map )`

##### Class

Pure function

##### Argument

`map` - shall be an expression of type `open_hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be a default real.

##### Result value

The result will be the ratio of the number of entries relative to the
number of slots in the hash map relative to the `load_factor`.

##### Example

```fortran
    program demo_relative_loading
      use stdlib_hashmap_open, only: &
         open_hashmap_type, init, int_index, &
         fnv_1_hasher, loading
      implicit none
      type(open_hashmap_type) :: map
      real :: ratio
      call init( map, fnv_1_hasher )
      ratio = relative_loading (map)
      print *, "Initial relative loading =  ", ratio
    end program demo_relative_loading
```


#### `set_other_data` - replaces the other dataa for an entry

##### Status

Experimental

##### Description

Replaces the other data for the entry at index `inmap` in the 
inverse table.

##### Syntax

`call [[stdlib_hashmap_open:set_other_data]]( map, inmap, other )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar variable of type `open_hashmap_type`. It
is an `intent(inout)` argument. It will be a hash map used to store
and access the entry's data.

`inmap`: shall be a scalar integer expression of  kind `int_index`. It
is an `intent(in)` argument. It is the non-zero index in the inverse
table to the entry of interest.

* `inmap` will be invalid if zero, or `key` has been deleted from the
  map subsequent to the generation of `inmap`, or `map` has been
  rehashed subsequent to the generation of `inmap`.

`other`: shall be a scalar expression of type `other_type`.
It is an `intent(in)` argument. It is the data to be stored as
the other data for the entry at the `inmap` index.

* If  unable to set the other data associated with `inmap`, either
  because `inmap` is not associated with a valid entry or because of
  allocation problems, then processing will stop with an informative
  stop code.

##### Example

```fortran
    program demo_set_other_data
        use stdlib_hashmap_open, only: &
		    open_hashmap_type, fnv_1_hasher, fnv_1a_hasher,&
            init, int_index, key_type, map_entry, other_type, &
            set, set_other_data
        type(open_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,          &
                   fnv_1_hasher, &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        call set( other, [ 17_int8, 5_int8, 6_int8, 15_int8, 40_int8 ] 
        call set_other_data( map, inmap, other )
    end program demo_set_other_data
```

#### `slots` - returns the number of hash map probes

##### Status

Experimental

##### Description

Returns the total number of slots on a hash map

##### Syntax

`Result = [[stdlib_hashmap_open:slots]]( map )`

##### Class

Pure function

##### Argument

`map`: shall be a scalar expression of type
`open_hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_index`.

##### Result value

The result is the number of slots in `map`.

##### Example

```fortran
    program demo_probes
      use stdlib_hashmap_open, only: &
         open_hashmap_type, init, int_index, &
         fnv_1_hasher, slots
      implicit none
      type(open_hashmap_type) :: map
      integer(int_index) :: initial_slots
      call init( map, fnv_1_hasher )
      initial_slots = slots (map)
      print *, "Initial slots =  ", initial_slots
    end program demo_probes
```


#### `total_depth` - returns the total depth of the hash map entries

##### Status

Experimental

##### Description

Returns the total number of one's based offsets of slot entries from
their slot index for a hash map

##### Syntax

`Result = [[stdlib_hashmap_open:total_depth]]( map )`

##### Class

Pure function

##### Argument

`map`: shall be a scalar expression of type
`open_hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_depth`.

##### Result value

The result is the total number of one's based offsets of slot entries
from their slot index the map.

##### Example

```fortran
    program demo_probes
      use stdlib_hashmap_open, only: &
         open_hashmap_type, init, int_index, &
         fnv_1_hasher, total_depth
      implicit none
      type(open_hashmap_type) :: map
      integer(int_depth) :: initial_depth
      call init( map, fnv_1_hasher )
      initial_depth = total_depth (map)
      print *, "Initial total depth =  ", initial_depth
    end program demo_probes
```


#### `unmap` - returns a copy of the key

##### Status

Experimental

##### Description

Returns a copy of the key associated with an index to the 
inverse table.

##### Syntax

`call [[stdlib_hashmap_open:unmap]]( map, inmap, key )`

##### Class

Subroutine

##### Arguments

`map`: shall be a scalar expression of type `open_hashmap_type`.
It is an `intent(in)` argument. It is the hash map whose entry
is unmapped.

`inmap`: shall be a scalar integer expression of kind `int_index`. It
is an `intent(in)` argument. It is the non-zero index to the inverse
table identifying the unmapped entry.

* If `inmap` is zero or `key` hass been eliminated from the table
  subsequent to the generation of `inmap`, or `map` has been rehashed
  subsequent to the generation of `inmap`, `other` is undefined.

`key`: shall be a variable of type `key_type`
`int8`, or an allocatable length default character. It is an
`intent(out)` argument. It is the `key` associated with the entry at
index `inmap` in the inverse table.

##### Example

```fortran
    program demo_unmap
        use stdlib_hashmap_open, only: &
		    open_hashmap_type, fnv_1_hasher, fnv_1a_hasher,&
            init, int_index, key_type, map_entry, other_type, &
            unmap
        type(open_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        integer(int_index)  :: inmap
        call init( map,           &
                   fnv_1_hasher,  &
                   slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, [ 1_int8, 5_int8, 3_int8, 15_int8 ] )
        call map_entry( map, inmap, key, other )
        call unmap( map, inmap, key )
    end program demo_unmap
```

#### `valid_index` - indicates whether `inmap` is a valid index

##### Status

Experimental

##### Description

Returns a flag indicating whether `inmap` is a valid index in the 
inverse table.

##### Syntax

`result = [[stdlib_hashmap_open:valid_index]]( map, inmap )`

##### Class

Pure function.

##### Arguments

`map`: shall be a scalar expression of type `open_hashmap_type`.
It is an `intent(in)` argument. It is the hash map whose inverse
table is examined.

`inmap`: shall be a scalar integer expression of kind `int_index`. It
is an `intent(in)` argument. It is the index to the inverse table whose
validity is being examined.

##### Result character

The result is a default logical scalar.

##### Result value

The result is `.true.` if `inmap` is a valid index to the inverse
table of `map` and `.false.` otherwise.


##### Example

```fortran
    program demo_valid_index
      use stdlib_hashmap_open, only: &
         open_hashmap_type, init, int_index, &
         fnv_1_hasher, valid_index
      implicit none
      type(open_hashmap_type) :: map
      integer(int_index) ::  inmap
      logocal :: valid
      call init( map, fnv_1_hasher )
      inmap = 10
      valid = valid_index (map, inmap)
      print *, "Initial index of 10 valid for empty map =  ", valid
    end program demo_valid_index
```

