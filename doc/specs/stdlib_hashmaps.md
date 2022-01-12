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

The Fortran Standard Library provides two modules for the
implementation of simple hash maps. These maps only accept hash
functions with a single argument, the key, and yield a 32 bit
hash code. The modules will need to be modified if it is desired to
use hash functions with a different API. The two modules are:
`stdlib_hashmap_wrappers`, and `stdlib_hashmaps` corresponding to the
files: `stdlib_hashmap_wrappers.f90`, and `stdlib_hashmaps.f90`

The module `stdlib_hashmap_wrappers` provides types and procedures for
use by `stdlib_hashmaps`. It provides an
interface to the 32 bit hash functions of the Standard Library module,
`stdlib_hash_32bit`, and provides wrappers to some of the
hash functions so that they no longer need to be supplied seeds. It
also defines two data types used to store information in the hash
maps, the `key_type` and the `other_type`. The `key_type` is used to
define keys that, in turn, are used to identify the data entered into
a hash map. The `other_type` is intended to contain the other data
associated with the key.

The module `stdlib_hashmaps` defines the API for a parent datatype,
`hashmap_type` and two extensions of that hash map type:
`chaining_hashmap_type` and `open_hashmap_type`.

The `hashmap_type` defines the Application Programers
Interface (API) for the procedures used by its two extensions. It
explicitly defines five non-overridable procedures. It also defines
the interfaces for eleven deferred procedures. It does not define the
finalization routines for the two extension types, or one routine
provided by the `open_hashmap_type`.

The `chaining_hashmap_type` uses separate chaining with linked
lists to deal with hash index collisions. In separate chaining the
colliding indices are handled by using linked lists with their roots
at the hash index. The `chaining_hashmap_type` procedures are
implemented in the module `stdlib_hashmap_chaining` corresponding
to the file, `stdlib_hashmap_chaining.f90`.

The `open_hashmap_type`
uses linear open addressing to deal with hash index collisions. In
linear open addressing the colliding indices are
handled by searching from the initial hash index in increasing
steps of one (modulo the hash map size) for an open map slot.
The `open_hashmap_type` procedures are implemented in the submodule
`stdlib_hashmap_open` corresponding to the file
`stdlib_hashmap_open.f90`.

The maps use powers of two for their slot sizes, so that the function,
`fibonacci_hash`, can 
be used to map the hash codes to indices in the map. This is
expected to be more efficient than prime number mapping using a
modulo operation, and reduces the requirement that the hash
function need to do a good job randomizing its lower order bits.
They do require a good randomizing hash method for good performance.
Both adjust the map size to reduce collisions, based on 
the ratio of the number of hash map probes to the number of subroutine 
calls.
Wile the maps make extensive use of pointers internally, a private
finalization subroutine avoids memory leaks.
The maps can take entry keys of type `key_type`, and other data of the
type `other_type`.
The maps allow the addition, removal, and lookup of entries, and the
inclusion of data in addition to the entry key.

## The `stdlib_hashmap_wrappers` module

The `stdlib_hashmap_wrappers` module provides data types to
represent keys and associated data stored in a module, but is also, a
wrapper for the `stdlib_hash_32bit` module. It allows
direct access to the `stdlib_hash_32bit` procedures:
`fibonacci_hash`, `fnv_1_hasher`, `fnv_1a_hasher`; and provides
wrapper functions, `seeded_nmhash32_hasher`,
`seeded_nmhash32x_hasher`, and `seeded_water_hasher` to the hash
functions: `nmhash32`, `nmhash32x`, and `water_hash`, respectively. It
defines an interface, `hasher_fun`, compatible with the hash functions
that take a `non-scalar key`. It defines one integer constant used
as a kind value,`int_hash`. It also defines two types, `key_type` and
`other_type`, and associated procedures, for storing and manipulating
keys and their associated data.

### The `stdlib_hashmap_wrappers`'s constant, `int_hash`

The constant `int_hash` is used to define the integer kind value for
the returned hash codes and variables used to access them. It
currently has the value, `int32`.

### The `stdlib_hashmap_wrappers`' module's derived types

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

The  module also defines six procedures for those types: `copy_key`,
`copy_other`, `equal_keys`, `free_key`, `free_other`, `get`, and
`set`, and one operator, `==`,
for use by the hash maps to manipulate or inquire of components of
those types.

### Table of `stdlib_hashmap_wrappers` procedures

The  `stdlib_hashmap_wrappers` module provides procedures in
several categories: procedures to manipulate data of the `key_type`;
procedures to manipulate data of the `other_type`, and 32 bit hash
functions for keys. The procedures in each category are listed
below. It also provides an operator to compare two key type values for
equality. 

Procedures to manipulate `key_type` data:

* `copy_key( key_in, key_out )` - Copies the contents of the key,
  `key_in`, to contents of the key, `key_out`.

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

Operator to compare two `key_type` values for equality

* `key1 == key2` - compares `key1' with 'key2' for equality

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
          copy_key, operator(==)equal_keys, key_type
      use iso_fortran_env, only: int8
      implicit none
      integer(int8) :: i, value(15)
      type(key_type) :: key_in, key_out
      value = [(i, i = 1, 15)]
      call set( key_in, value )
      call copy_key( key_in, key_out )
      print *, "key_in == key_out = ", key_in == key_out
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
intermittent.
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
intermittent.
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
      integer(int8) :: i, value(15)
      type(key_type) :: key_in, key_out
      value = [(i, i=1, 15)]
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

#### `operator(==)` - Compares two keys for equality 

##### Status 

Experimental 

##### Description 

Returns `.true.` if two keys are equal, and `.false.` otherwise. 

##### Syntax 

`test = [stdlib_hashmap_wrappers:key1==key2]`

##### Class 

Pure operator. 

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
          copy_key, operator(==), key_type, set 
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
      print *, "key_in == key_out = ", key_in == key_out 
    end program demo_equal_keys 
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

`value`: if the first argument is `key` `value` shall be a default
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


## The `stdlib_hashmaps` module

The `stdlib_hashmaps` module defines three public data types,
associated procedures and constants that implement two simple hash map
types using separate chaining hashing and open addressing hashing. The
derived type `hashmap_type` is the parent type to its two
extensions: `chaining_hashmap_type` and `open_hashmap_type`.
The extension types provide 
procedures to manipulate the structure of a hash map object:
`init`, `map_entry`, `rehash`, `remove_entry`, and
`set_other_data`. They also provide procedures to inquire about
entries in the hash map: `get_other_data`, `in_map`, `unmap` and
`valid_index`. Finally they provide procedures to inquire about the
overall structure and performance of the hash map object:`calls`,
`entries`, `get_other_data`, `loading`, `slots`, and
`total_depth`. The module also defines a number of public constants:
`probe_factor`, `load_factor`, `map_probe_factor`, `default_bits`,
`max_bits`, `int_calls`, `int_depth`, `int_index`,
`int_probes`, `success`, `alloc_fault`, and `array_size_error`.

### The `stdlib_hashmaps` module's public constants

The module defines several categories of public constants. Some are
used to parameterize the empirical slot expansion code. Others
parameterize the slots table size. Some are used to define
integer kind values for different applications. Finally, some are used
to report errors or success.

The constants `probe_factor`, and `map_probe_factor` are used to
parameterize the slot expansion code used to determine when in a
in a procedure call the number 
of slots need to be increased to decrease the search path for an
entry. The constant `probe_factor` is used to determine when
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

The constant `load_factor` is only used by the `open_hashmap_type`. It
specifies the maximum fraction of the available slots that may be
filled before expansion occurs. The current `load_factor = 0.5625` so
the current implementation of `open_hashmap_type` can only hold a
little more than `2**29` entries.

Finally the error codes `success`, `alloc_fault`, and
`array_size_error` are used to report the error status of certain
procedure calls. The `succes` code indicates that no problems were
found. The `alloc_fault` code indicates that a memory allocation
failed. Finally the `array_size_error` indicates that on table
creation `slots_bits` is less than `default_bits` or
greater than `max_bits`.

### The `stdlib_hashmaps` module's derived types

The `stdlib_hashmaps` module defines three public derived types and
seven private types used in the implementation of the public
types. The public types are the abstract `hashmap_type` and its
extensions: `chaining_hashmap_type` and `open_hashmap_type`. The three
private derived types, `chaining_map_entry_type`,
`chaining_map_entry_ptr`, and `chaining_map_entry_pool` are used in
the implementation of the `chaining_hashmap_type` public type. The
four private derived types, `open_map_entry_type`,
`open_map_entry_list`, `open_map_entry_ptr`, and `open_map_entry_pool`
are used in the implementation of the `open_hashmap_type` public
type. Each of these types are described below. 

#### The `hashmap_type` abstract type

The `hashmap_type` abstract type serves as the parent type for the two
types `chaining_hashmap_type` and `open_hashmap_type`. It defines
seven private components:
* `call_count` - the number of procedure calls on the map;
* `nbits` - the number of bits used to address the slots; 
* `num_entries` - the humber of entries in the map;
* `num_free` - the number of entries in the free list of removed 
  entries;
* `probe_count` - the number of map probes since the last resizing or
  initialization;
* `total_probes` - the number of probes of the map up to the last
  resizing or initialization; and
* `hasher` - a pointer to the hash function used by the map.
It also defines five non-overridable procedures:
* `calls` - returns the number of procedure calls on the map;
* `entries` - returns the number of entries in the map;
* `map_probes` - returns the number of map probes since
  initialization;
* `num_slots` - returns the number of slots in the map; and
* `slots_bits` - returns the number of bits used to address the slots;
and eleven deferred procedures:
* `get_other_data` - gets the other data associated with the key;
* `init` - initializes the hash map;
* `loading` - returns the ratio of the number of entries to the number
  of slots;
* `map_entry` - inserts a key and its other associated data into the
  map;
* `rehash` - rehashes the map with the provided hash function;
* `remove` - removes the entry associated wit the key;
* `set_other_data` - replaces the other data associated with the key;
* `total_depth` - returns the number of probes needed to address all
  the entries in the map;
* `valid_key` - returns a logical flag indicating whether the key is
  defined in the map.

The type's definition is below:

```fortran
    type, abstract :: hashmap_type
        private
        integer(int_calls) :: call_count = 0
        integer(int_calls) :: probe_count = 0
        integer(int_calls) :: total_probes = 0
        integer(int_index) :: num_entries = 0
        integer(int_index) :: num_free = 0
        integer(int_index) :: index_mask = 2_int_index**default_bits-1
        integer(int32)     :: nbits = default_bits
        procedure(hasher_fun), pointer, nopass :: hasher => fnv_1_hasher
    contains
        procedure, non_overridable, pass(map) :: calls
        procedure, non_overridable, pass(map) :: entries
        procedure, non_overridable, pass(map) :: map_probes
        procedure, non_overridable, pass(map) :: slots_bits
        procedure, non_overridable, pass(map) :: num_slots
        procedure(get_other), deferred, pass(map)    :: get_other_data
        procedure(init_map), deferred, pass(map)     :: init
        procedure(loading), deferred, pass(map)      :: loading
        procedure(map_entry), deferred, pass(map)    :: map_entry
        procedure(rehash_map), deferred, pass(map)   :: rehash
        procedure(remove_entry), deferred, pass(map) :: remove
        procedure(set_other), deferred, pass(map)    :: set_other_data
        procedure(total_depth), deferred, pass(map)  :: total_depth
        procedure(valid_index), deferred, pass(map)  :: valid_key
    end type hashmap_type
```


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

The `chaining_hashmap_type` derived type extends the `hashmap_type` to
implements a separate chaining hash map. In addition to the components
of the `hashmap_type` it provides the four components:
* `cache` - a pool of `chaining_map_entry_pool` objects used to reduce
allocation costs;
* `free_list` - a free list of map entries;
* `inverse` - an array of `chaining_map_entry_ptr` bucket lists
(inverses) storing entries at fixed locations once
entered; and
* `slots` - an array of bucket lists serving as the hash map.
It also implements all of the deferred procedures of the
`hashmap_type` and a finalizer for its maps. The type's definition is
as follows:

```fortran
    type, extends(hashmap_type) :: chaining_hashmap_type
        private
        type(chaining_map_entry_pool), pointer    :: cache => null() 
        type(chaining_map_entry_type), pointer    :: free_list => null() 
        type(chaining_map_entry_ptr), allocatable :: inverse(:) 
        type(chaining_map_entry_ptr), allocatable :: slots(:)
    contains
        procedure :: get_other_data => get_other_chaining_data
        procedure :: in_map => in_chain_map
        procedure :: init => init_chaining_map
        procedure :: loading => chaining_loading
        procedure :: map_entry => map_chain_entry
        procedure :: rehash => rehash_chaining_map
        procedure :: remove => remove_chaining_entry
        procedure :: set_other_data => set_other_chaining_data
        procedure :: total_depth => total_chaining_depth
        procedure :: unmap => unmap_chain
        procedure :: valid_index => valid_chaining_index
        final                    :: free_chaining_map
    end type chaining_hashmap_type
```

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

The `open_hashmap_type` derived type extends the `hashmap_type` to
implement an open addressing hash map. In addition to the components
of the `hashmap_type` it provides the four components:
* `cache` - a pool of `open_map_entry_pool` objects used to reduce
allocation costs;
* `free_list` - a free list of map entries;
* `index_mask` - an `and` mask used in linear addressing;
* `inverse` - an array of `open_map_entry_ptr` bucket lists
(inverses) storing entries at fixed locations once
entered; and
* `slots` - an array of bucket lists serving as the hash map.
It also implements all of the deferred procedures of the
`hashmap_type` and a finalizer for its maps. The type's definition is
as follows:

```fortran
    type, extends(hashmap_type) :: open_hashmap_type 
        private 
        integer(int_index) :: index_mask = 2_int_index**default_bits-1
        type(open_map_entry_pool), pointer    :: cache => null()
        integer(int_index), allocatable        :: slots(:)
        type(open_map_entry_ptr), allocatable  :: inverse(:)
        type(open_map_entry_list), pointer    :: free_list => null()
    contains
        procedure :: get_other_data => get_other_open_data
        procedure :: in_map => in_open_map
        procedure :: init => init_open_map
        procedure :: loading => open_loading
        procedure :: map_entry => map_open_entry
        procedure :: rehash => rehash_open_map
        procedure :: remove => remove_open_entry
        procedure :: set_other_data => set_other_open_data
        procedure :: total_depth => total_open_depth
        procedure :: unmap => unmap_open
        procedure :: valid_index => valid_open_index
        final     :: free_open_map
    end type open_hashmap_type
```

### Table of `stdlib_hashmap` procedures

The `stdlib_hashmap` module provides procedures in
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

* `map_entry( map, key, other, conflict )` - Inserts an entry into the
  hash map.

* `remove_entry(map, key, existed )` - Remove the entry, if any,
  associated with the `key`.

* `set_other_data( map, key, other, exists )` - Change the other data
  associated with the entry.

Procedures to report the content of a map:

* `get_other_data( map, key, other, exists )` - Returns the other data
  associated with the `key`;

* `valid_key(map, key)` - Returns a flag indicating whether the `key`
  is present in the map.

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


### Specifications of the `stdlib_hashmaps` procedures

#### `calls` - Returns the number of calls on a hash map

##### Status

Experimental

##### Description

Returns the number of procedure calls on a hash map.

##### Syntax

`value = [[stdlib_hashmaps:map % calls]]()`

##### Class

Pure function

##### Argument

`map` (pass) - shall be an expression of class `hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be an integer of kind `int_calls`.

##### Result value

The result will be the number of procedure calls on the hash map.

##### Example

```fortran
    program demo_calls
      use stdlib_hashmaps, only: chaining_hashmap_type, int_calls
      use stdlib_hashmap_wrappers, only: fnv_1_hasher
      implicit none
      type(chaining_hashmap_type) :: map
      type(int_calls) :: initial_calls
      call map % init(fnv_1_hasher )
      initial_calls = map % calls()
      print *, "INITIAL_CALLS =  ", initial_calls
    end program demo_calls
```


#### `entries` - Returns the number of entries in a hash map

##### Status

Experimental

##### Description

Returns the number of entries in a hash map.

##### Syntax

`value = [[stdlib_hashmaps:map%entries]]()`

##### Class

Pure function

##### Argument

`map` (pass)  - shall be an expression of class `hashmap_type`.
It is an `intent(in)` argument.

##### Result character

The result will be an integer of kind `int_index`.

##### Result value

The result will be the number of entries in the hash map.

##### Example

```fortran
    program demo_entries
      use stdlib_hashmaps, only: open_hashmap_type, int_index
      use stdlib_hashmap_wrappers, only: fnv_1_hasher
      implicit none
      type(open_hashmap_type) :: map
      type(int_index) :: initial_entries
      call map % init( fnv_1_hasher )
      initial_entries = map % entries ()
      print *, "INITIAL_ENTRIES =  ", initial_entries
    end program demo_entries
```


#### `get_other_data` - Returns other data associated with the `key`

##### Status

Experimental

##### Description

Returns the other data associated with the `key`,

##### Syntax

`value = [[stdlib_hashmaps:map%get_other_data)]]( key, other [, exists] )`

##### Class

Subroutine

##### Arguments

`map` (pass): shall be a scalar expression of class
  `chaining_hashmap_type` or `open_hashmap_type`. It is an
  `intent(in)` argument. It will be 
  the hash map used to store and access the other data.

`key`: shall be a scalar expression of type `key_type`. It
  is an `intent(in)` argument.

`other`: shall be a variable of type `other_data`.
  It is an `intent(out)` argument. It is the other data associated
  with the `key`.

`exists` (optional): shall be a variable of type logical. It is an
`intent(out)` argument.  If `true` an entry with the given `key`
exists in the map, if false `other` is undefined.

##### Example

 The following is an example of the retrieval of other data
  associated with a `key`:



```Fortran
    program demo_get_other_data
        use, intrinsic:: iso_fortran_env, only: &
            int8
        use stdlib_hashmaps, only: chaining_hashmap_type, int_index
        use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type
	    logical                     :: conflict, exists
        type(key_type)              :: key
        type(other_type)            :: other
        type(chaining_hashmap_type) :: map
        type dummy_type
            integer(int8) :: value(4)
        end type dummy_type
		type(dummy_type) :: dummy
        class(*), allocatable :: data
        dummy % value = [ 4_int8, 3_int8, 2_int8, 1_int8 ]
        allocate( data, source=dummy ) 
        call map % init( fnv_1_hasher )
        call set( key, [ 0_int8, 1_int8, 2_int8, 3_int8, 4_int8 ] )
        call set( other, data )
        call map % map_entry( key, other, conflict )
        if ( .not. conflict ) then
            call map % get_other_data( key, other )
        else
            stop 'Key is already present in the map.'
        end if
        call get( other, data )
        select type( data )
        type (dummy_type)
            print *, 'Other data % value = ', data % value
        type default
            print *, 'Invalid data type in other'
        end select
    end program demo_get_other_data
```


#### init - initializes a hash map

##### Status

Experimental

##### Description

Initializes a `chaining_hashmap_type` object.

##### Syntax

`call [[stdlib_hashmaps:map%init]]( hasher [, slots_bits, status ] ] )`

####@# Class

Subroutine

##### Arguments

`map` (pass): shall be a scalar variable of class
  `chaining_hashmap_type` or `open_hashmap_type`. It is an
  `intent(out)` argument. It will 
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
        use stdlib_hashmaps, only: chaining_map_type
        use stdlib_hashmap_wrappers, only: fnv_1_hasher
        type(fnv_1a_type)       :: fnv_1
        type(chaining_map_type) :: map
        call map % init( fnv_1a, slots_bits=10 )
    end program demo_init
```


#### `loading` - Returns the ratio of entries to slots

##### Status

Experimental

##### Description

Returns the ratio of the number of entries relative to the number of
slots in a hash map.

##### Syntax

`value = [[stdlib_hashmaps:map%loading]]( )`

##### Class

Pure function

##### Argument

`map` (pass) - shall be an expression of class `chaining_hashmap_type`
or `open_hashmap_type`. It is an `intent(in)` argument.

##### Result character

The result will be a default real.

##### Result value

The result will be the ratio of the number of entries relative to the
number of slots in the hash map.

##### Example

```fortran
    program demo_loading
      use stdlib_hashmaps, only: open_hashmap_type
      use stdlib_hashmap_wrappers, only:  fnv_1_hasher
      implicit none
      type(open_hashmap_type) :: map
      real :: ratio
      call map % init( fnv_1_hasher )
      ratio = map % loading ()
      print *, "Initial loading =  ", ratio
    end program demo_loading
```

#### `map_entry` - inserts an entry into the hash map

##### Status

Experimental

##### Description

Inserts an entry into the hash map if it is not already present.

##### Syntax

`call [[stdlib_hashmaps:map%map_entry]]( key[, other, conflict ] )`


##### Class

Subroutine

##### Arguments

`map` (pass): shall be a scalar variable of class
`chaining_hashmap_type` or `open_hashmap_type`. It
is an `intent(inout)` argument. It is the hash map to receive the
entry.

`key`: shall be a scalar expression of type `key_type`.
  It is an `intent(in)` argument. It is the key for the entry to be
  placed in the table.

`other` (optional): shall be a scalar expression of type `other_type`.
  It is an `intent(in)` argument. If present it is the other data to be
  associated with the `key`.

`conflict` (optional): shall be a scalar variable of type
`logical`. It is an `intent(in)` argument. If present, a `.true.`
value indicates that an entry with the value of `key` already exists
and the entry was not entered into the map, a `.false` value indicates
that `key` was not present in the map and the entry was added to the
table. 

* If `key` is already present in `map` then the presence of `other` 
is ignored.

##### Example

```fortran
    program demo_map_entry
        use, intrinsic:: iso_fortran_env, only: int8
        use stdlib_hashmaps, only: chaining_hashmap_type
        use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type, other_type
        type(chaining_hashmap_type) :: map
        type(key_type)      :: key
        logical             :: conflict
        type(other_type)    :: other
        class(*), allocatable :: dummy
        allocate( dummy, source=4 )
        call map % init( fnv_1_hasher, slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, dummy )
        call map % map_entry( key, other, conflict )
        print *, 'CONFLICT = ', conflict
    end program demo_map_entry
```

#### `map_probes` - returns the number of hash map probes

##### Status

Experimental

##### Description

Returns the total number of table probes on a hash map

##### Syntax

`Result = [[stdlib_hashmap:map%map_probes]]( )`

##### Class

Pure function

##### Argument

`map` (pass): shall be a scalar expression of class
`hashmap_type`. It is an `intent(in)`
argument. It is the hash map of interest.

##### Result character

The result is a scalar integer of kind `int_probes`.

##### Result value

The result is the number of probes of `map` since initialization or
rehashing. 

##### Example

```fortran
    program demo_probes
      use stdlib_hashmaps, only: chaining_hashmap_type, int_index
      use stdlib_hashmap_wrappers: fnv_1_hasher
      implicit none
      type(chaining_hashmap_type) :: map
      real :: nprobes
      call map % init( fnv_1_hasher )
      nprobes = map % probes()
      print *, "Initial probes =  ", nprobes
    end program demo_probes
```

#### `num_slots` - returns the number of hash map probes

##### Status

Experimental

##### Description

Returns the total number of slots on a hash map

##### Syntax

`Result = [[stdlib_hashmaps:map%num_slots]]( )`

##### Class

Pure function

##### Argument

`map`: shall be a scalar expression of class
`hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_index`.

##### Result value

The result is the number of slots in `map`.

##### Example

```fortran
    program demo_num_slots
      use stdlib_hashmaps, only: chaining_hashmap_type, int_index
      use stdlib_hashmap_wrappers, only: fnv_1_hasher
      implicit none
      type(chaining_hashmap_type) :: map
      integer(int_index) :: initial_slots
      call map % init( fnv_1_hasher )
      initial_slots = map % num_slots ()
      print *, "Initial slots =  ", initial_slots
    end program num_slots
```


#### rehash - changes the hashing function

##### Status

Experimental

##### Description

Changes the hashing function for the table entries to that of `hasher`.

##### Syntax

`call [[stdlib_hashmaps:map%rehash]]( hasher )`

##### Class

Subroutine

##### Arguments

`map` (pass): shall be a scalar variable of class
`chaining_hashmap_type` oe `open_hashmap_type`.
It is an `intent(inout)` argument. It is the hash map whose hashing 
method is to be changed.

`hasher`: shall be a function of interface `hasher_fun`.
It is the hash method to be used by `map`.

##### Example

```fortran
    program demo_rehash
        use stdlib_hashmaps, only: open_hashmap_type
        use stdlib_hasmap_wrappers, only: fnv_1_hasher, fnv_1a_hasher,&
            key_type, other_type
        type(openn_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        class(*), allocatable :: dummy
        allocate( dummy, source='a dummy value' )
        call map % init( fnv_1_hasher, slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, dummy )
        call map % map_entry( key, other )
        call map % rehash( fnv_1a_hasher )
    end program demo_rehash
```

#### `remove_entry` - removes an entry from the hash map

##### Status

Experimental

##### Description

Removes an entry from a hash map, `map`.

##### Syntax

`call [[stdlib_hashmaps:map%remove_entry]]( key[, existed ])`

##### Class

Subroutine

##### Arguments

`map` (pass): shall be a scalar variable of class
`chaining_hashmap_type` or `open_hashmap_type`. 
It is an `intent(inout)` argument. It is the hash map with the element 
to be removed.

`key`: shall be a scalar expression of type `key_type`. It
is an `intent(in)` argument. It is the `key` identifying the entry
to be removed.

`existed` (optional): shall be a scalar variable of type default
logical. It is an `intent(out)` argument. If present with the value
`true` the entry existed 
in the map before removal, if false the entry was not present to be
removed. 

##### Example

```fortran
    program demo_remove_entry
        use stdlib_hashmaps, only: open_hashmap_type, int_index
        use stdlib_hashmap_wrappers, only: fnv_1_hasher, &
            fnv_1a_hasher, key_type, other_type
        type(open_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        logical             :: existed
        class(*), allocatable :: dummy
        allocate( dummy, source=4.0 )
        call map % init( fnv_1_hasher, slots_bits=10 )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        call set( other, dummy )
        call map % map_entry( key, other )
        call map % remove_entry( key, existed )
        print *, "Removed key existed = ", existed
    end program demo_remove_entry
```

#### `set_other_data` - replaces the other dataa for an entry

##### Status

Experimental

##### Description

Replaces the other data for the entry with the key value, `key`.

##### Syntax

`call [[stdlib_hashmaps:map%set_other_data]]( key, other[, exists] )`

##### Class

Subroutine

##### Arguments

`map` (pass): shall be a scalar variable of class
`chaining_hashmap_type` or `open_hashmap_type`. It
is an `intent(inout)` argument. It will be a hash map used to store
and access the entry's data.

`key`: shall be a scalar expression of  type `key_type`. It
is an `intent(in)` argument. It is the `key` to the entry whose
`other` data is to be replaced.

`other`: shall be a scalar expression of type `other_type`.
It is an `intent(in)` argument. It is the data to be stored as
the other data for the entry with the key value, `key`.

`exists` (optional): shall be a scalar variable of type default
logical. It is an `intent(out)` argument. If present with the value
`.true.` an entry with that key existed in the map and its `other`
data was replaced, otherwise if `exists` is `.false.` the entry didnot
exisst and nothing was done.


##### Example

```fortran
    program demo_set_other_data
        use stdlib_hashmaps, only: open_hashmap_type
        use stdlib_hashmap_wrappers, only: fnv_1_hasher, &
            fnv_1a_hasher, key_type, other_type, set
        type(open_hashmap_type) :: map
        type(key_type)      :: key
        type(other_type)    :: other
        class(*), allocatable :: dummy
        call map % init( fnv_1_hasher, slots_bits=10 )
        allocate( dummy, source='A value` )
        call set( key, [ 5_int8, 7_int8, 4_int8, 13_int8 ] )
        Call set( other, dummy )
        call map % map_entry( key, other )
        deallocate( dummy )
        allocate( dummy, source='Another value` )
        call set( other, dummy )
        call map % set_other_data( key, other, exists )
        print *, 'The entry to have its other data replaced exists = ', exists
    end program demo_set_other_data
```

#### `slots_bits` - returns the number of bits used to address the hash map slots 

##### Status

Experimental

##### Description

Returns the total number of slots on a hash map

##### Syntax

`Result = [[stdlib_hashmaps:map%slots_bits]]( )`

##### Class

Pure function

##### Argument

`map` (pass): shall be a scalar expression of class
`hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_index`.

##### Result value

The result is the number of bits used in addressing the slots in `map`.

##### Example

```fortran
    program demo_slots_bits
      use stdlib_hashmaps, only: chaining_hashmap_type
      use stdlib_hashmap_wrappers, only: fnv_1_hasher
      implicit none
      type(chaining_hashmap_type) :: map
      integer :: bits
      call map % init( fnv_1_hasher )
      bits = map % slots_bits ()
      print *, "Initial slot bits =  ", bits
    end program demo_slots_bits
```


#### `total_depth` - returns the total depth of the hash map entries

##### Status

Experimental

##### Description

Returns the total number of one's based offsets of slot entries from
their slot index for a hash map

##### Syntax

`Result = [[stdlib_hashmaps:map%total_depth]]( )`

##### Class

Pure function

##### Argument

`map` (pass): shall be a scalar expression of class
`chaining_hashmap_type`. It is an `intent(in)` argument. It is the
hash map of interest.

##### Result character

The result is a scalar integer of kind `int_depth`.

##### Result value

The result is the total number of one's based offsets of slot entries
from their slot index the map.

##### Example

```fortran
    program demo_total_depth
      use stdlib_hashmaps, only: chaining_hashmap_type, int_depth
      use stdlib_hashmap_wrappers, only: fnv_1_hasher
      implicit none
      type(chaining_hashmap_type) :: map
      integer(int_depth) :: initial_depth
      call map % init( fnv_1_hasher )
      initial_depth = map % total_depth ()
      print *, "Initial total depth =  ", initial_depth
    end program demo_total_depth
```


#### `valid_key` - indicates whether `key` is present

##### Status

Experimental

##### Description

Returns a logical flag indicating whether `key` exists for an entry in
the map.

##### Syntax

`result = [[stdlib_hashmaps:map % valid_index]]( key )`

##### Class

Pure function.

##### Arguments

`map` (pass): shall be a scalar expression of class
`chaining_hashmap_type` or `open_hashmap_type`. 
It is an `intent(in)` argument. It is the hash map whose entries are
examined.

`key`: shall be a scalar expression a of type `key_type`. It
is an `intent(in)` argument. It is a `key` whose presence in the `map`
is being examined. 

##### Result character

The result is a default logical scalar.

##### Result value

The result is `.true.` if `key` is present in `map` and `.false.`
otherwise.

##### Example

```fortran
    program demo_valid_key
      use stdlib_kinds, only: int8
      use stdlib_hashmaps, only: chaining_hashmap_type
      use stdlib_hashmap_wrappers, only: fnv_1_hasher, key_type
      implicit none
      type(chaining_hashmap_type) :: map
      type(key_type) :: key
      logocal :: valid
      call map % init( fnv_1_hasher )
      call set_key(key, [0_int8, 1_int8] )
      valid = map % valid_key ( key )
      print *, "Initial key of 10 valid for empty map =  ", valid
    end program demo_valid_index
```
