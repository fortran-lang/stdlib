---
title: Map abstract base class
...

# The `stdlib_map_class` module

[TOC]


## Introduction


## `map_class` abstract base class for maps

The `map_class` is defined as abstract base class for general map types.

### Constructor

Implementation of the `map_class` can provide a constructor by overloading the type name
and by providing a subroutine with the name of the type name prefixed with `new_`.
A general constructor is provided in the proxy module [[stdlib_map]], where implementations
can be registered.

@note: The constructor shall not be type bound.


### Finalizer

Implementations of `map_class` own the data they are storing are responsible for
freeing any allocated memory to avoid memory leaks


### `len` and `get_length` functions

#### Status

experimental

#### Description

Retrieve the number of elements stored inside the map. The interface `len` provides a
more convenient way to access the number of stored elements but is functional identical
to the type bound procedure `get_length`.

#### Syntax

`res = self % [[map_class(type):get_length(bound)]] ()`

`res = [[stdlib_map_class(module):len(interface)]] (self)`

#### Class

Pure function.

#### Argument

- `self`: shall be a scalar expression of class `map_class`. It is an `intent(in)` argument.

#### Result value

The return value is of integer value


### `insert`

#### Status

experimental

#### Description

Insert a new key in the map, creates a reference to the inserted data type.

#### Syntax

`call self % [[map_class(type):insert(bound)]] (key, ptr)`

#### Class

Recursive subroutine.

#### Argument

- `self`: shall be a scalar expression of class `map_class`. It is an `intent(inout)` argument.
- `key`: shall be a scalar default character value. It is an `intent(in)` argument.
- `ptr`: shall be a pointer value of `container_type`. It is an `intent(out)` argument.
  Will be associated to the newly created value.


### `get`

#### Status

experimental

#### Description

Retrieve a reference to the value associated with the provided key inside the map.

#### Syntax

`call self % [[map_class(type):get(bound)]] (key, ptr)`

#### Class

Recursive subroutine.

#### Argument

- `self`: shall be a scalar expression of class `map_class`. It is an `intent(inout)` argument.
- `key`: shall be a scalar default character value. It is an `intent(in)` argument.
- `ptr`: shall be a pointer value of `container_type`. It is an `intent(out)` argument.
  Will be unassociated if no value for `key` exists.


### `drop`

#### Status

experimental

#### Description

Drop value at the specified key from the map. No action is performed if value does not exist.

#### Syntax

`call self % [[map_class(type):drop(bound)]] (key)`

#### Class

Subroutine.

#### Argument

- `self`: shall be a scalar expression of class `map_class`. It is an `intent(inout)` argument.
- `key`: shall be a scalar default character value. It is an `intent(in)` argument.


### `clear`

#### Status

experimental

#### Description

Drop all values from the map.

#### Syntax

`call self % [[map_class(type):clear(bound)]] ()`

#### Class

Subroutine.

#### Argument

- `self`: shall be a scalar expression of class `map_class`. It is an `intent(inout)` argument.
