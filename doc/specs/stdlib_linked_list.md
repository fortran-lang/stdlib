---
title: linked lists
---

# The `stdlib_linked_list` module

[TOC]

## Introduction

The `stdlib_linked_list` module defines a class and its interface to handle linked lists that
store any type of data. The list may contain data of the same type or of various types.


## Types

### `type(linked_list_type)`

Linked lists are variables of the type `linked_list_type`. The type provides all the methods
required for storing and retrieving data.


## Procedures and methods provided


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `size` - Return the number of data items in the list

#### Description

Return the number of data items in the list.

#### Syntax

`number = [[stdlib_linked_list(module):list%size]] ()`

#### Status

Experimental

#### Class

Pure function.

#### Argument

None

#### Result value

The result is an integer scalar, equal to the number of items currently contained in the list.

#### Example

```fortran
{!example/linked_list/example_size.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `clear` - Remove all items from the list

#### Description

Remove all items from the list

#### Syntax

`call [[stdlib_linked_list(module):list%clear]]`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

None


#### Example

```fortran
{!example/linked_list/example_clear.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `get` - Get the data item at a given position (node) in the list

#### Description

Get the data item at a given position (node) in the list

#### Syntax

`item = [[stdlib_linked_list(module):list%get(interface)]] (node_index)`

#### Status

Experimental

#### Class

Function.

#### Argument

- `node_index`: Shall be a scalar integer equal to the position in the list for the new item.
  This argument is `intent(in)`.

#### Result value

The data item (of type `class(*)`) that is stored at the given position.

Notes:

- If the index is 0 or less, the first item in the list is returned.
- If the index is larger than the number of items, the last item in the list is returned.

#### Example

```fortran
{!example/linked_list/example_get.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `insert` - Insert a new item at a given position (node) in the list

#### Description

Insert a new item at a given position (node) in the list

#### Syntax

`call [[stdlib_linked_list(module):list%insert(interface)]] (item, node_index)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `item`: Data item to be stored (any type).
  This argument is `intent(in)`.
- `node_index`: Shall be an integer scalar equal to the position in the list for the new item.
  This argument is `intent(in)`.

#### Result value

The list is extended with the new data item at the given position.

Notes:

- If the index is 0 or less, the item is stored at the first position.
- If the index is larger than the number of items, it will be appended to the end of the list.

#### Example

```fortran
{!example/linked_list/example_insert.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `replace` - Replace an existing data by a new item at a given position (node) in the list

#### Description

Replace an existing data by a new item at a given position (node) in the list

#### Syntax

`call [[stdlib_linked_list(module):list%insert(interface)]] (new_item, node_index)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `new_item`: The new data item to be stored (any type).
  This argument is `intent(in)`.
- `node_index`: Shall be an integer scalar equal to the position in the list for the item to be replaced.
  This argument is `intent(in)`.

#### Result value

The new data item is stored and the existing one removed.

Notes:

- If the index is 0 or less, or it is larger than the number of items, nothing is done.

#### Example

```fortran
{!example/linked_list/example_replace.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `remove` - Remove an items at a given position (node) in the list

#### Description

Remove an items at a given position (node) in the list

#### Syntax

`call [[stdlib_linked_list(module):list%remove(interface)]] (node_index)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `node_index`: Shall be an integer scalar equal to the position in the list for the item to be removed.
  This argument is `intent(in)`.

#### Result value

The indicated item has been removed from the list.

Notes:

- If the index is 0 or less or the index is larger than the number of items, nothing is done.

#### Example

```fortran
{!example/linked_list/example_remove.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `push` - Append a new item to the end of the list

#### Description

Append a new item to the end of the list

#### Syntax

`call [[stdlib_linked_list(module):list%push(interface)]] (item)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `item`: Data item to be stored (any type).
  This argument is `intent(in)`.

#### Result value

The list is extended with the new data item at the tail.

#### Example

```fortran
{!example/linked_list/example_push.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `pop` - Remove the last item in the list

#### Description

Remove the last item in the list

#### Syntax

`call [[stdlib_linked_list(module):list%pop(interface)]]`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

None

#### Result value

The list item in the list is removed.

#### Example

```fortran
{!example/linked_list/example_pop.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `reverse` - Reconstruct the list in reverse order

#### Description

Reconstruct the list in reverse order

#### Syntax

`call [[stdlib_linked_list(module):list%reverse(interface)]]`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

None

#### Result value

The list now contains the items in reverse order.

#### Example

```fortran
{!example/linked_list/example_reverse.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `concat` - Concatenate a list to another list

#### Description

Concatenate a list to another list

#### Syntax

`call [[stdlib_linked_list(module):list%concat(interface)]] (list_to_concat)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `list_to_concat`: list whose data items are to be appended to the given `linked_list_type` derived type.
  this argument is `intent(in)`.

#### Result value

The given list is extended with the data items in the second list. The second list remains intact.

#### Example

```fortran
{!example/linked_list/example_concat.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `absorb` - Absorb a list into another list

#### Description

Absorb a list into another list

#### Syntax

`call [[stdlib_linked_list(module):list%absorb(interface)]] (list_to_absorb)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `list_to_absorb`: list whose data items will be appended to the given `linked_list_type` derived type.
  this argument is `intent(inout)`.

#### Result value

The given list is extended with the data items in the second list. The second list is emptied.

#### Example

```fortran
{!example/linked_list/example_absorb.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `slice` - Return a sublist of a list

#### Description

Return a sublist of a list

#### Syntax

`sublist = [[stdlib_linked_list(module):list%slice(interface)]] (start, end)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `start`: Shall be an integer scalar equal to the first item to store in the sublist.
  this argument is `intent(in)`.
- `end`: Shall be an integer scalar equal to the last item to store in the sublist.
  this argument is `intent(in)`.

#### Result value

Sublist consisting of the indicated data items. Note that the items themselves are copied from the original
list, so that the two lists are independent.

#### Example

```fortran
{!example/linked_list/example_slice.f90!}
```


<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `splice` - Remove a sublist from a list, based on a start and end index.

#### Description

Remove a sublist from a list, based on a start and end index.

#### Syntax

`call [[stdlib_linked_list(module):list%splice(interface)]] (start, end)`

#### Status

Experimental

#### Class

Subroutine.

#### Argument

- `start`: Shall be an integer scalar equal to the first item to be removed from the list.
  this argument is `intent(in)`.
- `end`: Shall be an integer scalar equal to the last item to be removed from the list.
  this argument is `intent(in)`.

#### Result value

The data items in the given range are removed from the list.

#### Example

```fortran
{!example/linked_list/example_splice.f90!}
```
