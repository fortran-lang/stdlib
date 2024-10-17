---
title: filesystem
---

# The `stdlib_filesystem` module

[TOC]

## Introduction

Module for filesystem interactions.

## Constants

### `is_windows``

Boolean constant indicating whether the current platform is Windows.

### `path_separator``

Character constant representing the path separator for the current platform. On Windows, it is `\`. On other platforms, it is `/`.

## Procedures

### `exists`

#### Status

Experimental

#### Description

Determines if a file or directory exists at the given path by returning a logical value.

#### Syntax

`exists = ` [[stdlib_filesystem(module):exists(function)]] `(path)`

#### Arguments

`path`: Shall be a character expression containing the path to a file or directory to check for existence.

#### Return value

A logical value indicating whether a file or directory exists at the given path.

### `list_dir`

#### Status

Experimental

#### Description

Lists the contents of a directory.

#### Syntax

`call ` [[stdlib_filesystem(module):list_dir(subroutine)]] `(dir, files[, iostat][, iomsg])`

#### Arguments

`dir`: Shall be a character expression containing the path to the directory to list.

`files`: Shall be an allocatable rank-1 array of type `string_type` that will contain the names of the files and directories in the directory.

`iostat`: Shall be a scalar of type `integer` that receives the error status of `list_dir`. Optional argument.

`iomsg`: Shall be a deferred length character variable that receives the error message of `list_dir`. Optional argument.

### `mkdir`

#### Status

Experimental

#### Description

Creates a new directory.

#### Syntax

`call ` [[stdlib_filesystem(module):mkdir(subroutine)]] `(dir[, iostat][, iomsg])`

#### Arguments

`dir`: Shall be a character expression containing the path to the directory to create.

`iostat`: Shall be a scalar of type `integer` that receives the error status of `mkdir`. Optional argument.

`iomsg`: Shall be a deferred length character variable that receives the error message of `mkdir`. Optional argument.

### `rmdir`

#### Status

Experimental

#### Description

Removes a directory.

#### Syntax

`call ` [[stdlib_filesystem(module):rmdir(subroutine)]] `(dir)`

#### Arguments

`dir`: Shall be a character expression containing the path to the directory to remove.

### `run`

#### Status

Experimental

#### Description

Runs a command in the shell.

#### Syntax

`call ` [[stdlib_filesystem(module):run(subroutine)]] `(command[, iostat][, iomsg])`

#### Arguments

`command`: Shall be a character expression containing the command to run in the shell.

`iostat`: Shall be a scalar of type `integer` that receives the error status of `run`. Optional argument.

`iomsg`: Shall be a deferred length character variable that receives the error message of `run`. Optional argument.
