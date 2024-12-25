---
title: system
---

# System

[TOC]

## `run` - Execute an external process

### Status

Experimental

### Description

The `run` interface allows execution of external processes using a single command string or a list of arguments.  
Processes can be run either synchronously (blocking execution until the process finishes) or asynchronously (non-blocking execution).  
Optional arguments enable the collection of standard output and error streams, as well as sending input via standard input.

### Syntax

`process = ` [[stdlib_subprocess(module):run(interface)]] `(args [, wait] [, stdin] [, want_stdout] [, want_stderr])`

### Arguments

`args`: Shall be a `character(*)` string (for command-line execution) or a `character(*), dimension(:)` array (for argument-based execution). It specifies the command and arguments to execute. This is an `intent(in)` argument.

`wait` (optional): Shall be a `logical` flag. If `.true.` (default), the process will execute synchronously (blocking). If `.false.`, the process will execute asynchronously (non-blocking). This is an `intent(in)` argument.

`stdin` (optional): Shall be a `character(*)` value containing input to send to the process via standard input (pipe). This is an `intent(in)` argument.

`want_stdout` (optional): Shall be a `logical` flag. If `.true.`, the standard output of the process will be captured; if `.false.` (default), it will be lost. This is an `intent(in)` argument.

`want_stderr` (optional): Shall be a logical flag. If `.true.`, the standard error output of the process will be captured. Default: `.false.`. This is an `intent(in)` argument.

### Return Value

Returns an object of type `process_type` that contains information about the state of the created process.

### Example

```fortran
! Example usage with command line or list of arguments
type(process_type) :: p(2)

! Run a simple command line synchronously
p(1) = run("echo 'Hello, world!'", wait=.true., want_stdout=.true.)

! Run a command using an argument list asynchronously
p(2) = run(["/usr/bin/ls", "-l"], wait=.false.)

