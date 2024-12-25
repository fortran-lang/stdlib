---
title: system
---

# System and sub-processing module

The `stdlib_system` module provides interface for interacting with external processes, enabling the execution 
and monitoring of system commands or applications directly from Fortran. 

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
```

## `is_running` - Check if a process is still running

### Status

Experimental

### Description

The `is_running` interface provides a method to check if an external process is still running.  
This is useful for monitoring the status of asynchronous processes created with the `run` interface.

### Syntax

`status = ` [[stdlib_subprocess(module):is_running(interface)]] `(process)`

### Arguments

`process`:  Shall be a `type(process_type)` object representing the external process to check. This is an `intent(inout)` argument.


### Return Value

Returns a `logical` value: `.true.` if the process is still running, or `.false.` if the process has terminated.
After a call to `is_running`, the `type(process_type)` structure is also updated to the latest process state.

### Example

```fortran
! Example usage of is_running
type(process_type) :: proc
logical :: status

! Start an asynchronous process
proc = run("sleep 10", wait=.false.)

! Check if the process is running
status = is_running(proc)

if (status) then
    print *, "Process is still running."
else
    print *, "Process has terminated."
end if
```

## `is_completed` - Check if a process has completed execution

### Status

Experimental

### Description

The `is_completed` interface provides a method to check if an external process has finished execution.  
This is useful for determining whether asynchronous processes created with the `run` interface have terminated.

### Syntax

`status = ` [[stdlib_subprocess(module):is_completed(interface)]] `(process)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process to check. This is an `intent(inout)` argument.

### Return Value

Returns a `logical` value:  
- `.true.` if the process has completed.  
- `.false.` if the process is still running.  

After a call to `is_completed`, the `type(process_type)` structure is updated to reflect the latest process state.

### Example

```fortran
! Example usage of is_completed
type(process_type) :: proc
logical :: status

! Start an asynchronous process
proc = run("sleep 5", wait=.false.)

! Check if the process has completed
status = is_completed(proc)

if (status) then
    print *, "Process has completed."
else
    print *, "Process is still running."
end if
```

## `elapsed` - Return process lifetime in seconds

### Status

Experimental

### Description

The `elapsed` interface provides a method to calculate the total time that has elapsed since a process was started.  
This is useful for tracking the duration of an external process or for performance monitoring purposes.  

The result is a real value representing the elapsed time in seconds, measured from the time the process was created.

### Syntax

`delta_t = ` [[stdlib_subprocess(module):elapsed(interface)]] `(process)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process. It is an `intent(in)` argument.

### Return Value

Returns a `real(real64)` value that represents the elapsed time (in seconds) since the process was started.  
If the process is still running, the value returned is the time elapsed until the call to this function. 
Otherwise, the total process duration from creation until completion is returned.

### Example

```fortran
! Example usage of elapsed
type(process_type) :: p
real(RTICKS) :: delta_t

! Create a process
p = run("sleep 5", wait=.false.)

! Check elapsed time after 2 seconds
call sleep(2)
delta_t = elapsed(p)
print *, "Elapsed time (s): ", delta_t
```


