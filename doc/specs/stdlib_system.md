---
title: system
---

# System and sub-processing module

The `stdlib_system` module provides interface for interacting with external processes, enabling the execution 
and monitoring of system commands or applications directly from Fortran. 

[TOC]

## `run` - Execute an external process synchronously

### Status

Experimental

### Description

The `run` interface allows execution of external processes using a single command string or a list of arguments.  
Processes run synchronously, meaning execution is blocked until the process finishes.  
Optional arguments enable the collection of standard output and error streams, as well as sending input via standard input.

### Syntax

`process = ` [[stdlib_subprocess(module):run(interface)]] `(args [, stdin] [, want_stdout] [, want_stderr])`

### Arguments

`args`: Shall be a `character(*)` string (for command-line execution) or a `character(*), dimension(:)` array (for argument-based execution). It specifies the command and arguments to execute. This is an `intent(in)` argument.

`stdin` (optional): Shall be a `character(*)` value containing input to send to the process via standard input (pipe). This is an `intent(in)` argument.

`want_stdout` (optional): Shall be a `logical` flag. If `.true.`, the standard output of the process will be captured; if `.false.` (default), it will be lost. This is an `intent(in)` argument.

`want_stderr` (optional): Shall be a logical flag. If `.true.`, the standard error output of the process will be captured. Default: `.false.`. This is an `intent(in)` argument.

### Return Value

Returns an object of type `process_type` that contains information about the state of the created process.

### Example

```fortran
! Example usage with command line or list of arguments
type(process_type) :: p

! Run a simple command line synchronously
p = run("echo 'Hello, world!'", want_stdout=.true.)
```


## `runasync` - Execute an external process asynchronously

### Status

Experimental

### Description

The `runasync` interface allows execution of external processes using a single command string or a list of arguments.  
Processes are run asynchronously (non-blocking), meaning execution does not wait for the process to finish.  
Optional arguments enable the collection of standard output and error streams, as well as sending input via standard input.

### Syntax

`process = ` [[stdlib_subprocess(module):runasync(interface)]] `(args [, stdin] [, want_stdout] [, want_stderr])`

### Arguments

`args`: Shall be a `character(*)` string (for command-line execution) or a `character(*), dimension(:)` array (for argument-based execution). It specifies the command and arguments to execute. This is an `intent(in)` argument.

`stdin` (optional): Shall be a `character(*)` value containing input to send to the process via standard input (pipe). This is an `intent(in)` argument.

`want_stdout` (optional): Shall be a `logical` flag. If `.true.`, the standard output of the process will be captured; if `.false.` (default), it will be lost. This is an `intent(in)` argument.

`want_stderr` (optional): Shall be a logical flag. If `.true.`, the standard error output of the process will be captured. Default: `.false.`. This is an `intent(in)` argument.

### Return Value

Returns an object of type `process_type` that contains information about the state of the created process.

### Example

```fortran
! Example usage with command line or list of arguments
type(process_type) :: p(2)

! Run a simple command line asynchronously
p(1) = runasync("echo 'Hello, world!'", want_stdout=.true.)

! Run a command using an argument list asynchronously
p(2) = runasync(["/usr/bin/ls", "-l"], want_stdout=.true.)
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

`delta_t = ` [[stdlib_subprocess(module):elapsed(subroutine)]] `(process)`

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

## `wait` - Wait until a running process is completed

### Status

Experimental

### Description

The `wait` interface provides a method to block the calling program until the specified process completes.  
If the process is running asynchronously, this subroutine will pause the workflow until the given process finishes.  
Additionally, an optional maximum wait time can be provided. If the process does not finish within the specified time, 
the subroutine will return without waiting further.

On return from this routine, the process state is accordingly updated.
This is useful when you want to wait for a background task to complete, but want to avoid indefinite blocking 
in case of process hang or delay.


### Syntax

`call ` [[stdlib_subprocess(module):wait(subroutine)]] `(process [, max_wait_time])`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process to monitor.  
This is an `intent(inout)` argument, and its state is updated upon completion.

`max_wait_time` (optional): Shall be a `real` value specifying the maximum wait time in seconds.  
If not provided, the subroutine will wait indefinitely until the process completes.

### Example

```fortran
! Example usage of wait
type(process_type) :: p

! Start an asynchronous process
p = run("sleep 5", wait=.false.)

! Wait for process to complete with a 10-second timeout
call wait(p, max_wait_time=10.0)
print *, "Process completed or timed out."
```

## `update` - Update the internal state of a process

### Status

Experimental

### Description

The `update` interface allows the internal state of a process object to be updated by querying the system.  
After the process completes, the standard output and standard error are retrieved, if they were requested, and loaded into the `process%stdout` and `process%stderr` string variables, respectively.

This is especially useful for monitoring asynchronous processes and retrieving their output after they have finished.

### Syntax

`call ` [[stdlib_subprocess(module):update(subroutine)]] `(process)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process whose state needs to be updated.  
This is an `intent(inout)` argument, and its internal state is updated on completion.

### Example

```fortran
! Example usage of update
type(process_type) :: p

! Start an asynchronous process
p = run("sleep 5", wait=.false., want_stdout=.true., want_stderr=.true.)

! Periodically update the process state
call update(p)

! After completion, print the captured stdout and stderr
if (p%completed) then 
    print *, "Standard Output: ", p%stdout
    print *, "Standard Error: ", p%stderr
endif
```

## `kill` - Terminate a running process

### Status

Experimental

### Description

The `kill` interface is used to terminate a running external process. It attempts to stop the process and returns a boolean flag indicating whether the operation was successful.
This interface is useful when a process needs to be forcefully stopped, for example, if it becomes unresponsive or if its execution is no longer required.

### Syntax

`call ` [[stdlib_subprocess(module):kill(subroutine)]] `(process, success)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process to be terminated.  
This is an `intent(inout)` argument, and on return is updated with the terminated process state.

`success`: Shall be a `logical` variable. It is set to `.true.` if the process was successfully killed, or `.false.` otherwise.

### Example

```fortran
! Example usage of kill
type(process_type) :: p
logical :: success

! Start a process asynchronously
p = run("sleep 10", wait=.false.)

! Attempt to kill the process
call kill(p, success)

if (success) then
    print *, "Process successfully killed."
else
    print *, "Failed to kill the process."
end if
```

## `sleep` - Pause execution for a specified time in milliseconds

### Status

Experimental

### Description

The `sleep` interface pauses the execution of a program for a specified duration, given in milliseconds. 
This routine acts as a cross-platform wrapper, abstracting the underlying platform-specific sleep implementations. 
It ensures that the requested sleep duration is honored on both Windows and Unix-like systems.

### Syntax

`call ` [[stdlib_system(module):sleep(subroutine)]] `(millisec)`

### Arguments

`millisec`: Shall be an `integer` representing the number of milliseconds to sleep. This is an `intent(in)` argument.

### Example

```fortran
! Example usage of sleep
print *, "Starting sleep..."

! Sleep for 500 milliseconds
call sleep(500)

print *, "Finished sleeping!"
```

## `is_windows` - Check if the system is running on Windows

### Status

Experimental

### Description

The `is_windows` interface provides a quick, compile-time check to determine if the current system is Windows. 
It leverages a C function that checks for the presence of the `_WIN32` macro, which is defined in C compilers when targeting Windows. 
This function is highly efficient and works during the compilation phase, avoiding the need for runtime checks.

### Syntax

`result = ` [[stdlib_system(module):is_windows(function)]] `()`

### Return Value

Returns a `logical` flag: `.true.` if the system is Windows, or `.false.` otherwise.

### Example

```fortran
if (is_windows()) then
    print *, "Running on Windows!"
else
    print *, "Not running on Windows."
end if
```
