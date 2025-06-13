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
Additionally, a callback function can be specified to execute upon process completion, optionally receiving a user-defined payload.

### Syntax

`process = ` [[stdlib_system(module):run(interface)]] `(args [, stdin] [, want_stdout] [, want_stderr] [, callback] [, payload])`

### Arguments

`args`: Shall be a `character(*)` string (for command-line execution) or a `character(*), dimension(:)` array (for argument-based execution). It specifies the command and arguments to execute. This is an `intent(in)` argument.

`stdin` (optional): Shall be a `character(*)` value containing input to send to the process via standard input (pipe). This is an `intent(in)` argument.

`want_stdout` (optional): Shall be a `logical` flag. If `.true.`, the standard output of the process will be captured; if `.false.` (default), it will be lost. This is an `intent(in)` argument.

`want_stderr` (optional): Shall be a `logical` flag. If `.true.`, the standard error output of the process will be captured. If `.false.` (default), it will be lost. This is an `intent(in)` argument.

`callback` (optional): Shall be a procedure conforming to the `process_callback` interface. If present, this function will be called upon process completion with the process ID, exit state, and optionally collected standard input, output, and error streams. This is an `intent(in)` argument.

`payload` (optional): Shall be a generic (`class(*)`) scalar that will be passed to the callback function upon process completion. It allows users to associate custom data with the process execution. This is an `intent(inout), target` argument.

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
Additionally, a callback function can be specified to execute upon process completion, optionally receiving a user-defined payload.

### Syntax

`process = ` [[stdlib_system(module):runasync(interface)]] `(args [, stdin] [, want_stdout] [, want_stderr] [, callback] [, payload])`

### Arguments

`args`: Shall be a `character(*)` string (for command-line execution) or a `character(*), dimension(:)` array (for argument-based execution). It specifies the command and arguments to execute. This is an `intent(in)` argument.

`stdin` (optional): Shall be a `character(*)` value containing input to send to the process via standard input (pipe). This is an `intent(in)` argument.

`want_stdout` (optional): Shall be a `logical` flag. If `.true.`, the standard output of the process will be captured; if `.false.` (default), it will be lost. This is an `intent(in)` argument.

`want_stderr` (optional): Shall be a `logical` flag. If `.true.`, the standard error output of the process will be captured. Default: `.false.`. This is an `intent(in)` argument.

`callback` (optional): Shall be a procedure conforming to the `process_callback` interface. If present, this function will be called upon process completion with the process ID, exit state, and optionally collected standard input, output, and error streams. This is an `intent(in)` argument.

`payload` (optional): Shall be a generic (`class(*)`) scalar that will be passed to the callback function upon process completion. It allows users to associate custom data with the process execution. This is an `intent(inout), target` argument.

### Return Value

Returns an object of type `process_type` that contains information about the state of the created process.

### Example

```fortran
{!example/system/example_process_1.f90!}
```

## `is_running` - Check if a process is still running

### Status

Experimental

### Description

The `is_running` interface provides a method to check if an external process is still running.  
This is useful for monitoring the status of asynchronous processes created with the `run` interface.

### Syntax

`status = ` [[stdlib_system(module):is_running(interface)]] `(process)`

### Arguments

`process`:  Shall be a `type(process_type)` object representing the external process to check. This is an `intent(inout)` argument.


### Return Value

Returns a `logical` value: `.true.` if the process is still running, or `.false.` if the process has terminated.
After a call to `is_running`, the `type(process_type)` structure is also updated to the latest process state.

### Example

```fortran
{!example/system/example_process_2.f90!}
```

## `is_completed` - Check if a process has completed execution

### Status

Experimental

### Description

The `is_completed` interface provides a method to check if an external process has finished execution.  
This is useful for determining whether asynchronous processes created with the `run` interface have terminated.

### Syntax

`status = ` [[stdlib_system(module):is_completed(interface)]] `(process)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process to check. This is an `intent(inout)` argument.

### Return Value

Returns a `logical` value:  
- `.true.` if the process has completed.  
- `.false.` if the process is still running.  

After a call to `is_completed`, the `type(process_type)` structure is updated to reflect the latest process state.

### Example

```fortran
{!example/system/example_process_1.f90!}
```

## `elapsed` - Return process lifetime in seconds

### Status

Experimental

### Description

The `elapsed` interface provides a method to calculate the total time that has elapsed since a process was started.  
This is useful for tracking the duration of an external process or for performance monitoring purposes.  

The result is a real value representing the elapsed time in seconds, measured from the time the process was created.

### Syntax

`delta_t = ` [[stdlib_system(module):elapsed(subroutine)]] `(process)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process. It is an `intent(in)` argument.

### Return Value

Returns a `real(real64)` value that represents the elapsed time (in seconds) since the process was started.  
If the process is still running, the value returned is the time elapsed until the call to this function. 
Otherwise, the total process duration from creation until completion is returned.

### Example

```fortran
{!example/system/example_process_3.f90!}
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

`call ` [[stdlib_system(module):wait(subroutine)]] `(process [, max_wait_time])`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process to monitor.  
This is an `intent(inout)` argument, and its state is updated upon completion.

`max_wait_time` (optional): Shall be a `real` value specifying the maximum wait time in seconds.  
If not provided, the subroutine will wait indefinitely until the process completes.

### Example

```fortran
{!example/system/example_process_2.f90!}
```

## `update` - Update the internal state of a process

### Status

Experimental

### Description

The `update` interface allows the internal state of a process object to be updated by querying the system.  
After the process completes, the standard output and standard error are retrieved, if they were requested, and loaded into the `process%stdout` and `process%stderr` string variables, respectively.

This is especially useful for monitoring asynchronous processes and retrieving their output after they have finished.

### Syntax

`call ` [[stdlib_system(module):update(subroutine)]] `(process)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process whose state needs to be updated.  
This is an `intent(inout)` argument, and its internal state is updated on completion.

### Example

```fortran
{!example/system/example_process_5.f90!}
```

## `kill` - Terminate a running process

### Status

Experimental

### Description

The `kill` interface is used to terminate a running external process. It attempts to stop the process and returns a boolean flag indicating whether the operation was successful.
This interface is useful when a process needs to be forcefully stopped, for example, if it becomes unresponsive or if its execution is no longer required.

### Syntax

`call ` [[stdlib_system(module):kill(subroutine)]] `(process, success)`

### Arguments

`process`: Shall be a `type(process_type)` object representing the external process to be terminated.  
This is an `intent(inout)` argument, and on return is updated with the terminated process state.

`success`: Shall be a `logical` variable. It is set to `.true.` if the process was successfully killed, or `.false.` otherwise.

### Example

```fortran
{!example/system/example_process_4.f90!}
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
{!example/system/example_sleep.f90!}
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
{!example/system/example_process_1.f90!}
```

## `get_runtime_os` - Determine the OS type at runtime

### Status

Experimental

### Description

`get_runtime_os` inspects the runtime environment to identify the current OS type. It evaluates environment variables (`OSTYPE`, `OS`) and checks for specific files associated with known operating systems.
The supported OS types are `integer, parameter` variables stored in the `stdlib_system` module:

- **Linux** (`OS_LINUX`)
- **macOS** (`OS_MACOS`)
- **Windows** (`OS_WINDOWS`)
- **Cygwin** (`OS_CYGWIN`)
- **Solaris** (`OS_SOLARIS`)
- **FreeBSD** (`OS_FREEBSD`)
- **OpenBSD** (`OS_OPENBSD`)

If the OS cannot be identified, the function returns `OS_UNKNOWN`.

### Syntax

`os = [[stdlib_system(module):get_runtime_os(function)]]()`

### Class

Function

### Arguments

None.

### Return Value

Returns one of the `integer` `OS_*` parameters representing the OS type, from the `stdlib_system` module, or `OS_UNKNOWN` if undetermined.

### Example

```fortran
{!example/system/example_get_runtime_os.f90!}
```

---

## `OS_TYPE` - Cached OS type retrieval

### Status

Experimental

### Description

`OS_TYPE` provides a cached result of the `get_runtime_os` function. The OS type is determined during the first invocation and stored in a static variable. 
Subsequent calls reuse the cached value, making this function highly efficient.

This caching mechanism ensures negligible overhead for repeated calls, unlike `get_runtime_os`, which performs a full runtime inspection.

### Syntax

`os = [[stdlib_system(module):OS_TYPE(function)]]()`

### Class

Function

### Arguments

None.

### Return Value

Returns one of the `integer` `OS_*` parameters representing the OS type, from the `stdlib_system` module, or `OS_UNKNOWN` if undetermined.

### Example

```fortran
{!example/system/example_os_type.f90!}
```

---

## `is_directory` - Test if a path is a directory

### Status

Experimental

### Description

This function checks if a specified file system path is a directory. 
It is designed to work across multiple platforms. On Windows, paths with both forward `/` and backward `\` slashes are accepted.

### Syntax

`result = [[stdlib_system(module):is_directory(function)]] (path)`

### Class

Function

### Arguments

`path`: Shall be a character string containing the file system path to evaluate. It is an `intent(in)` argument.

### Return values

The function returns a `logical` value:

- `.true.` if the path matches an existing directory.
- `.false.` otherwise, or if the operating system is unsupported.

### Example

```fortran
{!example/system/example_is_directory.f90!}
```

---

## `null_device` - Return the null device file path

### Status

Experimental

### Description

This function returns the file path of the null device, which is a special file used to discard any data written to it. 
It reads as an empty file. The null device's path varies by operating system:
- On Windows, the null device is represented as `NUL`.
- On UNIX-like systems (Linux, macOS), the null device is represented as `/dev/null`.

### Syntax

`path = [[stdlib_system(module):null_device(function)]]()`

### Class

Function

### Arguments

None.

### Return Value

- **Type:** `character(:), allocatable`
- Returns the null device file path as a character string, appropriate for the operating system.

### Example

```fortran
{!example/system/example_null_device.f90!}
```

## `delete_file` - Delete a file

### Status

Experimental

### Description

This subroutine deletes a specified file from the filesystem. It ensures that the file exists and is not a directory before attempting deletion.
If the file cannot be deleted due to permissions, being a directory, or other issues, an error is raised. 
The function provides an optional error-handling mechanism via the `state_type` class. If the `err` argument is not provided, exceptions will trigger an `error stop`.

### Syntax

`call [[stdlib_system(module):delete_file(subroutine)]] (path [, err])`

### Class
Subroutine

### Arguments

`path`: Shall be a character string containing the path to the file to be deleted. It is an `intent(in)` argument.

`err` (optional): Shall be a `type(state_type)` variable for error handling. If provided, errors are returned as a state object. If not provided, the program stops execution on error.

### Behavior

- Checks if the file exists. If not, an error is raised.
- Ensures the path is not a directory before deletion.
- Attempts to delete the file, raising an error if unsuccessful.

### Return values

The file is removed from the filesystem if the operation is successful. If the operation fails, an error is raised.

### Example

```fortran
{!example/system/example_delete_file.f90!}
```

## `joinpath` - Joins the provided paths according to the OS

### Status

Experimental

### Description

This interface joins the paths provided to it according to the platform specific path-separator.
i.e `\` for windows and `/` for others

### Syntax

`res = [[stdlib_system(module):joinpath(interface)]] (p1, p2)`
`res = [[stdlib_system(module):joinpath(interface)]] (p)`

### Class
Pure function

### Arguments

`p1, p2`: Shall be a character string. It is an `intent(in)` argument.
    or
`p`: Shall be a list of character strings. `intent(in)` argument.

### Return values

The resultant path.

### Example

```fortran
{!example/system/example_path_1.f90!}
```

## `operator(/)`

Join two paths according to the platform specific path-separator,
Behavior exactly similar to `joinpath`

### Status

Experimental

### Syntax

`p = lval + rval`

### Class

Pure function.

### Arguments

`lval`: A character string, `intent(in)`.
`rval`: A character string, `intent(in)`.

### Result value

The result is an `allocatable` character string

#### Example

```fortran
{!example/system/example_path_1.f90!}
```

## `splitpath` - splits a path immediately following the last separator

### Status

Experimental

### Description

This subroutine splits a path immediately following the last separator after removing the trailing separators
splitting it into most of the times a directory and a file name.

### Syntax

`call [[stdlib_system(module):splitpath(interface)]] (p, head, tail)`

### Class
Subroutine

### Arguments

`p`: A character string containing the path to be split. `intent(in)`
`head`: The first part of the path. `allocatable, intent(out)`
`tail`: The rest part of the path. `allocatable, intent(out)`

### Behavior

- If `p` is empty, `head` is set to `.` and `tail` is empty
- If `p` consists entirely of path-separators. `head` is set to the path-separator and `tail` is empty
- `head` ends in a path-separator if and only if `p` appears to be a root directory or child of one

### Return values

The splitted path. `head` and `tail`.

### Example

```fortran
{!example/system/example_path_1.f90!}
```

## `basename` - The last part of a path

### Status

Experimental

### Description

This function returns the last part of a path after removing trailing path separators.

### Syntax

`res = [[stdlib_system(module):basename(interface)]] (p)`

### Class
Function

### Arguments

`p`: the path, a character string, `intent(in)`

### Behavior

- The `tail` of `stdlib_system(module):splitpath(interface)` is exactly what is returned. Same Behavior.

### Return values

A character string.

### Example

```fortran
{!example/system/example_path_1.f90!}
```

## `dirname` - Everything except the last part of the path

### Status

Experimental

### Description

This function returns everything except the last part of a path.

### Syntax

`res = [[stdlib_system(module):dirname(interface)]] (p)`

### Class
Function

### Arguments

`p`: the path, a character string, `intent(in)`

### Behavior

- The `head` of `stdlib_system(module):splitpath(interface)` is exactly what is returned. Same Behavior.

### Return values

A character string.

### Example

```fortran
{!example/system/example_path_1.f90!}
```
