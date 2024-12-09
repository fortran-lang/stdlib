---
title: system
---

# System and sub-processing module

[TOC]

## `run` - Execute a synchronous command

### Status

Experimental

### Description

This subroutine executes a command in the system shell synchronously, waiting for its completion before returning. It provides the option to capture the command's standard output (`stdout`) and standard error (`stderr`), along with its exit and command states. 

The implementation relies on Fortran's `execute_command_line`.

### Syntax

`call [[stdlib_system(module):run(subroutine)]](cmd [, exit_state] [, command_state] [, stdout] [, stderr])`

### Class

Subroutine

### Arguments

`cmd`: Shall be a scalar `character(len=*)` input argument containing the shell command to execute.

`exit_state` (optional): Shall be an integer `intent(out)` argument, returning the command's exit state (usually `0` on success).

`command_state` (optional): Shall be an integer `intent(out)` argument, indicating issues with command invocation.

`stdout` (optional): Shall be an `intent(out)` `type(string_type)` variable, capturing the command's standard output.

`stderr` (optional): Shall be an `intent(out)` `type(string_type)` variable, capturing the command's standard error messages.

### Return Values

- Captures the exit state and command state of the executed command.
- Retrieves `stdout` and/or `stderr` if the respective optional arguments are provided.
- Raises an error via `error stop` if no `exit_state` or `command_state` arguments are provided and an issue occurs.

### Example

```fortran
program example_run
    use stdlib_system, only: run
    implicit none
    type(string_type) :: output, error_output
    integer :: exit_status, cmd_status

    call run("ls -l", exit_state=exit_status, command_state=cmd_status, stdout=output, stderr=error_output)

    if (exit_status == 0) then
        print *, "Command executed successfully!"
        print *, "Output:", trim(output)
    else
        print *, "Error occurred:", trim(error_output)
    end if
end program example_run
```

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
program example_null_device
    use stdlib_system, only: null_device
    implicit none
    character(:), allocatable :: null_path

    ! Retrieve the null device path
    null_path = null_device()

    print *, "The null device path is: ", null_path
end program example_null_device
```

## `runtime_os` - Determine the OS type at runtime

### Status

Experimental

### Description

`runtime_os` inspects the runtime environment to identify the current OS type. It evaluates environment variables (`OSTYPE`, `OS`) and checks for specific files associated with known operating systems.
The supported OS types are:

- **Linux** (`OS_LINUX`)
- **macOS** (`OS_MACOS`)
- **Windows** (`OS_WINDOWS`)
- **Cygwin** (`OS_CYGWIN`)
- **Solaris** (`OS_SOLARIS`)
- **FreeBSD** (`OS_FREEBSD`)
- **OpenBSD** (`OS_OPENBSD`)

If the OS cannot be identified, the function returns `OS_UNKNOWN`.

### Syntax

`os = [[stdlib_system(module):runtime_os(function)]]()`

### Class

Function

### Arguments

None.

### Return Value

- **Type:** `integer`
- Returns a constant representing the OS type, or `OS_UNKNOWN` if undetermined.

### Example

```fortran
program example_os_detection
    use stdlib_system, only: OS_TYPE, runtime_os
    implicit none
    integer :: os_type_cached, os_type_runtime

    ! Cached OS detection
    os_type_cached = OS_TYPE()
    print *, "Cached OS Type: ", os_type_cached

    ! Runtime OS detection (full inspection)
    os_type_runtime = runtime_os()
    print *, "Runtime OS Type: ", os_type_runtime
end program example_os_detection
```

---

## `OS_TYPE` - Cached OS type retrieval

### Status

Experimental

### Description

`OS_TYPE` provides a cached result of the `runtime_os` function. The OS type is determined during the first invocation and stored in a static variable. 
Subsequent calls reuse the cached value, making this function highly efficient.

This caching mechanism ensures negligible overhead for repeated calls, unlike `runtime_os`, which performs a full runtime inspection.

### Syntax

`os = [[stdlib_system(module):OS_TYPE(function)]]()`

### Class

Function

### Arguments

None.

### Return Value

- **Type:** `integer`
- Returns a cached constant representing the OS type, as determined by `runtime_os`.

---

### Example

```fortran
program example_os_detection
    use stdlib_system, only: OS_TYPE, runtime_os
    implicit none
    integer :: os_type_cached, os_type_runtime

    ! Cached OS detection
    os_type_cached = OS_TYPE()
    print *, "Cached OS Type: ", os_type_cached

    ! Runtime OS detection (full inspection)
    os_type_runtime = runtime_os()
    print *, "Runtime OS Type: ", os_type_runtime
end program example_os_detection
```

