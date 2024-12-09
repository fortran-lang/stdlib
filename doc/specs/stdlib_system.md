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
