# Catching and handling errors

* [`check` - Checks the value of a logical condition](#check---checks-the-value-of-a-logical-condition)
* [`error_stop` - aborts a program](#error_stop---aborts-a-program)


## `check` - Checks the value of a logical condition

### Description

Checks the value of a logical condition.

### Syntax

`call check(condition, msg, code, warn)`

### Arguments

`condition`: Shall be a scalar of type `logical`.

`msg` (optional): Shall be a character expression containing the message to be printed to `stderr`. The default `msg` is 'Check failed.'.

`code` (optional): Shall be a scalar of type `integer`. The default `code` is `1`.

`warn` (optional): Shall be a scalar of type `logical`. The default `warn` is `.true.`.

### Return value

If `condition` is `.false`., and:

 * no other arguments are provided, this subroutine stops the program with the default message and exit code 1;

 * `msg` is provided, this subroutine stops the program and it prints the value of `msg`;

 * `code` is provided, this subroutine stops the program with the given exit code;

 * `warn` is provided and `warn` is `.true.`, this subroutine doesn't stop the program and prints the message.

### Examples

```fortran
program demo_check1
    use stdlib_experimental_error, only: check
    implicit none
    integer :: a = 1
    ! If a /= 5, stops the program with exit code 1 and prints 'Check failed.'
    call check(a == 5)
end program demo_check1
```
```fortran
program demo_check2
    use stdlib_experimental_error, only: check
    implicit none
    integer :: a = 1
    ! If a /= 5, stops the program with exit code 1 and prints  'a == 5 failed.'
    call check(a == 5, msg='a == 5 failed.')
end program demo_check2
```
```fortran
program demo_check3
    use stdlib_experimental_error, only: check
    implicit none
    integer :: a = 1
    ! If a /= 5,  prints 'a == 5 failed.', but doesn't stop the program.
    call check(a == 5, msg='a == 5 failed.', warn=.true.)
end program demo_check2
```
```fortran
program demo_check3
    use stdlib_experimental_error, only: check
    implicit none
    integer :: a = 1
    ! If a /= 5, stops the program with exit code 77 and prints 'a == 5 failed.'
    call check(a == 5, msg='a == 5 failed.', code=77)
end program demo_check3
```

## `error_stop` - aborts a program

### Description

Aborts the program with a message and a nonzero exit code.

### Syntax

`call error_stop(msg, code)`

### Arguments

`msg`: Shall be a character expression containing the message to be printed to `stderr`.

`code` (optional): Shall be a scalar of type `integer` to be returned as exit code.

### Output

Aborts the program with printing the message `msg` to `stderr` and a nonzero exit code. The nonzero exit code is equal to `code` if provided.

### Example

```fortran
program demo_error_stop
    use stdlib_experimental_error, only: error_stop
    implicit none
    call error_stop("Invalid argument", code = 123)
end program demo_error_stop
```
