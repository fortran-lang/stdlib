---
title: io
---

# IO

[TOC]

## `loadtxt` - load a 2D array from a text file

### Status

Experimental

### Description
Loads a rank-2 `array` from a text file.

### Syntax

`call [[stdlib_io(module):loadtxt(interface)]](filename, array)`

### Arguments

`filename`: Shall be  a character expression containing the file name from which to load the rank-2 `array`.

`array`: Shall be an allocatable rank-2 array of type `real`, `complex` or `integer`.

### Return value

Returns an allocated rank-2 `array` with the content of `filename`.

### Example

```fortran
program demo_loadtxt
    use stdlib_io, only: loadtxt
    implicit none
    real, allocatable :: x(:,:)
    call loadtxt('example.dat', x)
end program demo_loadtxt
```


## `open` - open a file

### Status

Experimental

### Description

Returns the unit number of a file opened to read, to write, or to read and write. The file might be a text file or a binary file. All files are opened using a streamed access.

### Syntax

`u = [[stdlib_io(module):open(function)]](filename [, mode] [, iostat])`

### Arguments

`filename`: Shall be a character expression containing the name of the file to open.

`mode` (optional): Shall be a character expression containing characters describing the way in which the file will be used. The available modes are:


| Character | Meaning |
| --------- | ------- |
| `'r'` | open for reading (default) |
| `'w'` | open for writing, truncating the file first |
| `'x'` | open for exclusive creation, failing if the file already exists |
| `'a'` | open for writing, appending to the end of the file if it exists |
| `'+'` | open for updating (reading and writing) |
| `'b'` | binary mode |
| `'t'` | text mode (default) |


The default `mode` is `'rt'` (i.e. open for reading a text file). The `mode` may include one of the four different methods for opening a file (i.e., `'r'`, `'w'`, `'x'`, and `'a'`). These four methods can be associated with the character `'+'` to open the file for updating. In addition, it can be specified if the file should be handled as a binary file (`'b'`) or a text file (`'t'`).

`iostat` (optional): Shall be a scalar of type `integer` that receives the error status of `open`, if provided. If no error exists, `iostat` is zero.

`u`: Shall be a scalar of type `integer` that specifies the unit number associated with the file `filename`.


### Return value

The result is a scalar of type `integer`.

### Example

```fortran
program demo_open
    use stdlib_io, only: open
    implicit none
    integer :: u
    u = open('example.dat', 'wt')
    write(u,'(a)')'This is an example for open'
    close(u)
end program demo_open
```


## `savetxt` - save a 2D array into a text file

### Status

Experimental

### Description
Saves a rank-2 `array` into a text file.

### Syntax

`call [[stdlib_io(module):savetxt(interface)]](filename, array)`

### Arguments

`filename`: Shall be  a character expression containing the name of the file that will contain the 2D `array`.

`array`: Shall be a rank-2 array of type `real`, `complex` or `integer`.

### Output

Provides a text file called `filename` that contains the rank-2 `array`.

### Example

```fortran
program demo_savetxt
    use stdlib_io, only: savetxt
    implicit none
    real :: x(3,2) = 1
    call savetxt('example.dat', x)
end program demo_savetxt
```


## `load_npy`

### Status

Experimental

### Description

Loads an `array` from a npy formatted binary file.

### Syntax

`call [[stdlib_io_npy(module):load_npy(interface)]](filename, array[, iostat][, iomsg])`

### Arguments

`filename`: Shall be  a character expression containing the file name from which to load the `array`.
            This argument is `intent(in)`.

`array`: Shall be an allocatable array of any rank of type `real`, `complex` or `integer`.
         This argument is `intent(out)`.

`iostat`: Default integer, contains status of loading to file, zero in case of success.
          It is an optional argument, in case not present the program will halt for non-zero status.
          This argument is `intent(out)`.

`iomsg`: Deferred length character value, contains error message in case `iostat` is non-zero.
         It is an optional argument, error message will be dropped if not present.
         This argument is `intent(out)`.

### Return value

Returns an allocated `array` with the content of `filename` in case of success.

### Example

```fortran
program demo_loadnpy
    use stdlib_io_npy, only: load_npy
    implicit none
    real, allocatable :: x(:,:)
    call loadtxt('example.npy', x)
end program demo_loadnpy
```


## `save_npy`

### Status

Experimental

### Description

Saves an `array` into a npy formatted binary file.

### Syntax

`call [[stdlib_io_npy(module):save_npy(interface)]](filename, array[, iostat][, iomsg])`

### Arguments

`filename`: Shall be  a character expression containing the name of the file that will contain the `array`.
            This argument is `intent(in)`.

`array`: Shall be an array of any rank of type `real`, `complex` or `integer`.
         This argument is `intent(in)`.

`iostat`: Default integer, contains status of saving to file, zero in case of success.
          It is an optional argument, in case not present the program will halt for non-zero status.
          This argument is `intent(out)`.

`iomsg`: Deferred length character value, contains error message in case `iostat` is non-zero.
         It is an optional argument, error message will be dropped if not present.
         This argument is `intent(out)`.

### Output

Provides a npy file called `filename` that contains the rank-2 `array`.

### Example

```fortran
program demo_savenpy
    use stdlib_io_npy, only: save_npy
    implicit none
    real :: x(3,2) = 1
    call save_npy('example.npy', x)
end program demo_savenpy
```

## `getline`

### Status

Experimental

### Description

Read a whole line from a formatted unit into a string variable

### Syntax

`call [[stdlib_io(module):getline(interface)]] (unit, line[, iostat][, iomsg])`
`call [[stdlib_io(module):getline(interface)]] (line[, iostat][, iomsg])`

### Arguments

`unit`: Formatted input unit.
        This argument is `intent(in)`.
        If `unit` is not specified standard input is used.

`line`: Deferred length character or `string_type` variable.
        This argument is `intent(out)`.

`iostat`: Default integer, contains status of reading from unit, zero in case of success.
          It is an optional argument, in case not present the program will halt for non-zero status.
          This argument is `intent(out)`.

`iomsg`: Deferred length character value, contains error message in case `iostat` is non-zero.
         It is an optional argument, error message will be dropped if not present.
         This argument is `intent(out)`.

### Example

```fortran
program demo_getline
    use, intrinsic :: iso_fortran_env, only : input_unit, output_unit
    use stdlib_io, only: getline
    implicit none
    character(len=:), allocatable :: line
    integer :: stat

    call getline(input_unit, line, stat)
    do while(stat == 0)
      write(output_unit, '(a)') line
      call getline(input_unit, line, stat)
    end do
end program demo_getline
```

## Formatting constants

### Status

Experimental

### Description

Formatting constants for printing out integer, floating point, and complex numbers at their full precision.
Provides formats for all kinds as defined in the `stdlib_kinds` module.

### Example

```fortran
program demo_fmt_constants
    use, intrinsic :: iso_fortran_env, only : int32, int64, real32, real64
    use stdlib_io, only : FMT_INT, FMT_REAL_SP, FMT_REAL_DP, FMT_COMPLEX_SP, FMT_COMPLEX_DP
    implicit none

    integer(kind=int32)  :: i32
    integer(kind=int64)  :: i64
    real(kind=real32)    :: r32
    real(kind=real64)    :: r64
    complex(kind=real32) :: c32
    complex(kind=real64) :: c64

    i32 = 100_int32
    i64 = 100_int64
    r32 = 100.0_real32
    r64 = 100.0_real64
    c32 = cmplx(100.0_real32, kind=real32)
    c64 = cmplx(100.0_real64, kind=real64)

    print "2("//FMT_INT//",1x)", i32, i64
    print FMT_REAL_SP, r32
    print FMT_REAL_DP, r64
    print FMT_COMPLEX_SP, c32
    print FMT_COMPLEX_DP, c64

```
