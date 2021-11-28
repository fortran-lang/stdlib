---
title: IO
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

Loads am `array` from a npy formatted binary file.

### Syntax

`call [[stdlib_io_npy(module):load_npy(interface)]](filename, array[, iostat][, iomsg])`

### Arguments

`filename`: Shall be  a character expression containing the file name from which to load the `array`.

`array`: Shall be an allocatable array of any rank of type `real`, `complex` or `integer`.

`iostat`: Default integer, contains status of loading to file, zero in case of success.
          Optional argument, in case not present the program will halt for non-zero status.

`iomsg`: Deferred length character value, contains error message in case `iostat` is non-zero.
         Optional argument, error message will be dropped if not present.

### Return value

Returns an allocated `array` with the content of `filename` in case of success.

### Example

```fortran
program demo_loadtxt
    use stdlib_io_npy, only: load_npy
    implicit none
    real, allocatable :: x(:,:)
    call loadtxt('example.npy', x)
end program demo_loadtxt
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

`array`: Shall be an array of any rank of type `real`, `complex` or `integer`.

`iostat`: Default integer, contains status of saving to file, zero in case of success.
          Optional argument, in case not present the program will halt for non-zero status.

`iomsg`: Deferred length character value, contains error message in case `iostat` is non-zero.
         Optional argument, error message will be dropped if not present.

### Output

Provides a text file called `filename` that contains the rank-2 `array`.

### Example

```fortran
program demo_savetxt
    use stdlib_io_npy, only: save_npy
    implicit none
    real :: x(3,2) = 1
    call save_npy('example.npy', x)
end program demo_savetxt
```
