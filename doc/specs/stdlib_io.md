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

`call ` [[stdlib_io(module):loadtxt(interface)]] `(filename, array [, skiprows] [, max_rows] [, fmt])`

### Arguments

`filename`: Shall be  a character expression containing the file name from which to load the rank-2 `array`.

`array`: Shall be an allocatable rank-2 array of type `real`, `complex` or `integer`.

`skiprows` (optional): Skip the first `skiprows` lines. If skipping more rows than present, a 0-sized array will be returned. The default is 0.

`max_rows` (optional): Read `max_rows` lines of content after `skiprows` lines. A negative value results in reading all lines. A value of zero results in no lines to be read. The default value is -1.

`fmt` (optional): Fortran format specifier for the text read.  Defaults to the write format for the data type.  Setting fmt='*' will specify list directed read.   



### Return value

Returns an allocated rank-2 `array` with the content of `filename`.

### Example

```fortran
{!example/io/example_loadtxt.f90!}
```


## `open` - open a file

### Status

Experimental

### Description

Returns the unit number of a file opened to read, to write, or to read and write. The file might be a text file or a binary file. All files are opened using a streamed access.

### Syntax

`u = ` [[stdlib_io(module):open(function)]] `(filename [, mode] [, iostat])`

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
{!example/io/example_open.f90!}
```


## `savetxt` - save a 2D array into a text file

### Status

Experimental

### Description
Saves a rank-2 `array` into a text file.

### Syntax

`call ` [[stdlib_io(module):savetxt(interface)]] `(filename, array)`

### Arguments

`filename`: Shall be  a character expression containing the name of the file that will contain the 2D `array`.

`array`: Shall be a rank-2 array of type `real`, `complex` or `integer`.

### Output

Provides a text file called `filename` that contains the rank-2 `array`.

### Example

```fortran
{!example/io/example_savetxt.f90!}
```


## `load_npy`

### Status

Experimental

### Description

Loads an `array` from a npy formatted binary file.

### Syntax

`call ` [[stdlib_io_npy(module):load_npy(interface)]] `(filename, array[, iostat][, iomsg])`

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
{!example/io/example_loadnpy.f90!}
```


## `save_npy`

### Status

Experimental

### Description

Saves an `array` into a npy formatted binary file.

### Syntax

`call ` [[stdlib_io_npy(module):save_npy(interface)]] `(filename, array[, iostat][, iomsg])`

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
{!example/io/example_savenpy.f90!}
```

## `getline`

### Status

Experimental

### Description

Read a whole line from a formatted unit into a string variable

### Syntax

`call ` [[stdlib_io(module):getline(interface)]] ` (unit, line[, iostat][, iomsg])`

`call ` [[stdlib_io(module):getline(interface)]] ` (line[, iostat][, iomsg])`

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
{!example/io/example_getline.f90!}
```

## Formatting constants

### Status

Experimental

### Description

Formatting constants for printing out integer, floating point, and complex numbers at their full precision.
Provides formats for all kinds as defined in the `stdlib_kinds` module.

### Example

```fortran
{!example/io/example_fmt_constants.f90!}
```
