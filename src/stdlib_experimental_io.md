# IO

## Implemented

 * `loadtxt`
 * `open`
 * `savetxt`


## `loadtxt` - load a 2D array from a text file

### Description
Loads a rank-2 `array` from a text file.

### Syntax

`call loadtxt(filename, array)`

### Arguments

`filename`: Shall be  a character expression containing the file name from which to load the rank-2 `array`.

`array`: Shall be an allocatable rank-2 array of type `real` or `integer`.

### Return value

Returns an allocated rank-2 `array` with the content of `filename`.

### Example

```fortran
program test
    use stdlib_experimental_io, only: loadtxt
    implicit none
    real, allocatable :: x(:,:)
    call loadtxt('example.dat', x) 
end program
```


## `open` - open a file

### Description

Returns the unit number of a file opened to read, to write, or to read and write. The file might be a text file or a binary file. All files are opened using a streamed access.

### Syntax

`u = open(filename [, mode] [, iostat])`

### Arguments

`filename`: Shall be a character expression containing the name of the file to open.

`mode` (optional): Shall be a character expression containing characters describing the way in which the file will be used. The available modes are:


| Character | Meaning |
| --------- | ------- |
| `r` | open for reading (default) |
| `w` | open for writing, truncating the file first |
| `x` | open for exclusive creation, failing if the file already exists |
| `a` | open for writing, appending to the end of the file if it exists |
| `b` | binary mode |
| `t` | text mode (default) |
| `+` | open for updating (reading and writing) |


The default mode is `rt` (i.e. open for reading a text file).

`iostat` (optional): Shall be a scalar of type `integer` that receives the error status of `open`, if provided. If no error exists, `iostat` is zero.

`u`: Shall be a scalar of type `integer` that specifies the unit number associated with the file `filename`.


### Return value

The result is a scalar of type `integer`.

### Example

```fortran
program test
    use stdlib_experimental_stats, only: mean
    implicit none
    integer :: io, u
    u = open('example.dat', 'rt', iostat = io)
end program
```


## `savetxt` - save a 2D array into a text file

### Description
Saves a rank-2 `array` into a text file.

### Syntax

`call savetxt(filename, array)`

### Arguments

`filename`: Shall be  a character expression containing the name of the file that will contain the 2D `array`.

`array`: Shall be a rank-2 array of type `real` or `integer`.

### Output

Provides a text file called `filename` that contains the rank-2 `array`.

### Example

```fortran
program test
    use stdlib_experimental_io, only: savetxt
    implicit none
    real :: x(3,2) = 1
    call savetxt('example.dat', x) 
end program
```
