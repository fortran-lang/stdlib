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

## `disp` - quickly display your data to the screen (or the default output location)

### Status

Experimental

### Description
Quickly display strings, scalars and low-dimensional arrays to the screen (or the default output location).

### Syntax

For 3D arrays:
`call [[stdlib_io(module):disp(interface)]](value, dim [, string])`  
For null:
`call [[stdlib_io(module):disp(interface)]]()`  
For others:
`call [[stdlib_io(module):disp(interface)]](value [, string])`

### Arguments

`value`: Shall be any type of scalar or (<= 3)D `array`.

`dim`: Shall be a scalar of type `integer` with a value: 1, 2 or 3.

`string`: Shall be a scalar of type `character` with any length(Usually used to mark data information).

### Output

The result is to print your data `value` and comments `string` on the screen (or the default output location).

### Example

```fortran
program demo_io_disp
    use, non_intrinsic :: stdlib_io, only: disp
    implicit none
    real :: r(2, 3)
    complex :: c(2, 3), c_3d(2, 3, 2)
    integer :: i(2, 3)
    logical :: l(2, 3)

    r = 1.; c = 1.; c_3d = 2.; i = 1; l = .true.
    r(1, 1) = (1.e-11, 1.0e-4)
    c(2, 2) = 10.e5
    c_3d(1,3,1) = (1000, 0.001)
        call disp('string', 'disp(string):')
        call disp('It is a note.')
        call disp()

        call disp(r, 'disp(r):')
        call disp(c, 'disp(c):')
        call disp(i, 'disp(i):')
        call disp(l, 'disp(l):')

        call disp(c_3d, 3, 'disp(c_3d, 3):')
        call disp(c_3d, 2, 'disp(c_3d, 2):')
end program demo_io_disp
```
**Result:**
```fortran
 disp(string):
 string
 It is a note.

 disp(r):
 0.1000E-10   1.000       1.000
  1.000       1.000       1.000
 disp(c):
          (1.000,0.000)           (1.000,0.000)           (1.000,0.000)
          (1.000,0.000)      (0.1000E+07,0.000)           (1.000,0.000)
 disp(i):
          1           1           1
          1           1           1
 disp(l):
          T           T           T
          T           T           T
 disp(c_3d, 3):
 Slice (:,:,1):
          (2.000,0.000)           (2.000,0.000)      (1000.,0.1000E-02)
          (2.000,0.000)           (2.000,0.000)           (2.000,0.000)
 Slice (:,:,2):
          (2.000,0.000)           (2.000,0.000)           (2.000,0.000)
          (2.000,0.000)           (2.000,0.000)           (2.000,0.000)
 disp(c_3d, 2):
 Slice (:,1,:):
          (2.000,0.000)           (2.000,0.000)
          (2.000,0.000)           (2.000,0.000)
 Slice (:,2,:):
          (2.000,0.000)           (2.000,0.000)
          (2.000,0.000)           (2.000,0.000)
 Slice (:,3,:):
     (1000.,0.1000E-02)           (2.000,0.000)
          (2.000,0.000)           (2.000,0.000)
```
