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

## `disp` - display your data to the screen (or another output unit)

### Status

Experimental

### Description
Display any type (`logical, integer, real, complex, character, string_type`) of scalar,   
and display some data type (`logical, integer, real, complex`) of vector or matrix.

Make good use of similar to the following usage, can help you understand the data information in the `array`.
```fortran
call disp( A(i, j, 2, :, 1:10) [, unit, header, brief] )    !! `i, j, ...` can be determined by `do` loop.
```

Generally, except for `complex` type, any other type of scalar or single element of the `array` will be printed out with a width of 12 characters and a space separator.  
For `complex` type, scalar or single element of the `array` will be printed out with a width of 25 characters and a space separator.

### Syntax

General API:
`call [[stdlib_io(module):disp(interface)]](value [, unit, header, brief])`

For null:
`call [[stdlib_io(module):disp(interface)]]()`  

### Arguments

`value`: Shall be any type of scalar, and some data type (`logical, integer, real, complex`) of vector or matrix.
    This is an `intent(in)` argument.

`unit`: Shall be an `integer` scalar link to an IO stream.
    This is an `intent(in)` and `optional` argument.

`header`: Shall be a scalar of type `character` with any length (usually used to comment data information).
    This is an `intent(in)` and `optional` argument.

`brief`: Shall be an `logical` scalar, controlling an abridged version of the `value` object is printed.
    This is an `intent(in)` and `optional` argument.

### Output

The result is to print `header` and `value` on the screen (or another output unit) in this order.  
If `value` is a `array` type, the dimension length information of the `array` will also be outputted.

### Example

```fortran
program test_io_disp
    
    use :: stdlib_io, only: disp
    implicit none
    real(8) :: r(2, 3)
    complex :: c(2, 3), c_3d(2, 100, 20)
    integer :: i(2, 3)
    logical :: l(10, 10)

    r = 1.; c = 1.; c_3d = 2.; i = 1; l = .true.
    r(1, 1) = -1.e-11
    r(1, 2) = -1.e10
    c(2, 2) = (-1.e10,-1.e10)
    c_3d(1,3,1) = (1000, 0.001)
    c_3d(1,3,2) = (1.e4, 100.)
    call disp('string', header='disp(string):')
    call disp('It is a note.')
    call disp()

    call disp(r, header='disp(r):')
    call disp(r(1,:), header='disp(r(1,:))')
    call disp(c, header='disp(c):')
    call disp(i, header='disp(i):')
    call disp(l, header='disp(l):', brief=.true.)

    call disp(c_3d(:,:,3), header='disp(c_3d(:,:,3)):', brief=.true.)
    call disp(c_3d(2,:,:), header='disp(c_3d(2,:,:)):', brief=.true.)

end program test_io_disp
```
**Result:**
```fortran
 disp(string):
 string
 It is a note.

 disp(r):
 [matrix size: 2×3]
 -0.1000E-10  -0.1000E+11    1.000
   1.000        1.000        1.000
 disp(r(1,:))
 [vector size: 3]
 -0.1000E-10  -0.1000E+11    1.000
 disp(c):
 [matrix size: 2×3]
            (1.000,0.000)             (1.000,0.000)             (1.000,0.000)
            (1.000,0.000) (-0.1000E+11,-0.1000E+11)             (1.000,0.000)
 disp(i):
 [matrix size: 2×3]
           1            1            1
           1            1            1
 disp(l):
 [matrix size: 10×10]
           T            T            T          ...            T
           T            T            T          ...            T
           T            T            T          ...            T
           :            :            :            :            :
           T            T            T          ...            T
 disp(c_3d(:,:,3)):
 [matrix size: 2×100]
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
 disp(c_3d(2,:,:)):
 [matrix size: 100×20]
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000)  
                        :                         :                         :                         :                         :  
            (2.000,0.000)             (2.000,0.000)             (2.000,0.000)                       ...             (2.000,0.000) 
```
