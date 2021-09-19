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

## `disp` - display your data

### Status

Experimental

### Class

Impure subroutine.

### Description

Outputs a `logical/integer/real/complex/character/string_type` scalar or `logical/integer/real/complex` and rank-1/rank-2 array to the screen or a file `unit`.

#### More details

```fortran
call disp( A(i, j, 2, :, 1:10) [, header, unit, brief] )    !! `i, j, ...` can be determined by `do` loop.
```

For `complex` type, the output format is `*(A25, 1X)`; 
For    other types, the output format is `*(A12, 1X)`.

To prevent users from accidentally passing large-length arrays to `disp`, causing unnecessary io blockage:
1. If the `brief` argument is not specified, `disp` will print **the brief array content with a length of `10*50` by default**.
2. Specify `brief=.true.`, `disp` will print **the brief array content with a length of `5*5`**;
3. Specify `brief=.false.`, `disp` will print **`all` the contents of the array**.

### Syntax

`call [[stdlib_io(module):disp(interface)]]([x, header, unit, brief])`  

### Arguments

`x`: Shall be a `logical/integer/real/complex/string_type` scalar or `logical/integer/real/complex` and rank-1/rank-2 array.
This argument is `intent(in)` and `optional`.

`header`: Shall be a `character(len=*)` scalar. 
This argument is `intent(in)` and `optional`.

`unit`: Shall be an `integer` scalar linked to an IO stream.
This argument is `intent(in)` and `optional`.

`brief`: Shall be a `logical` scalar.
This argument is `intent(in)` and `optional`.  
Controls an abridged version of the `x` object is printed.

### Output

The result is to print `header` and `x` on the screen (or another output `unit/file`) in this order.  
If `x` is a rank-1/rank-2 `array` type, the dimension length information of the `array` will also be outputted.

If `disp` is not passed any arguments, a blank line is printed.

If the `x` is present and of `real/complex` type, the data will retain four significant decimal places, like `(g0.4)`.

### Example

```fortran
program test_io_disp
    
    use stdlib_io, only: disp

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
**Results:**
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