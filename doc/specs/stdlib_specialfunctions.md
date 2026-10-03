# Special functions

```{contents}
:local:
:depth: 2
```

## `legendre` - Calculate Legendre polynomials

### Status

Experimental

### Description

Computes the value of the n-th Legendre polynomial at a specified point.
Currently only 64 bit floating point is supported.

This is an `elemental` function.

### Syntax

`result = ` `legendre` ` (n, x)`

### Arguments

`n`: Shall be a scalar of type `real(real64)`. 

`x`: Shall be a scalar or array (this function is elemental) of type `real(real64)`. 

### Return value

The function result will be the value of the `n`-th Legendre polynomial, evaluated at `x`.



## `dlegendre` - Calculate first derivatives of Legendre polynomials

### Status

Experimental

### Description

Computes the value of the first derivative of the n-th Legendre polynomial at a specified point.
Currently only 64 bit floating point is supported.

This is an `elemental` function.

### Syntax

`result = ` `dlegendre` ` (n, x)`

### Arguments

`n`: Shall be a scalar of type `real(real64)`. 

`x`: Shall be a scalar or array (this function is elemental) of type `real(real64)`. 

### Return value

The function result will be the value of the first derivative of the `n`-th Legendre polynomial, evaluated at `x`.
