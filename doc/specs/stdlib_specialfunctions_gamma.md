---
title: specialfunctions_gamma
---

# Special functions gamma

[TOC]

## `gamma` - Calculate gamma function with any number

### Status

Experimental

### Description

Intrinsic gamma function provides values for real type argument with single and double precision. Here the gamma function is extended to both integer and complex numbers.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):gamma(interface)]] (x)`

### Class

Elemental function

### Arguments

`x`: should be an integer or a complex type number

### Return value

The function returns a value with the same type and kind as input argument.


## `log_gamma` - calculate logarithm gamma function with any number

### Status

Experimental

### Description

Intrinsic log_gamma function provides values for real type arguments with single and double precisions. Here the log_gamma function is extended to both integer and complex numbers.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):log_gamma(interface)]] (x)`

### Class

Elemental function

### Arguments

`x`: Shall be an integer or a complex type number. 

### Return value

The function returns a value with the same type and kind as input argument. For integer argument, the result is single precision real type.

### Example

```fortran
program demo_log_gamma
    use stdlib_specialfunctions_gamma, only : lg => log_gamma
    implicit none
    
    integer :: i
    real :: x
    real(real64) :: y
    complex :: z
    
    i = 10
    x = 8.76
    y = x
    z = (5.345, -3.467)
    print *, lg(i)            !default single precision output
!12.8018274

    print *, lg(x)            !same kind as input
    
!10.0942659

    print *, lg(y)            !same kind as input
    
!10.094265528673880

    print *, lg(z)            !same kind as input
    
!(2.56165719, 0.549360633)

end program demo_log_gamma
```

## `log_factorial` - calculate logarithm of a factorial

### Status

Experimental

### Description

Compute the logarithm of factorial, log(n!)

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):log_factorial(interface)]] (x)`

### Class

Elemental function

### Arguments

`x`: Shall be an integer type number. 

### Return value

The function returns a value with single precision real type.


## `lower_incomplete_gamma` - calculate lower incomplete gamma integral

### Status

Experimental

### Description

The lower incomplete gamma function is defined as:

\gamma (p, x) = \int_{0}^{x}t^{p-1}e^{-t}dt, \; \; p >0,\; x,p\in \mathbb{R}

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):lower_incomplete_gamma(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.


## `upper_incomplete_gamma` - calculate upper incomplete gamma integral

### Status

Experimental

### Description

The upper incomplete gamma function is defined as:

\\Gamma (p, x) = \int_{x}^{\infty }t^{p-1}e^{-t}dt, \; \; p >0,\; x,p\in \mathbb{R}

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):upper_incomplete_gamma(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.


## `log_lower_incomplete_gamma` - calculate logarithm of the lower incomplete gamma integral

### Status

Experimental

### Description

Compute the logarithm of the absolute value of the lower incomplete gamma function.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):log_lower_incomplete_gamma(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.


## `log_upper_incomplete_gamma` - calculate logarithm of the upper incomplete gamma integral

### Status

Experimental

### Description

Compute the logarithm of the absolute value of the upper incomplete gamma function.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):log_upper_incomplete_gamma(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.


## `regularized_gamma_p` - calculate the gamma quotient P

### Status

Experimental

### Description

The regularized gamma quotient p, also known as normalized incomplete gamma function, is defined as:

P(p,x)=\gamma(p,x)/\Gamma(p)

The values of regularized gamma P is in the range of [0, 1]

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):regularized_gamma_p(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.


## `regularized_gamma_q` - calculate the gamma quotient Q

### Status

Experimental

### Description

The regularized gamma quotient Q is defined as:

Q(p,x)=\Gamma(p,x)/\Gamma(p)=1-P(p,x)

The values of regularized gamma Q is in the range of [0, 1]

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):regularized_gamma_q(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.
