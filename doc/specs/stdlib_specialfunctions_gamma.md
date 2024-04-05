---
title: specialfunctions_gamma
---

# Special functions gamma

[TOC]

## `gamma` - Calculate the gamma function

### Status

Experimental

### Description

The gamma function is defined as the analytic continuation of a convergent improper integral function on the whole complex plane except zero and negative integers:

$$\Gamma(z)=\int_{0}^{\infty}x^{z-1}e^{-x}dx, \;\;  z\in \mathbb{C} \setminus 0, -1, -2, \cdots$$

Fortran 2018 standard implements the intrinsic gamma function of real type argument in single and double precisions. Here the gamma function is extended to both integer and complex arguments. The values of the gamma function with integer arguments are exact. The values of the gamma function with complex arguments are approximated in single and double precisions by using Lanczos approximation.

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):gamma(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: should be a positive integer or a complex type number

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```fortran
{!example/specialfunctions_gamma/example_gamma.f90!}
```

## `log_gamma` - Calculate the natural logarithm of the gamma function

### Status

Experimental

### Description

Mathematically, logarithm of gamma function is a special function with complex arguments by itself. Due to the different branch cut structures and a different principal branch, natural logarithm of gamma function log_gamma(z) with complex argument is different from the ln(Gamma(z)). The two have the same real part but different imaginary part. 

Fortran 2018 standard implements intrinsic log_gamma function of absolute value of real type argument in single and double precision. Here the log_gamma function is extended to both integer and complex arguments. The values of log_gamma function with complex arguments are approximated in single and double precisions by using Stirling's approximation.

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):log_gamma(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a positive integer or a complex type number. 

### Return value

The function returns real single precision values for integer input arguments, while it returns complex values with the same kind as complex input arguments.

### Example

```fortran
{!example/specialfunctions_gamma/example_log_gamma.f90!}
```

## `log_factorial` - calculate the logarithm of a factorial

### Status

Experimental

### Description

Compute the natural logarithm of factorial, log(n!)

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):log_factorial(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a positive integer type number. 

### Return value

The function returns real type values with single precision.

### Example
```fortran
{!example/specialfunctions_gamma/example_log_factorial.f90!}
```

## `lower_incomplete_gamma` - calculate lower incomplete gamma integral

### Status

Experimental

### Description

The lower incomplete gamma function is defined as:

$$\gamma(p,x)=\int_{0}^{x}t^{p-1}e^{-t}dt, \;\;  p > 0, x\in \mathbb{R}$$

When x < 0, p must be positive integer.

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):lower_incomplete_gamma(interface)]] ` (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.

### Example
```fortran
{!example/specialfunctions_gamma/example_ligamma.f90!}
```

## `upper_incomplete_gamma` - calculate the upper incomplete gamma integral

### Status

Experimental

### Description

The upper incomplete gamma function is defined as:

$$\Gamma (p, x) = \int_{x}^{\infty }t^{p-1}e^{-t}dt, \; \; p >0,\; x \in \mathbb{R}$$

When x < 0, p must be a positive integer.

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):upper_incomplete_gamma(interface)]] ` (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.

### Example
```fortran
{!example/specialfunctions_gamma/example_uigamma.f90!}
```

## `log_lower_incomplete_gamma` - calculate the natural logarithm of the lower incomplete gamma integral

### Status

Experimental

### Description

Compute the natural logarithm of the absolute value of the lower incomplete gamma function.

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):log_lower_incomplete_gamma(interface)]] ` (p, x)`

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

Compute the natural logarithm of the absolute value of the upper incomplete gamma function.

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):log_upper_incomplete_gamma(interface)]] ` (p, x)`

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

The regularized gamma quotient P, also known as normalized incomplete gamma function, is defined as:

$$P(p,x)=\gamma(p,x)/\Gamma(p)$$

The values of regularized gamma P is in the range of [0, 1]

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):regularized_gamma_p(interface)]] ` (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.

### Example
```fortran
{!example/specialfunctions_gamma/example_gamma_p.f90!}
```

## `regularized_gamma_q` - calculate the gamma quotient Q

### Status

Experimental

### Description

The regularized gamma quotient Q is defined as:

$$Q(p,x)=\Gamma(p,x)/\Gamma(p)=1-P(p,x)$$

The values of regularized gamma Q is in the range of [0, 1]

### Syntax

`result = ` [[stdlib_specialfunctions_gamma(module):regularized_gamma_q(interface)]] ` (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.

### Example
```fortran
{!example/specialfunctions_gamma/example_gamma_q.f90!}
```
