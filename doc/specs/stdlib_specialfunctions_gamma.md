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

\Gamma(z)=\int_{0}^{\infty}x^{z-1}e^{-x}dx, \;\;  z\in \mathbb{C} \setminus 0, -1, -2, \cdots

Fortran 2018 standard implements the intrinsic gamma function of real type argument in single and double precisions. Here the gamma function is extended to both integer and complex arguments. The values of the gamma function with integer arguments are exact. The values of the gamma function with complex arguments are approximated in single and double precisions by using Lanczos approximation.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):gamma(interface)]] (x)`

### Class

Elemental function

### Arguments

`x`: should be a positive integer or a complex type number

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```fortran
program demo_gamma
    use iso_fortran_env, only : real64, int64
    use stdlib_specialfunctions_gamma, only : gamma
    implicit none
    
    integer :: i
    integer(int64) :: n
    real :: x
    real(real64) :: y
    complex :: z
    complex(real64) :: z1
    
    i = 10
    n = 15_int64
    x = 2.5
    y = 4.3_real64
    z = (2.3, 0.6)
    z1 = (-4.2_real64, 3.1_real64)
    
    print *, gamma(i)              !integer gives exact result
! 362880

    print *, gamma(n)
! 87178291200

    print *, gamma(x)              ! intrinsic function call
! 1.32934034

    print *, gamma(y)              ! intrinsic function call
! 8.8553433604540341

    print *, gamma(z)
! (0.988054395, 0.383354813)

    print *, gamma(z1)
! (-2.78916032990983999E-005, 9.83164600163221218E-006)
end program demo_gamma
```

## `log_gamma` - Calculate the natural logarithm of the gamma function

### Status

Experimental

### Description

Due to the different branch cut structures and a different principal branch, natural logarithm of gamma function log_gamma(z) with complex argument is different from the ln(Gamma(z)). The two have the same real part but different imaginary part. 

Fortran 2018 standard implements intrinsic log_gamma function of absolute value of real type argument in single and double precision. Here the log_gamma function is extended to both integer and complex arguments. The values of log_gamma function with complex arguments are approximated in single and double precisions by using Stirling's approximation.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):log_gamma(interface)]] (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a positive integer or a complex type number. 

### Return value

The function returns real single precision values for integer input arguments, while it returns complex values with the same kind as complex input arguments.

### Example

```fortran
program demo_log_gamma
    use iso_fortran_env, only : real64
    use stdlib_specialfunctions_gamma, only : log_gamma
    implicit none
    
    integer :: i
    real :: x
    real(real64) :: y
    complex :: z
    complex(real64) :: z1
    
    i = 10
    x = 8.76
    y = x
    z = (5.345, -3.467)
    z1 = z
    print *, log_gamma(i)     !default single precision output
!12.8018274

    print *, log_gamma(x)     !intrinsic function call
    
!10.0942659

    print *, log_gamma(y)     !intrinsic function call
    
!10.094265528673880

    print *, log_gamma(z)     !same kind as input
    
!(2.56165648, -5.73382425)

    print *, log_gamma(z1)
    
!(2.5616575105114614, -5.7338247782852498)
end program demo_log_gamma
```

## `log_factorial` - calculate the logarithm of a factorial

### Status

Experimental

### Description

Compute the natural logarithm of factorial, log(n!)

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):log_factorial(interface)]] (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a positive integer type number. 

### Return value

The function returns real type values with single precision.

### Example
```fortran
program demo_log_factorial
    use iso_fortran_env, only : int64
    use stdlib_specialfunctions_gamma, only : lf => log_factorial
    implicit none
    integer :: n
    
    n = 10
    print *, lf(n)

! 15.1044130

    print *, lf(35_int64)
    
! 92.1361771
end program demo_log_factorial
```

## `lower_incomplete_gamma` - calculate lower incomplete gamma integral

### Status

Experimental

### Description

The lower incomplete gamma function is defined as:

\gamma(p,x)=\int_{0}^{x}t^{p-1}e^{-t}dt, \;\;  p > 0, x\in \mathbb{R}

When x < 0, p must be positive integer.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):lower_incomplete_gamma(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.

### Example
```fortran
program demo_ligamma
    use stdlib_specialfunctions_gamma, only : lig => lower_incomplete_gamma
    implicit none
    integer :: p
    real :: p1, x
    
    p = 3
    p1 = 2.3
    print *, lig(p, -5.0)
    
! -2521.02417

    print *, lig(p1, 5.0)
    
! 1.09715652
end demo_ligamma
```

## `upper_incomplete_gamma` - calculate the upper incomplete gamma integral

### Status

Experimental

### Description

The upper incomplete gamma function is defined as:

\Gamma (p, x) = \int_{x}^{\infty }t^{p-1}e^{-t}dt, \; \; p >0,\; x \in \mathbb{R}

When x < 0, p must be a positive integer.

### Syntax

`result = [[stdlib_specialfunctions_gamma(module):upper_incomplete_gamma(interface)]] (p, x)`

### Class

Elemental function

### Arguments

`p`: is a positive integer or real type argument.

`x`: is a real type argument. 

### Return value

The function returns a real type value with the same kind as argument x.

### Example
```fortran
program demo_uigamma
    use stdlib_specialfunctions_gamma, only : uig => upper_incomplete_gamma
    implicit none
    
    print *, uig(3, -5.0)

!2523.02295

    print *, uig(2.3, 5.0)
    
!6.95552528E-02
end program demo_uigamma
```

## `log_lower_incomplete_gamma` - calculate the natural logarithm of the lower incomplete gamma integral

### Status

Experimental

### Description

Compute the natural logarithm of the absolute value of the lower incomplete gamma function.

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

Compute the natural logarithm of the absolute value of the upper incomplete gamma function.

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

The regularized gamma quotient P, also known as normalized incomplete gamma function, is defined as:

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

### Example
```fortran
program demo_gamma_p
    use stdlib_specialfunctions_gamma, only : rgp => regularized_gamma_p
    implicit none
    
    print *, rgp(3.0, 5.0)

! 0.875347972
end program demo_gamma_p
```

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

### Example
```fortran
program demo_gamma_q
    use stdlib_specialfunctions_gamma, only : rgq => regularized_gamma_q
    implicit none
    
    print *, rgq(3.0, 5.0)
    
! 0.124652028
end program demo_gamma_q
```
