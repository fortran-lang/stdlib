---
title: linalg_iterative_solvers
---

# The `stdlib_linalg_iterative_solvers` module

[TOC]

## Introduction

The `stdlib_linalg_iterative_solvers` module provides base implementations for known iterative solver methods. Each method is exposed with two procedure flavors: 

* A `solve_<method>_kernel` which holds the method's base implementation. The linear system argument is defined through a `linop` derived type which enables extending the method for implicit or unknown (by `stdlib`) matrices or to complex scenarios involving distributed parallelism for which the user shall extend the `inner_product` and/or matrix-vector product to account for parallel syncrhonization.

* A `solve_<method>` which proposes an off-the-shelf ready to use interface for `dense` and `CSR_<kind>_type` matrices for all `real` kinds.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `linop` derived type

The `linop_<kind>_type` derive type is an auxiliary class enabling to abstract the definition of the linear system and the actual implementation of the solvers.

#### Type-bound procedures

The following type-bound procedure pointers enable customization of the solver:

##### `matvec`

Proxy procedure for the matrix-vector product $y = alpha * op(M) * x + beta * y$.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):matvec(interface)]] ` (x,y,alpha,beta,op)`

###### Class

Subroutine

###### Argument(s)

`x`: 1-D array of `real(<kind>)`. This argument is `intent(in)`.

`y`: 1-D array of `real(<kind>)`. This argument is `intent(inout)`.

`alpha`: scalar of `real(<kind>)`. This argument is `intent(in)`.

`beta`: scalar of `real(<kind>)`. This argument is `intent(in)`.

`op`: `character(1)` scalar. This argument is `intent(in)`.

##### `inner_product`

Proxy procedure for the `dot_product`.

#### Syntax

`res = ` [[stdlib_iterative_solvers(module):inner_product(interface)]] ` (x,y)`

###### Class

Function

###### Argument(s)

`x`: 1-D array of `real(<kind>)`. This argument is `intent(in)`.

`y`: 1-D array of `real(<kind>)`. This argument is `intent(in)`.

###### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x` and `y`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `solver_workspace` derived type

The `solver_workspace_<kind>_type` derive type is an auxiliary class enabling to hold the data associated to the working arrays needed by the solvers to operate.

#### Type-bound procedures

- `callback`: null pointer procedure enabling to pass a callback at each iteration to check on the solvers status.

##### Class

Subroutine

##### Argument(s)

`x`: 1-D array of `real(<kind>)` type with the current state of the solution vector. This argument is `intent(in)` as it should not be modified by the callback.

`norm_sq`: scalar of `real(<kind>)` type representing the squared norm of the residual at the current iteration. This argument is `intent(in)`.

`iter`: scalar of `integer` type giving the current iteration counter. This argument is `intent(in)`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `solve_cg_kernel` subroutine

#### Description

Implements the Conjugate Gradient (CG) method for solving the linear system \( Ax = b \), where \( A \) is a symmetric positive-definite linear operator defined via the `linop` type. This is the core implementation, allowing flexibility for custom matrix types or parallel environments.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):solve_cg_kernel(interface)]] ` (A, b, x, tol, maxiter, workspace)`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `class(linop_<kind>_type)` defining the linear operator. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`tol`: scalar of type `real(<kind>)` specifying the requested tolerance with respect to the relative residual vector norm. This argument is `intent(in)`.

`maxiter`: scalar of type `integer` defining the maximum allowed number of iterations. This argument is `intent(in)`.

`workspace`: `type(solver_workspace_<kind>_type)` holding the work temporal array for the solver. This argument is `intent(inout)`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `solve_cg` subroutine

#### Description

Provides a user-friendly interface to the CG method for solving \( Ax = b \), supporting `dense` and `CSR_<kind>_type` matrices. It handles workspace allocation and optional parameters for customization.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):solve_cg(interface)]] ` (A, b, x [, di, tol, maxiter, restart, workspace])`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `dense` or `CSR_<kind>_type` matrix defining the linear system. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`di` (optional): 1-D mask array of type `logical(1)` defining the degrees of freedom subject to dirichlet boundary conditions. The actual boundary conditions values should be stored in the `b` load array. This argument is `intent(in)`.

`tol` (optional): scalar of type `real(<kind>)` specifying the requested tolerance with respect to the relative residual vector norm. If no value is given, a default value of `1.e-4` is set. This argument is `intent(in)`.

`maxiter` (optional): scalar of type `integer` defining the maximum allowed number of iterations. If no value is given, a default of `N` is set, where `N = size(b)`. This argument is `intent(in)`.

`workspace` (optional): scalar derived type of `type(solver_workspace_<kind>_type)` holding the work array for the solver. If the user passes its own `workspace`, then a pointer is set internally to it. Otherwise, memory will be internally allocated and deallocated before exiting the procedure. This argument is `intent(inout)`.

#### Example

```fortran
{!example/linalg/example_solve_cg.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `solve_pcg_kernel` subroutine

#### Description

Implements the Preconditioned Conjugate Gradient (PCG) method for solving the linear system \( Ax = b \), where \( A \) is a symmetric positive-definite linear operator defined via the `linop` type. This is the core implementation, allowing flexibility for custom matrix types or parallel environments.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):solve_cg_kernel(interface)]] ` (A, M, b, x, tol, maxiter, workspace)`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `class(linop_<kind>_type)` defining the linear operator. This argument is `intent(in)`.

`M`: `class(linop_<kind>_type)` defining the preconditioner linear operator. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`tol`: scalar of type `real(<kind>)` specifying the requested tolerance with respect to the relative residual vector norm. This argument is `intent(in)`.

`maxiter`: scalar of type `integer` defining the maximum allowed number of iterations. This argument is `intent(in)`.

`workspace`: scalar derived type  of `type(solver_workspace_<kind>_type)` holding the work array for the solver. This argument is `intent(inout)`.

#### Example

```fortran
{!example/linalg/example_solve_custom.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `solve_pcg` subroutine

#### Description

Provides a user-friendly interface to the PCG method for solving \( Ax = b \), supporting `dense` and `CSR_<kind>_type` matrices. It supports optional preconditioners and handles workspace allocation.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):solve_pcg(interface)]] ` (A, b, x [, di, tol, maxiter, restart, precond, M, workspace])`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `dense` or `CSR_<kind>_type` matrix defining the linear system. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`di` (optional): 1-D mask array of type `logical(int8)` defining the degrees of freedom subject to dirichlet boundary conditions. The actual boundary conditions values should be stored in the `b` load array. This argument is `intent(in)`.

`tol` (optional): scalar of type `real(<kind>)` specifying the requested tolerance with respect to the relative residual vector norm. If no value is given, a default value of `1.e-4` is set. This argument is `intent(in)`.

`maxiter` (optional): scalar of type `integer` defining the maximum allowed number of iterations. If no value is given, a default of `N` is set, where `N = size(b)`. This argument is `intent(in)`.

`precond` (optional): scalar of type `integer` enabling to switch among the default preconditioners available. If no value is given, no preconditionning will be applied. This argument is `intent(in)`.

`M` (optional): scalar derived type of `class(linop_<kind>_type)` defining a custom preconditioner linear operator. If given, `precond` will have no effect, a pointer is set to this custom preconditioner.

`workspace` (optional): `type(solver_workspace_<kind>_type)` holding the work temporal array for the solver. If the user passes its own `workspace`, then internally a pointer is set to it, otherwise, memory will be internally allocated and deallocated before exiting the procedure. This argument is `intent(inout)`.

#### Example

```fortran
{!example/linalg/example_solve_pcg.f90!}
```