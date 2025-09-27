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

The `stdlib_linop_<kind>_type` derive type is an auxiliary class enabling to abstract the definition of the linear system and the actual implementation of the solvers.

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

`op`: `character(1)` scalar which can be have any of the following values: `N` (no transpose), `T` (transpose) or `H` (conjugate transpose). This argument is `intent(in)`.

##### `inner_product`

Proxy procedure for the `dot_product`.

#### Syntax

`res = ` [[stdlib_iterative_solvers(module):inner_product(interface)]] ` (x,y)`

###### Class

Pure function

###### Argument(s)

`x`: 1-D array of `real(<kind>)`. This argument is `intent(in)`.

`y`: 1-D array of `real(<kind>)`. This argument is `intent(in)`.

###### Output value or Result value

The output is a scalar of `type` and `kind` same as to that of `x` and `y`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### The `solver_workspace` derived type

The `stdlib_solver_workspace_<kind>_type` derive type is an auxiliary class enabling to hold the data associated to the working arrays needed by the solvers to operate.

#### Type-bound procedures

- `callback`: null pointer procedure enabling to pass a callback at each iteration to check on the solvers status.

##### Class

Subroutine

##### Argument(s)

`x`: 1-D array of `real(<kind>)` type with the current state of the solution vector. This argument is `intent(in)` as it should not be modified by the callback.

`norm_sq`: scalar of `real(<kind>)` type representing the squared norm of the residual at the current iteration. This argument is `intent(in)`.

`iter`: scalar of `integer` type giving the current iteration counter. This argument is `intent(in)`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_solve_cg_kernel` subroutine

#### Description

Implements the Conjugate Gradient (CG) method for solving the linear system \( Ax = b \), where \( A \) is a symmetric positive-definite linear operator defined via the `stdlib_linop` type. This is the core implementation, allowing flexibility for custom matrix types or parallel environments.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):stdlib_solve_cg_kernel(interface)]] ` (A, b, x, tol, maxiter, workspace)`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `class(stdlib_linop_<kind>_type)` defining the linear operator. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`rtol` and `atol`: scalars of type `real(<kind>)` specifying the convergence test. For convergence, the following criterion is used \( || b - Ax ||^2 <= max(rtol^2 * || b ||^2 , atol^2 ) \). These arguments are `intent(in)`.

`maxiter`: scalar of type `integer` defining the maximum allowed number of iterations. This argument is `intent(in)`.

`workspace`: scalar derived type of `type(stdlib_solver_workspace_<kind>_type)` holding the work array for the solver. This argument is `intent(inout)`.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `solve_cg` subroutine

#### Description

Provides a user-friendly interface to the CG method for solving \( Ax = b \), supporting `dense` and `CSR_<kind>_type` matrices. It handles workspace allocation and optional parameters for customization.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):solve_cg(interface)]] ` (A, b, x [, di, rtol, atol, maxiter, restart, workspace])`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `dense` or `CSR_<kind>_type` matrix defining the linear system. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the right-hand-side (or loading) of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and as the output solution. This argument is `intent(inout)`.

`di` (optional): 1-D mask array of type `logical(int8)` defining the degrees of freedom subject to dirichlet boundary conditions. The actual boundary conditions values should be stored in the `b` load array. This argument is `intent(in)`.

`rtol` and `atol` (optional): scalars of type `real(<kind>)` specifying the convergence test. For convergence, the following criterion is used \( || b - Ax ||^2 <= max(rtol^2 * || b ||^2 , atol^2 ) \). Defaults values are `rtol=1.e-5` and `atol=epsilon(1._<kind>)`. These arguments are `intent(in)`.

`maxiter` (optional): scalar of type `integer` defining the maximum allowed number of iterations. If no value is given, a default of `N` is set, where `N = size(b)`. This argument is `intent(in)`.

`workspace` (optional): scalar derived type of `type(stdlib_solver_workspace_<kind>_type)` holding the work array for the solver. If the user passes its own `workspace`, then a pointer is set internally to it. Otherwise, memory will be internally allocated and deallocated before exiting the procedure. This argument is `intent(inout)`.

#### Example

```fortran
{!example/linalg/example_solve_cg.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_solve_pcg_kernel` subroutine

#### Description

Implements the Preconditioned Conjugate Gradient (PCG) method for solving the linear system \( Ax = b \), where \( A \) is a symmetric positive-definite linear operator defined via the `stdlib_linop` type. This is the core implementation, allowing flexibility for custom matrix types or parallel environments.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):stdlib_solve_cg_kernel(interface)]] ` (A, M, b, x, tol, maxiter, workspace)`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `class(stdlib_linop_<kind>_type)` defining the linear operator. This argument is `intent(in)`.

`M`: `class(stdlib_linop_<kind>_type)` defining the preconditioner linear operator. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`rtol` and `atol` (optional): scalars of type `real(<kind>)` specifying the convergence test. For convergence, the following criterion is used \( || b - Ax ||^2 <= max(rtol^2 * || b ||^2 , atol^2 ) \). These arguments are `intent(in)`.

`maxiter`: scalar of type `integer` defining the maximum allowed number of iterations. This argument is `intent(in)`.

`workspace`: scalar derived type  of `type(stdlib_solver_workspace_<kind>_type)` holding the work array for the solver. This argument is `intent(inout)`.

#### Example

```fortran
{!example/linalg/example_solve_custom.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_solve_pcg` subroutine

#### Description

Provides a user-friendly interface to the PCG method for solving \( Ax = b \), supporting `dense` and `CSR_<kind>_type` matrices. It supports optional preconditioners and handles workspace allocation.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):stdlib_solve_pcg(interface)]] ` (A, b, x [, di, tol, maxiter, restart, precond, M, workspace])`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `dense` or `CSR_<kind>_type` matrix defining the linear system. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`di` (optional): 1-D mask array of type `logical(int8)` defining the degrees of freedom subject to dirichlet boundary conditions. The actual boundary conditions values should be stored in the `b` load array. This argument is `intent(in)`.

`rtol` and `atol` (optional): scalars of type `real(<kind>)` specifying the convergence test. For convergence, the following criterion is used \( || b - Ax ||^2 <= max(rtol^2 * || b ||^2 , atol^2 ) \). Defaults values are `rtol=1.e-5` and `atol=epsilon(1._<kind>)`. These arguments are `intent(in)`.

`maxiter` (optional): scalar of type `integer` defining the maximum allowed number of iterations. If no value is given, a default of `N` is set, where `N = size(b)`. This argument is `intent(in)`.

`precond` (optional): scalar of type `integer` enabling to switch among the default preconditioners available with the following enum (`pc_none`, `pc_jacobi`). If no value is given, no preconditionning will be applied. This argument is `intent(in)`.

`M` (optional): scalar derived type of `class(stdlib_linop_<kind>_type)` defining a custom preconditioner linear operator. If given, `precond` will have no effect, a pointer is set to this custom preconditioner.

`workspace` (optional): scalar derived type of `type(stdlib_solver_workspace_<kind>_type)` holding the work temporal array for the solver. If the user passes its own `workspace`, then internally a pointer is set to it, otherwise, memory will be internally allocated and deallocated before exiting the procedure. This argument is `intent(inout)`.

#### Example

```fortran
{!example/linalg/example_solve_pcg.f90!}
```

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_solve_bicgstab_kernel` subroutine

#### Description

Implements the Biconjugate Gradient Stabilized (BiCGSTAB) method for solving the linear system \( Ax = b \), where \( A \) is a general (non-symmetric) linear operator defined via the `stdlib_linop` type. BiCGSTAB is particularly suitable for solving non-symmetric linear systems and provides better stability than the basic BiCG method. This is the core implementation, allowing flexibility for custom matrix types or parallel environments.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):stdlib_solve_bicgstab_kernel(interface)]] ` (A, M, b, x, rtol, atol, maxiter, workspace)`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `class(stdlib_linop_<kind>_type)` defining the linear operator. This argument is `intent(in)`.

`M`: `class(stdlib_linop_<kind>_type)` defining the preconditioner linear operator. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`rtol` and `atol`: scalars of type `real(<kind>)` specifying the convergence test. For convergence, the following criterion is used \( || b - Ax ||^2 <= max(rtol^2 * || b ||^2 , atol^2 ) \). These arguments are `intent(in)`.

`maxiter`: scalar of type `integer` defining the maximum allowed number of iterations. This argument is `intent(in)`.

`workspace`: scalar derived type of `type(stdlib_solver_workspace_<kind>_type)` holding the work array for the solver. This argument is `intent(inout)`.

#### Note

The BiCGSTAB method requires 8 auxiliary vectors in its workspace, making it more memory-intensive than CG or PCG methods. However, it can handle general non-symmetric matrices and often converges faster than BiCG for many problems.

<!-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -->
### `stdlib_solve_bicgstab` subroutine

#### Description

Provides a user-friendly interface to the BiCGSTAB method for solving \( Ax = b \), supporting `dense` and `CSR_<kind>_type` matrices. BiCGSTAB is suitable for general (non-symmetric) linear systems and supports optional preconditioners for improved convergence. It handles workspace allocation and optional parameters for customization.

#### Syntax

`call ` [[stdlib_iterative_solvers(module):stdlib_solve_bicgstab(interface)]] ` (A, b, x [, di, rtol, atol, maxiter, restart, precond, M, workspace])`

#### Status

Experimental

#### Class

Subroutine

#### Argument(s)

`A`: `dense` or `CSR_<kind>_type` matrix defining the linear system. This argument is `intent(in)`.

`b`: 1-D array of `real(<kind>)` defining the loading conditions of the linear system. This argument is `intent(in)`.

`x`: 1-D array of `real(<kind>)` which serves as the input initial guess and the output solution. This argument is `intent(inout)`.

`di` (optional): 1-D mask array of type `logical(int8)` defining the degrees of freedom subject to dirichlet boundary conditions. The actual boundary conditions values should be stored in the `b` load array. This argument is `intent(in)`.

`rtol` and `atol` (optional): scalars of type `real(<kind>)` specifying the convergence test. For convergence, the following criterion is used \( || b - Ax ||^2 <= max(rtol^2 * || b ||^2 , atol^2 ) \). Default values are `rtol=1.e-5` and `atol=epsilon(1._<kind>)`. These arguments are `intent(in)`.

`maxiter` (optional): scalar of type `integer` defining the maximum allowed number of iterations. If no value is given, a default of `N` is set, where `N = size(b)`. This argument is `intent(in)`.

`restart` (optional): scalar of type `logical` indicating whether to restart the iteration with zero initial guess. Default is `.true.`. This argument is `intent(in)`.

`precond` (optional): scalar of type `integer` enabling to switch among the default preconditioners available with the following enum (`pc_none`, `pc_jacobi`). If no value is given, no preconditioning will be applied. This argument is `intent(in)`.

`M` (optional): scalar derived type of `class(stdlib_linop_<kind>_type)` defining a custom preconditioner linear operator. If given, `precond` will have no effect, and a pointer is set to this custom preconditioner. This argument is `intent(in)`.

`workspace` (optional): scalar derived type of `type(stdlib_solver_workspace_<kind>_type)` holding the work temporal array for the solver. If the user passes its own `workspace`, then internally a pointer is set to it, otherwise, memory will be internally allocated and deallocated before exiting the procedure. This argument is `intent(inout)`.

#### Note

BiCGSTAB is particularly effective for:
- Non-symmetric linear systems
- Systems where CG cannot be applied
- Cases where BiCG suffers from irregular convergence

The method uses 8 auxiliary vectors internally, requiring more memory than simpler methods but often providing better stability and convergence properties.

#### Example

```fortran
{!example/linalg/example_solve_bicgstab.f90!}
```