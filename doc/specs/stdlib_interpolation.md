# `stdlib_interpolation`

```{contents}
:local:
:depth: 2
```

## Introduction

The `stdlib_interpolation` module provides derived types and drivers for 1D data interpolation across all supported `real` kinds (`sp`, `dp`, `xdp`, `qp`).
Currently supported methods include:

- Piecewise linear interpolation (`linear_interp1d_<kind>_type`, `interp1d`)

An abstract base type `interp1d_<kind>_type` defines the common 1D interpolator interface so higher-order schemes (such as cubic splines and PCHIP) can build on the same evaluation and interval-search infrastructure.

## Extrapolation flags

Out-of-bounds behavior outside `[x(1), x(n)]` is controlled via the following `enum, bind(c)` constants:

- `interp_extrap_linear`: Extrapolate linearly using the boundary segment slope (default).
- `interp_extrap_clamp`: Clamp out-of-bounds points to `y(1)` (for `xq < x(1)`) or `y(n)` (for `xq > x(n)`).
- `interp_extrap_nan`: Return quiet IEEE `NaN` for `xq < x(1)` or `xq > x(n)`.

## Derived types for 1D interpolation

### Piecewise linear interpolator {#linear_interp1d}

#### Status

Experimental

#### Description

Given strictly increasing abscissas $x_1 < x_2 < \dots < x_n$ and values $y_1, \dots, y_n$ with $n \ge 2$, `linear_interp1d_<kind>_type` precomputes segment slopes

$$
m_i = \frac{y_{i+1} - y_i}{x_{i+1} - x_i}, \quad i = 1, \dots, n - 1.
$$

For a query coordinate $x_q \in [x_i, x_{i+1}]$, the target interval is located in $O(\log n)$ time via binary search and evaluated as $y_q = y_i + m_i (x_q - x_i)$.

Available derived types:

- `linear_interp1d_sp_type`: Single precision (`sp`), extending `interp1d_sp_type`.
- `linear_interp1d_dp_type`: Double precision (`dp`), extending `interp1d_dp_type`.
- `linear_interp1d_xdp_type`: Extended double precision (`xdp`), extending `interp1d_xdp_type`.
- `linear_interp1d_qp_type`: Quadruple precision (`qp`), extending `interp1d_qp_type`.

#### Syntax

- Construct a linear interpolator:

`interp = ` `linear_interp1d` `(x, y [, extrap])`

`interp = ` `linear_interp1d` `(x, y, err [, extrap])`

- Or initialize in place via type-bound subroutine:

`call interp%initialize(x, y [, extrap, err])`

- Evaluate and clean up:

`yq = interp%evaluate(xq)`

`n = interp%num_points()`

`ok = interp%is_initialized()`

`call interp%destroy()`

When `err` is not present, `linear_interp1d` is `pure` and halts via `error stop` on invalid inputs. When `err` is present, invalid inputs set `err` to `STDLIB_VALUE_ERROR` without terminating execution.

#### Arguments

- `x`: Shall be a rank-1 array of type `real` with `size(x) >= 2` and strictly increasing values (`x(i+1) > x(i)`). It is an `intent(in)` argument.
- `y`: Shall be a rank-1 array of type `real` with the same kind and size as `x`. It is an `intent(in)` argument.
- `extrap` (optional): Shall be a default `integer` scalar equal to `interp_extrap_linear`, `interp_extrap_clamp`, or `interp_extrap_nan`. It is an `intent(in)` argument.
- `err` (optional in `initialize`): Shall be a `type(state_type)` variable (`stdlib_error`). It is an `intent(out)` argument.
- `xq`: Shall be a scalar or rank-1 array of type `real` with the same kind as `x`. It is an `intent(in)` argument.

#### Return value

`evaluate(xq)` returns a scalar or rank-1 `real` array of the same kind and shape as `xq`.

## `interp1d` - one-shot 1D linear interpolation

### Status

Experimental

### Description

Evaluates 1D piecewise linear interpolation directly at scalar or rank-1 query points `xq` without explicitly managing an interpolator object.

### Syntax

`yq = ` `interp1d` `(xq, x, y [, extrap])`

`yq = ` `interp1d` `(xq, x, y, err [, extrap])`

### Arguments

- `xq`: Shall be a scalar or rank-1 array of type `real`. It is an `intent(in)` argument.
- `x`: Shall be a rank-1 array of type `real` having the same kind as `xq`, with `size(x) >= 2` and strictly increasing elements. It is an `intent(in)` argument.
- `y`: Shall be a rank-1 array of type `real` having the same kind and size as `x`. It is an `intent(in)` argument.
- `extrap` (optional): Shall be a default `integer` scalar specifying the extrapolation mode. It is an `intent(in)` argument.
- `err` (optional overload): Shall be a `type(state_type)` variable. It is an `intent(out)` argument.

### Return value

The result is a scalar or rank-1 `real` array having the same kind and shape as `xq`. If `err` is provided and input validation fails, `err` is assigned `STDLIB_VALUE_ERROR` and `yq` is set to quiet `NaN`.

### Example

```{literalinclude} ../../example/interpolation/example_linear_interp1d.f90
:language: fortran
```
