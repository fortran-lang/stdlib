---
title: specialfunctions
---

# Special functions - Neural Networks activations and their gradients

[TOC]

## `Gaussian` - Gaussian function

### Status

Experimental

### Description

Computes the gaussian function:
$$f(x)=\exp(-x^2)$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):gaussian(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Gaussian_grad` - Gradient of the Gaussian function

### Status

Experimental

### Description

Computes the gradient of the gaussian function:
$$f(x)=-2 * x * \exp( - x ^ 2)$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):gaussian_grad(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Elu` - Exponential Linear Unit function

### Status

Experimental

### Description

Computes the gaussian function:
$$
\text{f}(x) =
\begin{cases} 
x, & \text{if } x \geq 0 \\
a * (\exp(x) - 1), & \text{otherwise}
\end{cases}
$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):elu(interface)]] ` (x,a)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 
`a`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Elu_grad` - Gradient of the Exponential Linear Unit function

### Status

Experimental

### Description

Computes the gradient of the gaussian function:
$$
\text{f}(x) =
\begin{cases} 
1, & \text{if } x \geq 0 \\
a * \exp(x), & \text{otherwise}
\end{cases}
$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):elu_grad(interface)]] ` (x,a)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.
`a`: Shall be a scalar or array of any `real` kind.  

### Return value

The function returns a value with the same type and kind as input argument.

## `Relu` - Rectified Linear Unit function

### Status

Experimental

### Description

Computes the Rectified Linear Unit function:
$$f(x) = \text{max}(0,x)$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):relu(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Relu_grad` - Gradient of the Rectified Linear Unit function

### Status

Experimental

### Description

Computes the gradient of the gaussian function:
$$
f(x) =
\begin{cases} 
1, & \text{if } x \geq 0 \\
0, & \text{otherwise}
\end{cases}
$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):relu_grad(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `Gelu` - Gaussian Error Linear Unit function

### Status

Experimental

### Description

Computes the Gaussian Error Linear Unit function:
$$f(x) = \frac{1}{2} x ( 1 + \text{erf}(\frac{x}{\sqrt{2}}) ) $$

### Syntax

`result = ` [[stdlib_specialfunctions(module):gelu(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Gelu_grad` - Gradient of the Gaussian Error Linear Unit function

### Status

Experimental

### Description

Computes the gradient of the gaussian error linear unit function:
$$
f(x) = \frac{1}{2} ( 1 + \text{erf}(x \sqrt{2}) ) + x \sqrt{2} \exp( -\frac{1}{2} x^2)
$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):gelu_grad(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `Gelu_approx` - Approximation of the Gaussian Error Linear Unit function

### Status

Experimental

### Description

Computes a fast approximation of the Gaussian Error Linear Unit function using a fast $\text{erf}$ approximation:
$$f(x) = \frac{1}{2} x ( 1 + \text{ferf}(\frac{x}{\sqrt{2}}) ) $$

### Syntax

`result = ` [[stdlib_specialfunctions(module):gelu_approx(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Gelu_approx_grad` - Gradient of the Approximated Gaussian Error Linear Unit function

### Status

Experimental

### Description

Computes the gradient of the gaussian error linear unit function using a fast $\text{erf}$ approximation:
$$
f(x) = \frac{1}{2} ( 1 + \text{ferf}(x \sqrt{2}) ) + x \sqrt{2} \exp( -\frac{1}{2} x^2)
$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):gelu_approx_grad(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `Sigmoid` - Sigmoid function

### Status

Experimental

### Description

Computes the sigmoid function:
$$f(x) = \frac{1}{1+\exp(-x)} $$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Sigmoid(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Sigmoid_grad` - Gradient of the Sigmoid function

### Status

Experimental

### Description

Computes the gradient of the Sigmoid function:
$$f(x) = \frac{\exp(x)}{(1+\exp(x))^2} $$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Sigmoid_grad(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `Step` - Step function

### Status

Experimental

### Description

Computes the step function:
$$
f(x) =
\begin{cases} 
1, & \text{if } x > 0 \\
0, & \text{otherwise}
\end{cases}
$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Step(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Step_grad` - Gradient of the Step function

### Status

Experimental

### Description

Computes the gradient of the Sigmoid function:
$$f(x) = 0 $$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Step_grad(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `Softmax` - Softmax function

### Status

Experimental

### Description

Computes the Softmax function:
$$f(x) = \frac{\exp(x)-\text{max}(x_j)}{\sum_j{\exp(x)-\text{max}(x_j)}}$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Softmax(interface)]] ` (x,dim)`

### Class

Pure function for ranks 1 to 4.

### Arguments

`x`: Shall be an array of rank 1 to 4 of any `real` kind. 
`dim`: integer scalar indicating upon which dimension to apply the normalization.

### Return value

The function returns an array with the same rank and kind as the input argument `x`.

## `Softplus_grad` - Gradient of the Softplus function

### Status

Experimental

### Description

Computes the gradient of the Softmax function:
$$f(x,dim) = \text{Softmax}(x,dim)*(1-\text{Softmax}(x,dim)) $$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Softmax_grad(interface)]] ` (x,dim)`

### Class

Pure function for ranks 1 to 4.

### Arguments

`x`: Shall be an array of rank 1 to 4 of any `real` kind. 
`dim`: integer scalar indicating upon which dimension to apply the normalization.

### Return value

The function returns a value with the same type and kind as input argument.

## `Softplus` - Softplus function

### Status

Experimental

### Description

Computes the Softplus function:
$$f(x) = \log(\exp(x)+1)$$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Softplus(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Softplus_grad` - Gradient of the Softplus function

### Status

Experimental

### Description

Computes the gradient of the Softplus function:
$$f(x) = \frac{\exp(x)}{\exp(x)+1} $$

### Syntax

`result = ` [[stdlib_specialfunctions(module):Softplus_grad(interface)]] ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.