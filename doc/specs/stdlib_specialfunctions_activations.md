# Special functions - Neural Networks activations and their gradients

```{contents}
:local:
:depth: 2
```

## `Gaussian` - Gaussian function

### Status

Experimental

### Description

Computes the gaussian function:
$$f(x)=\exp(-x^2)$$

### Syntax

`result = ` `gaussian` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_gaussian.f90
:language: fortran
```

## `Gaussian_grad` - Gradient of the Gaussian function

### Status

Experimental

### Description

Computes the gradient of the gaussian function:
$$f(x)=-2 * x * \exp( - x ^ 2)$$

### Syntax

`result = ` `gaussian_grad` ` (x)`

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

`result = ` `elu` ` (x,a)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 
`a`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_elu.f90
:language: fortran
```

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

`result = ` `elu_grad` ` (x,a)`

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

`result = ` `relu` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_relu.f90
:language: fortran
```

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

`result = ` `relu_grad` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `leaky_relu` - leaky Rectified Linear Unit function

### Status

Experimental

### Description

Computes the gaussian function:
$$
\text{f}(x) =
\begin{cases} 
x, & \text{if } x \geq 0 \\
a * x, & \text{otherwise}
\end{cases}
$$

### Syntax

`result = ` `leaky_relu` ` (x,a)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 
`a`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_leaky_relu.f90
:language: fortran
```

## `leaky_relu_grad` - Gradient of the leaky Rectified Linear Unit function

### Status

Experimental

### Description

Computes the gradient of the leaky_relu function:
$$
\text{f}(x) =
\begin{cases} 
1, & \text{if } x \geq 0 \\
a , & \text{otherwise}
\end{cases}
$$

### Syntax

`result = ` `leaky_relu_grad` ` (x,a)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.
`a`: Shall be a scalar or array of any `real` kind.  

### Return value

The function returns a value with the same type and kind as the input argument.

## `Gelu` - Gaussian Error Linear Unit function

### Status

Experimental

### Description

Computes the Gaussian Error Linear Unit function:
$$f(x) = \frac{1}{2} x ( 1 + \text{erf}(\frac{x}{\sqrt{2}}) ) $$

### Syntax

`result = ` `gelu` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_gelu.f90
:language: fortran
```

## `Gelu_grad` - Gradient of the Gaussian Error Linear Unit function

### Status

Experimental

### Description

Computes the gradient of the gaussian error linear unit function:
$$
f(x) = \frac{1}{2} ( 1 + \text{erf}(x \sqrt{2}) ) + x \sqrt{2} \exp( -\frac{1}{2} x^2)
$$

### Syntax

`result = ` `gelu_grad` ` (x)`

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

`result = ` `gelu_approx` ` (x)`

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

`result = ` `gelu_approx_grad` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `Selu` - Scaled Exponential Linear Unit function

### Status

Experimental

### Description

Applies the Scaled Exponential Linear Unit activation function:
$$
f(x) =
\begin{cases} 
scale * x, & \text{if } x \ge 0 \\
scale * (\alpha * exp(x) - \alpha ), & \text{otherwise}
\end{cases}
$$
Where,
$$scale = 1.0507009873554804934193349852946$$ and $$\alpha = 1.6732632423543772848170429916717$$

### Syntax

`result = ` `selu` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_selu.f90
:language: fortran
```

## `selu_grad` - Gradient of the Scaled Exponential Linear Unit function

### Status

Experimental

### Description

Applies the gradient of the Scaled Exponential Linear Unit activation function:
$$
f(x) =
\begin{cases} 
scale, & \text{if } x \ge 0 \\
scale * \alpha * exp(x) , & \text{otherwise}
\end{cases}
$$

### Syntax

`result = ` `selu_grad` ` (x)`

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

`result = ` `sigmoid` ` (x)`

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

`result = ` `sigmoid_grad` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `SiLU` - Sigmoid Linear Unit function

### Status

Experimental

### Description

Computes the Sigmoid Linear Unit function:
$$f(x) = \frac{x}{1+\exp(-x)} $$

### Syntax

`result = ` `silu` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_silu.f90
:language: fortran
```

## `Silu_grad` - Gradient of the Sigmoid Linear Unit function

### Status

Experimental

### Description

Computes the gradient of the Sigmoid function:
$$f(x) = \frac{\exp(x)*(x+(1+\exp(x))^2)}{(1+\exp(x))^2} $$

### Syntax

`result = ` `silu_grad` ` (x)`

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

`result = ` `step` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_step.f90
:language: fortran
```

## `step_grad` - Gradient of the Step function

### Status

Experimental

### Description

Computes the gradient of the Sigmoid function:
$$f(x) = 0 $$

### Syntax

`result = ` `step_grad` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `softmax` - softmax function

### Status

Experimental

### Description

Computes the softmax function:
$$f(x) = \frac{\exp(x)-\text{max}(x_j)}{\sum_j{\exp(x)-\text{max}(x_j)}}$$

### Syntax

`result = ` `softmax` ` (x,dim)`

### Class

Pure function for ranks 1 to 4.

### Arguments

`x`: Shall be an array of rank 1 to 4 of any `real` kind. 
`dim`: integer scalar indicating upon which dimension to apply the normalization.

### Return value

The function returns an array with the same rank and kind as the input argument `x`.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_softmax.f90
:language: fortran
```

## `softmax_grad` - Gradient of the softmax function

### Status

Experimental

### Description

Computes the gradient of the softmax function:
$$f(x,dim) = \text{softmax}(x,dim)*(1-\text{softmax}(x,dim)) $$

### Syntax

`result = ` `softmax_grad` ` (x,dim)`

### Class

Pure function for ranks 1 to 4.

### Arguments

`x`: Shall be an array of rank 1 to 4 of any `real` kind. 
`dim`: integer scalar indicating upon which dimension to apply the normalization.

### Return value

The function returns a value with the same type and kind as input argument.

## `softplus` - softplus function

### Status

Experimental

### Description

Computes the softplus function:
$$f(x) = \log(\exp(x)+1)$$

### Syntax

`result = ` `softplus` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

### Example
```{literalinclude} ../../example/specialfunctions_activations/example_softplus.f90
:language: fortran
```

## `softplus_grad` - Gradient of the softplus function

### Status

Experimental

### Description

Computes the gradient of the softplus function:
$$f(x) = \frac{\exp(x)}{\exp(x)+1} $$

### Syntax

`result = ` `softplus_grad` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind.

### Return value

The function returns a value with the same type and kind as input argument.

## `Fast tanh` - Approximation of the hyperbolic tangent function

### Status

Experimental

### Description

Computes an approximated but faster solution to:
$$f(x)=\tanh(x)$$

### Syntax

`result = ` `fast_tanh` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `fast_tanh_grad` - Gradient of the approximation of the hyperbolic tangent function

### Status

Experimental

### Description

Computes the gradient of the `fast_tanh` function:
$$f(x)=1 - \fast_tanh(x)^2$$

### Syntax

`result = ` `fast_tanh_grad` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.

## `Fast erf` - Approximation of the error function

### Status

Experimental

### Description

Computes an approximated but faster solution to:
$$f(x)=\erf(x)$$

### Syntax

`result = ` `fast_erf` ` (x)`

### Class

Elemental function

### Arguments

`x`: Shall be a scalar or array of any `real` kind. 

### Return value

The function returns a value with the same type and kind as input argument.