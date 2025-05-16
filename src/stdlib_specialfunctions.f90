module stdlib_specialfunctions
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp

    implicit none

    private

    interface legendre
        !! version: experimental
        !! 
        !! Legendre polynomial
        pure elemental module function legendre_fp64(n,x) result(leg)
            integer, intent(in) :: n
            real(dp), intent(in) :: x
            real(dp) :: leg
        end function
    end interface
    public :: legendre 

    interface dlegendre
        !! version: experimental
        !! 
        !! First derivative Legendre polynomial
        pure elemental module function dlegendre_fp64(n,x) result(dleg)
            integer, intent(in) :: n
            real(dp), intent(in) :: x
            real(dp) :: dleg
        end function
    end interface
    public :: dlegendre 

    interface gaussian
        !! Version: experimental
        !!
        !! gaussian function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#gaussian))
        elemental module function gaussian_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function gaussian_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: gaussian

    interface gaussian_grad
        !! Version: experimental
        !!
        !! gradient of the gaussian function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#gaussian_grad))
        elemental module function gaussian_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function gaussian_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: gaussian_grad

    interface elu
        !! Version: experimental
        !!
        !! exponential linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#elu))
        elemental module function elu_sp( x , a ) result( y )
            real(sp), intent(in) :: x
            real(sp), intent(in) :: a
            real(sp) :: y
        end function
        elemental module function elu_dp( x , a ) result( y )
            real(dp), intent(in) :: x
            real(dp), intent(in) :: a
            real(dp) :: y
        end function
    end interface
    public :: elu

    interface elu_grad
        !! Version: experimental
        !!
        !! gradient of the exponential linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#elu_grad))
        elemental module function elu_grad_sp( x , a ) result( y )
            real(sp), intent(in) :: x
            real(sp), intent(in) :: a
            real(sp) :: y
        end function
        elemental module function elu_grad_dp( x , a ) result( y )
            real(dp), intent(in) :: x
            real(dp), intent(in) :: a
            real(dp) :: y
        end function
    end interface
    public :: elu_grad

    interface relu
        !! Version: experimental
        !!
        !! Rectified linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#relu))
        elemental module function relu_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function relu_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: relu

    interface relu_grad
        !! Version: experimental
        !!
        !! Gradient rectified linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#relu_grad))
        elemental module function relu_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function relu_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: relu_grad

    interface leaky_relu
        !! Version: experimental
        !!
        !! leaky Rectified linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#leaky_relu))
        elemental module function leaky_relu_sp( x , a ) result( y )
            real(sp), intent(in) :: x
            real(sp), intent(in) :: a
            real(sp) :: y
        end function
        elemental module function leaky_relu_dp( x , a ) result( y )
            real(dp), intent(in) :: x
            real(dp), intent(in) :: a
            real(dp) :: y
        end function
    end interface
    public :: leaky_relu

    interface leaky_relu_grad
        !! Version: experimental
        !!
        !! Gradient of the leaky Rectified linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#leaky_relu_grad))
        elemental module function leaky_relu_grad_sp( x , a ) result( y )
            real(sp), intent(in) :: x
            real(sp), intent(in) :: a
            real(sp) :: y
        end function
        elemental module function leaky_relu_grad_dp( x , a ) result( y )
            real(dp), intent(in) :: x
            real(dp), intent(in) :: a
            real(dp) :: y
        end function
    end interface
    public :: leaky_relu_grad

    interface gelu
        !! Version: experimental
        !!
        !! Gaussian error linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#gelu))
        elemental module function gelu_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function gelu_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: gelu

    interface gelu_grad
        !! Version: experimental
        !!
        !! Gradient of the gaussian error linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#gelu_grad))
        elemental module function gelu_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function gelu_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: gelu_grad

    interface gelu_approx
        !! Version: experimental
        !!
        !! Approximated gaussian error linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#gelu_approx))
        elemental module function gelu_approx_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function gelu_approx_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: gelu_approx

    interface gelu_approx_grad
        !! Version: experimental
        !!
        !! Gradient of the approximated gaussian error linear unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#gelu_approx_grad))
        elemental module function gelu_approx_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function gelu_approx_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: gelu_approx_grad

    interface selu
        !! Version: experimental
        !!
        !! Scaled Exponential Linear Unit
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#selu))
        elemental module function selu_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function selu_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: selu

    interface selu_grad
        !! Version: experimental
        !!
        !! Scaled Exponential Linear Unit
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#selu_grad))
        elemental module function selu_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function selu_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: selu_grad

    interface sigmoid
        !! Version: experimental
        !!
        !! Sigmoid function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#sigmoid))
        elemental module function sigmoid_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function sigmoid_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: sigmoid

    interface sigmoid_grad
        !! Version: experimental
        !!
        !! Gradient of the sigmoid function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#sigmoid_grad))
        elemental module function sigmoid_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function sigmoid_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: sigmoid_grad
    
    interface silu
        !! Version: experimental
        !!
        !! Sigmoid Linear Unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#silu))
        elemental module function silu_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function silu_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: silu

    interface silu_grad
        !! Version: experimental
        !!
        !! Gradient of the Sigmoid Linear Unit function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#silu_grad))
        elemental module function silu_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function silu_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: silu_grad

    interface step
        !! Version: experimental
        !!
        !! Step function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#step))
        elemental module function step_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function step_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: step

    interface step_grad
        !! Version: experimental
        !!
        !! Gradient of the step function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#step_grad))
        elemental module function step_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function step_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: step_grad

    interface softmax
        !! Version: experimental
        !!
        !! softmax function. Available for ranks 1 to 4
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#softmax))
        pure module function softmax_r1_sp( x ) result( y )
            real(sp), intent(in) :: x(:)
            real(sp) :: y(size(x))
        end function
        pure module function softmax_r2_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:)
            real(sp) :: y(size(x,1), size(x,2))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_r3_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:,:)
            real(sp) :: y(size(x,1), size(x,2), size(x,3))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_r4_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:,:,:)
            real(sp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_r1_dp( x ) result( y )
            real(dp), intent(in) :: x(:)
            real(dp) :: y(size(x))
        end function
        pure module function softmax_r2_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:)
            real(dp) :: y(size(x,1), size(x,2))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_r3_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:,:)
            real(dp) :: y(size(x,1), size(x,2), size(x,3))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_r4_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:,:,:)
            real(dp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))
            integer, intent(in), optional :: dim
        end function
    end interface
    public :: softmax

    interface softmax_grad
        !! Version: experimental
        !!
        !! Gradient of the softmax function. Available for ranks 1 to 4
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#softmax_grad))
        pure module function softmax_grad_r1_sp( x ) result( y )
            real(sp), intent(in) :: x(:)
            real(sp) :: y(size(x))
        end function
        pure module function softmax_grad_r2_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:)
            real(sp) :: y(size(x,1), size(x,2))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_grad_r3_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:,:)
            real(sp) :: y(size(x,1), size(x,2), size(x,3))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_grad_r4_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:,:,:)
            real(sp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_grad_r1_dp( x ) result( y )
            real(dp), intent(in) :: x(:)
            real(dp) :: y(size(x))
        end function
        pure module function softmax_grad_r2_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:)
            real(dp) :: y(size(x,1), size(x,2))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_grad_r3_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:,:)
            real(dp) :: y(size(x,1), size(x,2), size(x,3))
            integer, intent(in), optional :: dim
        end function
        pure module function softmax_grad_r4_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:,:,:)
            real(dp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))
            integer, intent(in), optional :: dim
        end function
    end interface
    public :: softmax_grad

    interface logsoftmax
        !! Version: experimental
        !!
        !! softmax function. Available for ranks 1 to 4
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#logsoftmax))
        pure module function logsoftmax_r1_sp( x ) result( y )
            real(sp), intent(in) :: x(:)
            real(sp) :: y(size(x))
        end function
        pure module function logsoftmax_r2_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:)
            real(sp) :: y(size(x,1), size(x,2))
            integer, intent(in), optional :: dim
        end function
        pure module function logsoftmax_r3_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:,:)
            real(sp) :: y(size(x,1), size(x,2), size(x,3))
            integer, intent(in), optional :: dim
        end function
        pure module function logsoftmax_r4_sp( x , dim ) result( y )
            real(sp), intent(in) :: x(:,:,:,:)
            real(sp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))
            integer, intent(in), optional :: dim
        end function
        pure module function logsoftmax_r1_dp( x ) result( y )
            real(dp), intent(in) :: x(:)
            real(dp) :: y(size(x))
        end function
        pure module function logsoftmax_r2_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:)
            real(dp) :: y(size(x,1), size(x,2))
            integer, intent(in), optional :: dim
        end function
        pure module function logsoftmax_r3_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:,:)
            real(dp) :: y(size(x,1), size(x,2), size(x,3))
            integer, intent(in), optional :: dim
        end function
        pure module function logsoftmax_r4_dp( x , dim ) result( y )
            real(dp), intent(in) :: x(:,:,:,:)
            real(dp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))
            integer, intent(in), optional :: dim
        end function
    end interface
    public :: logsoftmax

    interface softplus
        !! Version: experimental
        !!
        !! softplus function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#softplus))
        elemental module function softplus_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function softplus_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: softplus

    interface softplus_grad
        !! Version: experimental
        !!
        !! Gradient of the softplus function
        !> ([Specification](../page/specs/stdlib_specialfunctions.html#softplus_grad))
        elemental module function softplus_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function softplus_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: softplus_grad

    interface fast_tanh 
        !! Version: experimental
        !!
        !! Fast approximation of the tanh function
        elemental module function fast_tanh_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function fast_tanh_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: fast_tanh

    interface fast_tanh_grad
        !! Version: experimental
        !!
        !! gradient of the hyperbolic tangent function
        elemental module function fast_tanh_grad_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function fast_tanh_grad_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: fast_tanh_grad

    interface fast_erf 
        !! Version: experimental
        !!
        !! Fast approximation of the erf function
        elemental module function fast_erf_sp( x ) result( y )
            real(sp), intent(in) :: x
            real(sp) :: y
        end function
        elemental module function fast_erf_dp( x ) result( y )
            real(dp), intent(in) :: x
            real(dp) :: y
        end function
    end interface
    public :: fast_erf

end module stdlib_specialfunctions