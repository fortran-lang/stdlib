submodule(stdlib_specialfunctions) stdlib_specialfunctions_activations
    use stdlib_intrinsics, only: sum => stdlib_sum
    implicit none
    
    real(sp), parameter :: isqrt2_sp = 1._sp / sqrt(2._sp)
    real(dp), parameter :: isqrt2_dp = 1._dp / sqrt(2._dp)

contains

!==================================================
! Gaussian
!==================================================
elemental module function gaussian_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = exp(-x**2)
end function

elemental module function gaussian_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = -2._sp * x * exp(-x**2)
end function

elemental module function gaussian_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = exp(-x**2)
end function

elemental module function gaussian_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = -2._dp * x * exp(-x**2)
end function


!==================================================
! Exponential Linear Unit
!==================================================
elemental module function elu_sp( x , a ) result ( y )
    real(sp), intent(in) :: x
    real(sp), intent(in) :: a
    real(sp) :: y
    y = merge( x , a * (exp(x) - 1._sp), x >= 0._sp)
end function

elemental module function elu_grad_sp( x , a ) result ( y )
    real(sp), intent(in) :: x
    real(sp), intent(in) :: a
    real(sp) :: y
    y = merge( 1._sp , a * exp(x), x >= 0._sp)
end function

elemental module function elu_dp( x , a ) result ( y )
    real(dp), intent(in) :: x
    real(dp), intent(in) :: a
    real(dp) :: y
    y = merge( x , a * (exp(x) - 1._dp), x >= 0._dp)
end function

elemental module function elu_grad_dp( x , a ) result ( y )
    real(dp), intent(in) :: x
    real(dp), intent(in) :: a
    real(dp) :: y
    y = merge( 1._dp , a * exp(x), x >= 0._dp)
end function


!==================================================
! Rectified Linear Unit
!==================================================
elemental module function relu_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = max(0._sp, x)
end function

elemental module function relu_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = merge( 1._sp , 0._sp, x > 0._sp)
end function

elemental module function relu_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = max(0._dp, x)
end function

elemental module function relu_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = merge( 1._dp , 0._dp, x > 0._dp)
end function


!==================================================
! leaky Rectified Linear Unit
!==================================================
elemental module function leaky_relu_sp( x , a ) result( y )
    real(sp), intent(in) :: x
    real(sp), intent(in) :: a
    real(sp) :: y
    y = merge( x, a * x , x >= 0._sp)
end function

elemental module function leaky_relu_grad_sp( x , a ) result( y )
    real(sp), intent(in) :: x
    real(sp), intent(in) :: a
    real(sp) :: y
    y = merge( 1._sp , a , x >= 0._sp)
end function

elemental module function leaky_relu_dp( x , a ) result( y )
    real(dp), intent(in) :: x
    real(dp), intent(in) :: a
    real(dp) :: y
    y = merge( x, a * x , x >= 0._dp)
end function

elemental module function leaky_relu_grad_dp( x , a ) result( y )
    real(dp), intent(in) :: x
    real(dp), intent(in) :: a
    real(dp) :: y
    y = merge( 1._dp , a , x >= 0._dp)
end function


!==================================================
! GELU: Gaussian Error Linear Units function
!==================================================
elemental module function gelu_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = 0.5_sp * x * (1._sp + erf(x * isqrt2_sp))
end function

elemental module function gelu_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = 0.5_sp * (1._sp + erf(x * isqrt2_sp) )
    y = y + x * isqrt2_sp * exp( - 0.5_sp * x**2 )
end function

elemental module function gelu_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = 0.5_dp * x * (1._dp + erf(x * isqrt2_dp))
end function

elemental module function gelu_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = 0.5_dp * (1._dp + erf(x * isqrt2_dp) )
    y = y + x * isqrt2_dp * exp( - 0.5_dp * x**2 )
end function


elemental module function gelu_approx_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = 0.5_sp * x * (1._sp + fast_erf(x * isqrt2_sp))
end function

elemental module function gelu_approx_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = 0.5_sp * (1._sp + fast_erf(x * isqrt2_sp) )
    y = y + x * isqrt2_sp * exp( - 0.5_sp * x**2 )
end function

elemental module function gelu_approx_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = 0.5_dp * x * (1._dp + fast_erf(x * isqrt2_dp))
end function

elemental module function gelu_approx_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = 0.5_dp * (1._dp + fast_erf(x * isqrt2_dp) )
    y = y + x * isqrt2_dp * exp( - 0.5_dp * x**2 )
end function


!==================================================
! Scaled Exponential Linear Unit (SELU)
!==================================================
elemental module function selu_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    real(sp), parameter :: scale = 1.0507009873554804934193349852946_sp
    real(sp), parameter :: alpha = 1.6732632423543772848170429916717_sp
    y = merge( x , alpha * exp(x) - alpha, x > 0._sp)
    y = scale * y
end function

elemental module function selu_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    real(sp), parameter :: scale = 1.0507009873554804934193349852946_sp
    real(sp), parameter :: alpha = 1.6732632423543772848170429916717_sp
    y = merge( scale , scale * alpha * exp(x), x > 0._sp)
end function

elemental module function selu_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    real(dp), parameter :: scale = 1.0507009873554804934193349852946_dp
    real(dp), parameter :: alpha = 1.6732632423543772848170429916717_dp
    y = merge( x , alpha * exp(x) - alpha, x > 0._dp)
    y = scale * y
end function

elemental module function selu_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    real(dp), parameter :: scale = 1.0507009873554804934193349852946_dp
    real(dp), parameter :: alpha = 1.6732632423543772848170429916717_dp
    y = merge( scale , scale * alpha * exp(x), x > 0._dp)
end function


!==================================================
! Sigmoid
!==================================================
elemental module function sigmoid_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = 1._sp / (1._sp + exp(-x))
end function

elemental module function sigmoid_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = exp(x) / (1._sp + exp(x))**2
end function

elemental module function sigmoid_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = 1._dp / (1._dp + exp(-x))
end function

elemental module function sigmoid_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = exp(x) / (1._dp + exp(x))**2
end function


!==================================================
! SiLU: Sigmoid Linear Unit
!==================================================
elemental module function silu_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = x / (1._sp + exp(-x))
end function

elemental module function silu_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = (1._sp + exp(x))**2
    y = exp(x) * ( x + y ) / y
end function

elemental module function silu_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = x / (1._dp + exp(-x))
end function

elemental module function silu_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = (1._dp + exp(x))**2
    y = exp(x) * ( x + y ) / y
end function


!==================================================
! Step
!==================================================
elemental module function step_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = merge( 1._sp , 0._sp, x > 0._sp)
end function

elemental module function step_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = 0._sp
end function

elemental module function step_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = merge( 1._dp , 0._dp, x > 0._dp)
end function

elemental module function step_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = 0._dp
end function


!==================================================
! softmax
!==================================================
pure module function softmax_r1_sp( x ) result( y )
    real(sp), intent(in) :: x(:)
    real(sp) :: y(size(x))

    y = exp(x - maxval(x))
    y = y / sum(y)
end function

pure module function softmax_r2_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:)
    real(sp) :: y(size(x,1), size(x,2))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<2)then
        do j = 1, size(x,dim=2)
            y(:, j) = softmax( x(:, j) )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :) = softmax( x(j, :) )
        end do
    end if
end function
pure module function softmax_r3_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:,:)
    real(sp) :: y(size(x,1), size(x,2), size(x,3))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<3)then
        do j = 1, size(x,dim=3)
            y(:, :, j) = softmax( x(:, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :) = softmax( x(j, :, :), dim=2 )
        end do
    end if
end function
pure module function softmax_r4_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:,:,:)
    real(sp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<4)then
        do j = 1, size(x,dim=4)
            y(:, :, :, j) = softmax( x(:, :, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :, :) = softmax( x(j, :, :, :), dim=3 )
        end do
    end if
end function

pure module function softmax_grad_r1_sp( x ) result( y )
    real(sp), intent(in) :: x(:)
    real(sp) :: y(size(x))
    
    y = softmax(x)
    y = y * (1._sp - y)
end function

pure module function softmax_grad_r2_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:)
    real(sp) :: y(size(x,1), size(x,2))

    integer, intent(in), optional :: dim
    integer :: dim_

    dim_ = 1; if(present(dim)) dim_ = dim

    y = softmax(x,dim_)
    y = y * (1._sp - y)
end function
pure module function softmax_grad_r3_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:,:)
    real(sp) :: y(size(x,1), size(x,2), size(x,3))

    integer, intent(in), optional :: dim
    integer :: dim_

    dim_ = 1; if(present(dim)) dim_ = dim

    y = softmax(x,dim_)
    y = y * (1._sp - y)
end function
pure module function softmax_grad_r4_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:,:,:)
    real(sp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))

    integer, intent(in), optional :: dim
    integer :: dim_

    dim_ = 1; if(present(dim)) dim_ = dim

    y = softmax(x,dim_)
    y = y * (1._sp - y)
end function

pure module function softmax_r1_dp( x ) result( y )
    real(dp), intent(in) :: x(:)
    real(dp) :: y(size(x))

    y = exp(x - maxval(x))
    y = y / sum(y)
end function

pure module function softmax_r2_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:)
    real(dp) :: y(size(x,1), size(x,2))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<2)then
        do j = 1, size(x,dim=2)
            y(:, j) = softmax( x(:, j) )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :) = softmax( x(j, :) )
        end do
    end if
end function
pure module function softmax_r3_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:,:)
    real(dp) :: y(size(x,1), size(x,2), size(x,3))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<3)then
        do j = 1, size(x,dim=3)
            y(:, :, j) = softmax( x(:, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :) = softmax( x(j, :, :), dim=2 )
        end do
    end if
end function
pure module function softmax_r4_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:,:,:)
    real(dp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<4)then
        do j = 1, size(x,dim=4)
            y(:, :, :, j) = softmax( x(:, :, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :, :) = softmax( x(j, :, :, :), dim=3 )
        end do
    end if
end function

pure module function softmax_grad_r1_dp( x ) result( y )
    real(dp), intent(in) :: x(:)
    real(dp) :: y(size(x))
    
    y = softmax(x)
    y = y * (1._dp - y)
end function

pure module function softmax_grad_r2_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:)
    real(dp) :: y(size(x,1), size(x,2))

    integer, intent(in), optional :: dim
    integer :: dim_

    dim_ = 1; if(present(dim)) dim_ = dim

    y = softmax(x,dim_)
    y = y * (1._dp - y)
end function
pure module function softmax_grad_r3_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:,:)
    real(dp) :: y(size(x,1), size(x,2), size(x,3))

    integer, intent(in), optional :: dim
    integer :: dim_

    dim_ = 1; if(present(dim)) dim_ = dim

    y = softmax(x,dim_)
    y = y * (1._dp - y)
end function
pure module function softmax_grad_r4_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:,:,:)
    real(dp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))

    integer, intent(in), optional :: dim
    integer :: dim_

    dim_ = 1; if(present(dim)) dim_ = dim

    y = softmax(x,dim_)
    y = y * (1._dp - y)
end function


!==================================================
! logsoftmax
!==================================================
pure module function logsoftmax_r1_sp( x ) result( y )
    real(sp), intent(in) :: x(:)
    real(sp) :: y(size(x))

    y = x - maxval(x)
    y = y - log( sum(exp(y)) )
end function

pure module function logsoftmax_r2_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:)
    real(sp) :: y(size(x,1), size(x,2))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<2)then
        do j = 1, size(x,dim=2)
            y(:, j) = logsoftmax( x(:, j) )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :) = logsoftmax( x(j, :) )
        end do
    end if
end function
pure module function logsoftmax_r3_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:,:)
    real(sp) :: y(size(x,1), size(x,2), size(x,3))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<3)then
        do j = 1, size(x,dim=3)
            y(:, :, j) = logsoftmax( x(:, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :) = logsoftmax( x(j, :, :), dim=2 )
        end do
    end if
end function
pure module function logsoftmax_r4_sp( x , dim ) result( y )
    real(sp), intent(in) :: x(:,:,:,:)
    real(sp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<4)then
        do j = 1, size(x,dim=4)
            y(:, :, :, j) = logsoftmax( x(:, :, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :, :) = logsoftmax( x(j, :, :, :), dim=3 )
        end do
    end if
end function

pure module function logsoftmax_r1_dp( x ) result( y )
    real(dp), intent(in) :: x(:)
    real(dp) :: y(size(x))

    y = x - maxval(x)
    y = y - log( sum(exp(y)) )
end function

pure module function logsoftmax_r2_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:)
    real(dp) :: y(size(x,1), size(x,2))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<2)then
        do j = 1, size(x,dim=2)
            y(:, j) = logsoftmax( x(:, j) )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :) = logsoftmax( x(j, :) )
        end do
    end if
end function
pure module function logsoftmax_r3_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:,:)
    real(dp) :: y(size(x,1), size(x,2), size(x,3))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<3)then
        do j = 1, size(x,dim=3)
            y(:, :, j) = logsoftmax( x(:, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :) = logsoftmax( x(j, :, :), dim=2 )
        end do
    end if
end function
pure module function logsoftmax_r4_dp( x , dim ) result( y )
    real(dp), intent(in) :: x(:,:,:,:)
    real(dp) :: y(size(x,1), size(x,2), size(x,3), size(x,4))

    integer, intent(in), optional :: dim
    integer :: dim_, j

    dim_ = 1; if(present(dim)) dim_ = dim

    if(dim_<4)then
        do j = 1, size(x,dim=4)
            y(:, :, :, j) = logsoftmax( x(:, :, :, j), dim=dim_ )
        end do
    else
        do j = 1, size(x,dim=1)
            y(j, :, :, :) = logsoftmax( x(j, :, :, :), dim=3 )
        end do
    end if
end function


!==================================================
! softplus
!==================================================
elemental module function softplus_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = log(exp(x) + 1._sp)
end function

elemental module function softplus_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = exp(x) / (exp(x) + 1._sp)
end function

elemental module function softplus_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = log(exp(x) + 1._dp)
end function

elemental module function softplus_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = exp(x) / (exp(x) + 1._dp)
end function


!==================================================
! Fast intrinsics for accelerated activations
!==================================================

! Source: https://fortran-lang.discourse.group/t/fastgpt-faster-than-pytorch-in-300-lines-of-fortran/5385/31
elemental module function fast_tanh_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    real(sp) :: x2, a, b
    
    x2 = x*x
    a = x * (135135.0_sp + x2 * (17325.0_sp + x2 * (378.0_sp + x2)))
    b = 135135.0_sp + x2 * (62370.0_sp + x2 * (3150.0_sp + x2 * 28.0_sp))
    y = merge( a / b , sign(1._sp,x) , x2 <= 25._sp )
end function

elemental module function fast_tanh_grad_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    y = 1._sp - fast_tanh(x)**2
end function

elemental module function fast_erf_sp( x ) result( y )
    real(sp), intent(in) :: x
    real(sp) :: y
    real(sp) :: abs_x
    
    abs_x = abs(x)
    y = 1._sp - 1._sp / (1._sp+ 0.278393_sp*abs_x + 0.230389_sp*abs_x**2 + 0.000972_sp*abs_x**3 + 0.078108_sp*abs_x**4)**4
    y = y * sign(1.0_sp,x)
end function

elemental module function fast_tanh_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    real(dp) :: x2, a, b
    
    x2 = x*x
    a = x * (135135.0_dp + x2 * (17325.0_dp + x2 * (378.0_dp + x2)))
    b = 135135.0_dp + x2 * (62370.0_dp + x2 * (3150.0_dp + x2 * 28.0_dp))
    y = merge( a / b , sign(1._dp,x) , x2 <= 25._dp )
end function

elemental module function fast_tanh_grad_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    y = 1._dp - fast_tanh(x)**2
end function

elemental module function fast_erf_dp( x ) result( y )
    real(dp), intent(in) :: x
    real(dp) :: y
    real(dp) :: abs_x
    
    abs_x = abs(x)
    y = 1._dp - 1._dp / (1._dp+ 0.278393_dp*abs_x + 0.230389_dp*abs_x**2 + 0.000972_dp*abs_x**3 + 0.078108_dp*abs_x**4)**4
    y = y * sign(1.0_dp,x)
end function


end submodule