
module test_specialfunctions_activation
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use stdlib_kinds
    use stdlib_specialfunctions
    use stdlib_math, only: linspace
    implicit none
    private

    public :: collect_specialfunctions_activation
    ! use a low accuracy tolerance for the tests as activations should be fast
    ! and not necessarily accurate
    real(sp), parameter :: tol_sp = 1e-4_sp
    real(dp), parameter :: tol_dp = 1e-4_dp

contains

    subroutine collect_specialfunctions_activation(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [                                       &
            new_unittest("gaussian", test_gaussian),        &
            new_unittest("elu", test_elu),                  &
            new_unittest("relu", test_relu),                &
            new_unittest("leaky_relu", test_leaky_relu),    &
            new_unittest("gelu"   , test_gelu),             &
            new_unittest("selu"   , test_selu),             &
            new_unittest("sigmoid", test_sigmoid),          &
            new_unittest("silu"   , test_silu),             &
            new_unittest("softmax", test_softmax),          &
            new_unittest("logsoftmax", test_logsoftmax)     &
            ]
    end subroutine collect_specialfunctions_activation

    subroutine test_gaussian(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp) :: x(n), y(n), y_ref(n)

            x = linspace(-2._sp, 2._sp, n)
            y_ref = exp(-x**2)
            y = gaussian( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            y_ref = -2._sp * x * exp(-x**2)
            y = gaussian_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(n), y(n), y_ref(n)

            x = linspace(-2._dp, 2._dp, n)
            y_ref = exp(-x**2)
            y = gaussian( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            y_ref = -2._dp * x * exp(-x**2)
            y = gaussian_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_elu(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp) :: x(n), y(n), y_ref(n), a

            x = linspace(-2._sp , 2._sp, n)
            a = 1.0_sp
            where(x >= 0._sp)
                y_ref = x
            elsewhere
                y_ref = a * (exp(x) - 1._sp)
            end where
            y = elu( x , a )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            where(x >= 0._sp)
                y_ref = 1.0_sp
            elsewhere
                y_ref = a * exp(x)
            end where
            y = elu_grad( x , a )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(n), y(n), y_ref(n), a

            x = linspace(-2._dp , 2._dp, n)
            a = 1.0_dp
            where(x >= 0._dp)
                y_ref = x
            elsewhere
                y_ref = a * (exp(x) - 1._dp)
            end where
            y = elu( x , a )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            where(x >= 0._dp)
                y_ref = 1.0_dp
            elsewhere
                y_ref = a * exp(x)
            end where
            y = elu_grad( x , a )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_relu(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp) :: x(n), y(n), y_ref(n)

            x = linspace(-2._sp , 2._sp, n)
            y_ref = max(0._sp, x)
            y = relu( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            where(x > 0._sp)
                y_ref = 1.0_sp
            elsewhere
                y_ref = 0.0_sp
            end where
            y = relu_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(n), y(n), y_ref(n)

            x = linspace(-2._dp , 2._dp, n)
            y_ref = max(0._dp, x)
            y = relu( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            where(x > 0._dp)
                y_ref = 1.0_dp
            elsewhere
                y_ref = 0.0_dp
            end where
            y = relu_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_selu(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp), parameter :: scale = 1.0507009873554804934193349852946_sp
            real(sp), parameter :: alpha = 1.6732632423543772848170429916717_sp
            real(sp) :: x(n), y(n), y_ref(n)

            x = linspace(-2._sp, 2._sp, n)
            where(x >= 0._sp)
                y_ref = scale * x
            elsewhere
                y_ref = scale * (alpha * exp(x) - alpha)
            end where
            y = selu( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            where(x >= 0._sp)
                y_ref = scale
            elsewhere
                y_ref = scale * alpha * exp(x)
            end where
            y = selu_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp), parameter :: scale = 1.0507009873554804934193349852946_dp
            real(dp), parameter :: alpha = 1.6732632423543772848170429916717_dp
            real(dp) :: x(n), y(n), y_ref(n)

            x = linspace(-2._dp, 2._dp, n)
            where(x >= 0._dp)
                y_ref = scale * x
            elsewhere
                y_ref = scale * (alpha * exp(x) - alpha)
            end where
            y = selu( x )
            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return

            ! Derivative
            where(x >= 0._dp)
                y_ref = scale
            elsewhere
                y_ref = scale * alpha * exp(x)
            end where
            y = selu_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_gelu(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp) :: x(n), y(n), y_ref(n)

            y_ref = [-0.0455002784729  , -0.093188509345055, -0.148066952824593,&
                    -0.168328359723091, -0.0915712043643  ,  0.130650997161865,&
                    0.498338282108307,  0.963044226169586,  1.462367057800293,&
                    1.9544997215271  ]
            x = linspace(-2._sp, 2._sp, n)
            y = gelu( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            y = gelu_approx( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(n), y(n), y_ref(n)

            y_ref = [-0.0455002784729  , -0.093188509345055, -0.148066952824593,&
                    -0.168328359723091, -0.0915712043643  ,  0.130650997161865,&
                    0.498338282108307,  0.963044226169586,  1.462367057800293,&
                    1.9544997215271  ]
            x = linspace(-2._dp, 2._dp, n)
            y = gelu( x )
            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return

            y = gelu_approx( x )
            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_leaky_relu(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp) :: x(n), y(n), y_ref(n), a

            call random_number(x)
            a = 0.1_sp
            where(x>=0._sp)
                y_ref = x
            elsewhere
                y_ref = a * x
            end where
            y = leaky_relu( x , a )

            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(n), y(n), y_ref(n), a

            call random_number(x)
            a = 0.1_dp
            where(x>=0._dp)
                y_ref = x
            elsewhere
                y_ref = a * x
            end where
            y = leaky_relu( x , a )

            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_sigmoid(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp) :: x(n), y(n), y_ref(n)

            y_ref = [0.119202919304371, 0.174285307526588, 0.247663781046867,&
                    0.339243650436401, 0.444671928882599, 0.555328071117401,&
                    0.660756349563599, 0.752336204051971, 0.825714707374573,&
                    0.880797028541565]
            x = linspace(-2._sp, 2._sp, n)
            y = sigmoid( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(n), y(n), y_ref(n)

            y_ref = [0.119202919304371, 0.174285307526588, 0.247663781046867,&
                    0.339243650436401, 0.444671928882599, 0.555328071117401,&
                    0.660756349563599, 0.752336204051971, 0.825714707374573,&
                    0.880797028541565]
            x = linspace(-2._dp, 2._dp, n)
            y = sigmoid( x )
            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_silu(error)
        type(error_type), allocatable, intent(out) :: error
        integer, parameter :: n = 10
        block
            real(sp) :: x(n), y(n), y_ref(n), a

            x = linspace(-2._sp, 2._sp, n)
            y_ref = x / (1._sp + exp(-x))
            y = silu( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return

            ! Derivative
            y_ref = (1._sp + exp(x))**2
            y_ref = exp(x) * ( x + y_ref ) / y_ref
            y = silu_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(n), y(n), y_ref(n), a

            x = linspace(-2._dp, 2._dp, n)
            y_ref = x / (1._dp + exp(-x))
            y = silu( x )
            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return

            ! Derivative
            y_ref = (1._dp + exp(x))**2
            y_ref = exp(x) * ( x + y_ref ) / y_ref
            y = silu_grad( x )
            call check(error, norm2(y-y_ref) < n*tol_dp )
            if (allocated(error)) return
        end block
    end subroutine

    subroutine test_softmax(error)
        type(error_type), allocatable, intent(out) :: error

        block
            real(sp) :: x(3,3,3), y(3,3,3), y_ref(3,3,3)

            x = reshape( [ 0.82192878, 0.76998032, 0.98611263,&
                           0.8621334 , 0.65358045, 0.26387113,&
                           0.12743663, 0.35237132, 0.23801647,&
           
                           0.69773567, 0.40568874, 0.44789482,&
                           0.42930753, 0.49579193, 0.53139985,&
                           0.03035799, 0.65293157, 0.47613957,&
           
                           0.21088634, 0.9356926 , 0.0991312 ,&
                           0.46070181, 0.02943479, 0.17557538,&
                           0.10541313, 0.33946349, 0.34804323 ] ,[3,3,3] )

            !> softmax on dim = 1
            y = softmax(x,dim=1)

            y_ref = reshape( [ 0.319712639, 0.303528070, 0.376759291,&
                               0.423455358, 0.343743294, 0.232801422,&
                               0.296809316, 0.371676773, 0.331513911,&

                               0.395936400, 0.295658976, 0.308404684,&
                               0.314838648, 0.336482018, 0.348679334,&
                               0.225966826, 0.421138495, 0.352894694,&

                               0.252614945, 0.521480858, 0.225904226,&
                               0.416388273, 0.270521373, 0.313090324,&
                               0.282621205, 0.357150704, 0.360228121 ] ,[3,3,3] )

            call check(error, norm2(y-y_ref) < tol_sp )
            if (allocated(error)) return

            !> softmax on dim = 2
            y = softmax(x,dim=2)

            y_ref = reshape( [ 0.393646270, 0.392350882, 0.510482967,&
                               0.409795105, 0.349239051, 0.247922391,&
                               0.196558580, 0.258410037, 0.241594598,&
   
                               0.439052343, 0.296315849, 0.320951223,&
                               0.335690796, 0.324254662, 0.348903090,&
                               0.225256786, 0.379429489, 0.330145657,&
   
                               0.314101219, 0.511530280, 0.297435701,&
                               0.403239518, 0.206675291, 0.321064562,&
                               0.282659233, 0.281794399, 0.381499708 ] ,[3,3,3] )

            call check(error, norm2(y-y_ref) < tol_sp )
            if (allocated(error)) return

            !> softmax on dim = 3
            y = softmax(x,dim=3)

            y_ref = reshape( [ 0.412202179, 0.347835541, 0.501081109,&
                               0.431399941, 0.418453932, 0.310344934,&
                               0.346536130, 0.299599379, 0.295405835,&
           
                               0.364060789, 0.241637364, 0.292525023,&
                               0.279837668, 0.357372403, 0.405537367,&
                               0.314476222, 0.404643506, 0.374830246,&
           
                               0.223737061, 0.410527140, 0.206393898,&
                               0.288762331, 0.224173695, 0.284117699,&
                               0.338987619, 0.295757085, 0.329763889 ] ,[3,3,3] )

            call check(error, norm2(y-y_ref) <  tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(3,3,3), y(3,3,3), y_ref(3,3,3)

            x = reshape( [ 0.82192878, 0.76998032, 0.98611263,&
                           0.8621334 , 0.65358045, 0.26387113,&
                           0.12743663, 0.35237132, 0.23801647,&
           
                           0.69773567, 0.40568874, 0.44789482,&
                           0.42930753, 0.49579193, 0.53139985,&
                           0.03035799, 0.65293157, 0.47613957,&
           
                           0.21088634, 0.9356926 , 0.0991312 ,&
                           0.46070181, 0.02943479, 0.17557538,&
                           0.10541313, 0.33946349, 0.34804323 ] ,[3,3,3] )

            !> softmax on dim = 1
            y = softmax(x,dim=1)

            y_ref = reshape( [ 0.319712639, 0.303528070, 0.376759291,&
                               0.423455358, 0.343743294, 0.232801422,&
                               0.296809316, 0.371676773, 0.331513911,&

                               0.395936400, 0.295658976, 0.308404684,&
                               0.314838648, 0.336482018, 0.348679334,&
                               0.225966826, 0.421138495, 0.352894694,&

                               0.252614945, 0.521480858, 0.225904226,&
                               0.416388273, 0.270521373, 0.313090324,&
                               0.282621205, 0.357150704, 0.360228121 ] ,[3,3,3] )

            call check(error, norm2(y-y_ref) < tol_dp )
            if (allocated(error)) return

            !> softmax on dim = 2
            y = softmax(x,dim=2)

            y_ref = reshape( [ 0.393646270, 0.392350882, 0.510482967,&
                               0.409795105, 0.349239051, 0.247922391,&
                               0.196558580, 0.258410037, 0.241594598,&
   
                               0.439052343, 0.296315849, 0.320951223,&
                               0.335690796, 0.324254662, 0.348903090,&
                               0.225256786, 0.379429489, 0.330145657,&
   
                               0.314101219, 0.511530280, 0.297435701,&
                               0.403239518, 0.206675291, 0.321064562,&
                               0.282659233, 0.281794399, 0.381499708 ] ,[3,3,3] )

            call check(error, norm2(y-y_ref) < tol_dp )
            if (allocated(error)) return

            !> softmax on dim = 3
            y = softmax(x,dim=3)

            y_ref = reshape( [ 0.412202179, 0.347835541, 0.501081109,&
                               0.431399941, 0.418453932, 0.310344934,&
                               0.346536130, 0.299599379, 0.295405835,&
           
                               0.364060789, 0.241637364, 0.292525023,&
                               0.279837668, 0.357372403, 0.405537367,&
                               0.314476222, 0.404643506, 0.374830246,&
           
                               0.223737061, 0.410527140, 0.206393898,&
                               0.288762331, 0.224173695, 0.284117699,&
                               0.338987619, 0.295757085, 0.329763889 ] ,[3,3,3] )

            call check(error, norm2(y-y_ref) <  tol_dp )
            if (allocated(error)) return
        end block
    end subroutine test_softmax

    subroutine test_logsoftmax(error)
        type(error_type), allocatable, intent(out) :: error

        block
            real(sp) :: x(3,3,3), y(3,3,3), y_ref(3,3,3)
        
            x = reshape( [ 0.755308866500854,-0.789980888366699, 0.88806813955307 ,&
                          -1.210636496543884, 0.746919095516205, 0.177668794989586,&
                          0.540819883346558, 0.291532933712006,-0.324642956256866,&
                      
                          1.94184136390686 , 0.951070547103882,-2.303410291671753,&
                          0.59752631187439 , 1.189722180366516, 1.401878595352173,&
                          -0.262732744216919, 0.421907186508179,-0.200457707047462,&
                      
                          -0.702468574047089, 0.153426378965378, 0.330110251903534,&
                          -1.16956090927124 ,-0.845042765140533,-1.364316940307617,&
                          -1.679381608963013,-1.497506022453308,-1.194215059280396 ] ,[3,3,3] )
            
            !> logsoftmax on dim = 1
            y = logsoftmax(x,dim=1)

            y_ref = reshape( [ -0.856636286,-2.40192604,-0.723877013,&
                               -2.49238253,-0.534826934,-1.10407722 ,&
                               -0.788554132,-1.03784108,-1.65401697 ,&
   
                               -0.326149583,-1.31692040,-4.57140112 ,&
                               -1.61804128,-1.02584541,-0.813688993 ,&
                               -1.39805317,-0.713413179,-1.33577800 ,&
   
                               -1.81836534,-0.962470412,-0.785786569,&
                               -1.16514850,-0.840630412,-1.35990453 ,&
                               -1.34127355,-1.15939808,-0.856107056 ],[3,3,3] )

            !> logsoftmax on dim = 2
            y = logsoftmax(x,dim=2)

            y_ref = reshape( [ -0.666278005,-2.15167999, -0.581566215,&
                               -2.63222337 ,-0.614779949,-1.29196548 ,&
                               -0.880766988,-1.07016611,-1.79427731  ,&
   
                               -0.315551817,-1.05034387,-3.90906072  ,&
                               -1.65986681 ,-0.811692238,-0.203771874,&
                               -2.52012587 ,-1.57950723 ,-1.80610812 ,&
                               
                               -0.694792688,-0.444887042,-0.337523341,&
                               -1.16188502 ,-1.44335616 ,-2.03195047 ,&
                               -1.67170572 ,-2.09581947 ,-1.86184871 ],[3,3,3] )

            call check(error, norm2(y-y_ref) < tol_sp )
            if (allocated(error)) return
            
            !> logsoftmax on dim = 3
            y = logsoftmax(x,dim=3)
            
            y_ref = reshape( [ -1.50595474 , -2.22700500 ,-0.478398114,&
                               -2.09693313 , -1.01544499 ,-1.52940571 ,&
                               -0.442325860, -0.835677147,-0.936625183,&
   
                               -0.319422185, -0.485953659,-3.66987658 ,&
                               -0.288770229, -0.572641909,-0.305195898,&
                               -1.24587846 , -0.705302894,-0.812439919,&
   
                               -2.96373224 , -1.28359783 ,-1.03635597 ,&
                               -2.05585742 , -2.60740685 ,-3.07139134 ,&
                               -2.66252732 , -2.62471604 ,-1.80619729 ],[3,3,3] )
            
            call check(error, norm2(y-y_ref) < tol_sp )
            if (allocated(error)) return
        end block
        block
            real(dp) :: x(3,3,3), y(3,3,3), y_ref(3,3,3)
        
            x = reshape( [ 0.755308866500854,-0.789980888366699, 0.88806813955307 ,&
                          -1.210636496543884, 0.746919095516205, 0.177668794989586,&
                          0.540819883346558, 0.291532933712006,-0.324642956256866,&
                      
                          1.94184136390686 , 0.951070547103882,-2.303410291671753,&
                          0.59752631187439 , 1.189722180366516, 1.401878595352173,&
                          -0.262732744216919, 0.421907186508179,-0.200457707047462,&
                      
                          -0.702468574047089, 0.153426378965378, 0.330110251903534,&
                          -1.16956090927124 ,-0.845042765140533,-1.364316940307617,&
                          -1.679381608963013,-1.497506022453308,-1.194215059280396 ] ,[3,3,3] )
            
            !> logsoftmax on dim = 1
            y = logsoftmax(x,dim=1)

            y_ref = reshape( [ -0.856636286,-2.40192604,-0.723877013,&
                               -2.49238253,-0.534826934,-1.10407722 ,&
                               -0.788554132,-1.03784108,-1.65401697 ,&
   
                               -0.326149583,-1.31692040,-4.57140112 ,&
                               -1.61804128,-1.02584541,-0.813688993 ,&
                               -1.39805317,-0.713413179,-1.33577800 ,&
   
                               -1.81836534,-0.962470412,-0.785786569,&
                               -1.16514850,-0.840630412,-1.35990453 ,&
                               -1.34127355,-1.15939808,-0.856107056 ],[3,3,3] )

            !> logsoftmax on dim = 2
            y = logsoftmax(x,dim=2)

            y_ref = reshape( [ -0.666278005,-2.15167999, -0.581566215,&
                               -2.63222337 ,-0.614779949,-1.29196548 ,&
                               -0.880766988,-1.07016611,-1.79427731  ,&
   
                               -0.315551817,-1.05034387,-3.90906072  ,&
                               -1.65986681 ,-0.811692238,-0.203771874,&
                               -2.52012587 ,-1.57950723 ,-1.80610812 ,&
                               
                               -0.694792688,-0.444887042,-0.337523341,&
                               -1.16188502 ,-1.44335616 ,-2.03195047 ,&
                               -1.67170572 ,-2.09581947 ,-1.86184871 ],[3,3,3] )

            call check(error, norm2(y-y_ref) < tol_dp )
            if (allocated(error)) return
            
            !> logsoftmax on dim = 3
            y = logsoftmax(x,dim=3)
            
            y_ref = reshape( [ -1.50595474 , -2.22700500 ,-0.478398114,&
                               -2.09693313 , -1.01544499 ,-1.52940571 ,&
                               -0.442325860, -0.835677147,-0.936625183,&
   
                               -0.319422185, -0.485953659,-3.66987658 ,&
                               -0.288770229, -0.572641909,-0.305195898,&
                               -1.24587846 , -0.705302894,-0.812439919,&
   
                               -2.96373224 , -1.28359783 ,-1.03635597 ,&
                               -2.05585742 , -2.60740685 ,-3.07139134 ,&
                               -2.66252732 , -2.62471604 ,-1.80619729 ],[3,3,3] )
            
            call check(error, norm2(y-y_ref) < tol_dp )
            if (allocated(error)) return
        end block
    end subroutine test_logsoftmax


end module test_specialfunctions_activation

program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_specialfunctions_activation, only : collect_specialfunctions_activation
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [new_testsuite("activation functions",                      &
                 collect_specialfunctions_activation)]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester