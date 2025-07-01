
! Test vector norms
module test_linalg_norm
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg_constants
    use stdlib_linalg, only: norm, linalg_state_type
    use stdlib_linalg_state, only: linalg_state_type

    implicit none (type,external)

    contains

    !> Vector norm tests
    subroutine test_vector_norms(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        allocate(tests(0))
        
        call add_test(tests,new_unittest("strided_1d_norm_s",test_strided_1d_s))
        call add_test(tests,new_unittest("norm_s_1d",test_norm_s_1d))      
        call add_test(tests,new_unittest("norm_s_2d",test_norm_s_2d))      
        call add_test(tests,new_unittest("norm_s_3d",test_norm_s_3d))      
        call add_test(tests,new_unittest("norm2_s_2d",test_norm2_s_2d))        
        call add_test(tests,new_unittest("maxabs_s_2d",test_maxabs_s_2d))        
        call add_test(tests,new_unittest("norm_dimmed_s_2d",test_norm_dimmed_s_2d))        
        call add_test(tests,new_unittest("norm2_s_3d",test_norm2_s_3d))        
        call add_test(tests,new_unittest("maxabs_s_3d",test_maxabs_s_3d))        
        call add_test(tests,new_unittest("norm_dimmed_s_3d",test_norm_dimmed_s_3d))        
        call add_test(tests,new_unittest("strided_1d_norm_d",test_strided_1d_d))
        call add_test(tests,new_unittest("norm_d_1d",test_norm_d_1d))      
        call add_test(tests,new_unittest("norm_d_2d",test_norm_d_2d))      
        call add_test(tests,new_unittest("norm_d_3d",test_norm_d_3d))      
        call add_test(tests,new_unittest("norm2_d_2d",test_norm2_d_2d))        
        call add_test(tests,new_unittest("maxabs_d_2d",test_maxabs_d_2d))        
        call add_test(tests,new_unittest("norm_dimmed_d_2d",test_norm_dimmed_d_2d))        
        call add_test(tests,new_unittest("norm2_d_3d",test_norm2_d_3d))        
        call add_test(tests,new_unittest("maxabs_d_3d",test_maxabs_d_3d))        
        call add_test(tests,new_unittest("norm_dimmed_d_3d",test_norm_dimmed_d_3d))        
        call add_test(tests,new_unittest("strided_1d_norm_c",test_strided_1d_c))
        call add_test(tests,new_unittest("norm_c_1d",test_norm_c_1d))      
        call add_test(tests,new_unittest("norm_c_2d",test_norm_c_2d))      
        call add_test(tests,new_unittest("norm_c_3d",test_norm_c_3d))      
        call add_test(tests,new_unittest("maxabs_c_2d",test_maxabs_c_2d))        
        call add_test(tests,new_unittest("norm_dimmed_c_2d",test_norm_dimmed_c_2d))        
        call add_test(tests,new_unittest("maxabs_c_3d",test_maxabs_c_3d))        
        call add_test(tests,new_unittest("norm_dimmed_c_3d",test_norm_dimmed_c_3d))        
        call add_test(tests,new_unittest("strided_1d_norm_z",test_strided_1d_z))
        call add_test(tests,new_unittest("norm_z_1d",test_norm_z_1d))      
        call add_test(tests,new_unittest("norm_z_2d",test_norm_z_2d))      
        call add_test(tests,new_unittest("norm_z_3d",test_norm_z_3d))      
        call add_test(tests,new_unittest("maxabs_z_2d",test_maxabs_z_2d))        
        call add_test(tests,new_unittest("norm_dimmed_z_2d",test_norm_dimmed_z_2d))        
        call add_test(tests,new_unittest("maxabs_z_3d",test_maxabs_z_3d))        
        call add_test(tests,new_unittest("norm_dimmed_z_3d",test_norm_dimmed_z_3d))        

    end subroutine test_vector_norms
    
    
    !> Test strided norm
    subroutine test_strided_1d_s(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp), parameter :: m = 8_ilp
        integer(ilp), parameter :: n = m**2
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), target :: a(n)
        real(sp), allocatable :: slice(:)
        real(sp), pointer :: twod(:,:)
        real(sp) :: rea(n),ima(n)
        
        call random_number(rea)
        a = rea
        
        ! Test sliced array results
        slice = a(4:7:59)                
        call check(error,abs(norm(a(4:7:59),2)-norm(slice,2))<tol*max(1.0_sp,norm(slice,2)), &
                         'sliced real(sp) norm(a(4:7:59),2)')
        if (allocated(error)) return
        
        ! Test 2d array results
        twod(1:m,1:m) => a                   
        call check(error,abs(norm(twod,2)-norm(a,2))<tol*max(1.0_sp,norm(twod,2)), &
                         '2d-reshaped real(sp) norm(a,2)')
        if (allocated(error)) return        
        
        ! Test row norm (strided access)
        slice = twod(3,:) 
        call check(error,abs(norm(twod(3,:),2)-norm(slice,2))<tol*max(1.0_sp,norm(twod(3,:),2)), &
                         'row real(sp) norm(t(3,:),2)')
        if (allocated(error)) return                


        ! Test column norm (strided access)
        slice = twod(::2,3) 
        call check(error,abs(norm(twod(::2,3),2)-norm(slice,2))<tol*max(1.0_sp,norm(twod(::2,3),2)), &
                         'column real(sp) norm(t(::2,3),2)')
        if (allocated(error)) return                

        
        
    end subroutine test_strided_1d_s    
    
    
    !> Test several norms with different dimensions
    subroutine test_norm_s_1d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**1
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: a(:), b(:)
        character(64) :: msg
        
        allocate(a(n), b(2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_sp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_sp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_s_1d
    
    !> Test several norms with different dimensions
    subroutine test_norm_s_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**2
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: a(:), b(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_sp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_sp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_s_2d
    
    !> Test several norms with different dimensions
    subroutine test_norm_s_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**3
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: a(:), b(:,:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_sp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_sp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_s_3d
    
    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    subroutine test_norm2_s_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: a(:), b(:,:)
        intrinsic :: norm2
        character(64) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,2) - norm2(a))<tol*norm(a,2),&
                         'Euclidean norm does not match real(sp) `norm2` intrinsic')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,2)-norm2(b))<tol*norm(b,2),&
                         'Dimmed Euclidean norm does not match real(sp) `norm2` intrinsic')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Euclidean norms match real(sp) `norm2` intrinsic')") dim
            call check(error,all(abs(norm(b,2,dim)-norm2(b,dim))<tol*max(1.0_sp,norm(b,2,dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_norm2_s_2d
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_s_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: a(:), b(:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match real(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match real(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match real(sp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_sp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_s_2d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_s_2d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        real(sp), allocatable :: a(:), b(:,:)
        real(sp), allocatable :: bnrm(:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' sp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 2d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_s_2d
    

    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    subroutine test_norm2_s_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: a(:), b(:,:,:)
        intrinsic :: norm2
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,2) - norm2(a))<tol*norm(a,2),&
                         'Euclidean norm does not match real(sp) `norm2` intrinsic')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,2)-norm2(b))<tol*norm(b,2),&
                         'Dimmed Euclidean norm does not match real(sp) `norm2` intrinsic')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Euclidean norms match real(sp) `norm2` intrinsic')") dim
            call check(error,all(abs(norm(b,2,dim)-norm2(b,dim))<tol*max(1.0_sp,norm(b,2,dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_norm2_s_3d
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_s_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: a(:), b(:,:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match real(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match real(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match real(sp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_sp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_s_3d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_s_3d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        real(sp), allocatable :: a(:), b(:,:,:)
        real(sp), allocatable :: bnrm(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' sp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 3d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_s_3d
    

    
    !> Test strided norm
    subroutine test_strided_1d_d(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp), parameter :: m = 8_ilp
        integer(ilp), parameter :: n = m**2
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), target :: a(n)
        real(dp), allocatable :: slice(:)
        real(dp), pointer :: twod(:,:)
        real(dp) :: rea(n),ima(n)
        
        call random_number(rea)
        a = rea
        
        ! Test sliced array results
        slice = a(4:7:59)                
        call check(error,abs(norm(a(4:7:59),2)-norm(slice,2))<tol*max(1.0_dp,norm(slice,2)), &
                         'sliced real(dp) norm(a(4:7:59),2)')
        if (allocated(error)) return
        
        ! Test 2d array results
        twod(1:m,1:m) => a                   
        call check(error,abs(norm(twod,2)-norm(a,2))<tol*max(1.0_dp,norm(twod,2)), &
                         '2d-reshaped real(dp) norm(a,2)')
        if (allocated(error)) return        
        
        ! Test row norm (strided access)
        slice = twod(3,:) 
        call check(error,abs(norm(twod(3,:),2)-norm(slice,2))<tol*max(1.0_dp,norm(twod(3,:),2)), &
                         'row real(dp) norm(t(3,:),2)')
        if (allocated(error)) return                


        ! Test column norm (strided access)
        slice = twod(::2,3) 
        call check(error,abs(norm(twod(::2,3),2)-norm(slice,2))<tol*max(1.0_dp,norm(twod(::2,3),2)), &
                         'column real(dp) norm(t(::2,3),2)')
        if (allocated(error)) return                

        
        
    end subroutine test_strided_1d_d    
    
    
    !> Test several norms with different dimensions
    subroutine test_norm_d_1d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**1
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: a(:), b(:)
        character(64) :: msg
        
        allocate(a(n), b(2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_dp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_dp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_d_1d
    
    !> Test several norms with different dimensions
    subroutine test_norm_d_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**2
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: a(:), b(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_dp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_dp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_d_2d
    
    !> Test several norms with different dimensions
    subroutine test_norm_d_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**3
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: a(:), b(:,:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_dp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_dp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_d_3d
    
    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    subroutine test_norm2_d_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: a(:), b(:,:)
        intrinsic :: norm2
        character(64) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,2) - norm2(a))<tol*norm(a,2),&
                         'Euclidean norm does not match real(dp) `norm2` intrinsic')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,2)-norm2(b))<tol*norm(b,2),&
                         'Dimmed Euclidean norm does not match real(dp) `norm2` intrinsic')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Euclidean norms match real(dp) `norm2` intrinsic')") dim
            call check(error,all(abs(norm(b,2,dim)-norm2(b,dim))<tol*max(1.0_dp,norm(b,2,dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_norm2_d_2d
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_d_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: a(:), b(:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match real(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match real(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match real(dp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_dp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_d_2d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_d_2d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        real(dp), allocatable :: a(:), b(:,:)
        real(dp), allocatable :: bnrm(:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' dp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 2d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_d_2d
    

    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    subroutine test_norm2_d_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: a(:), b(:,:,:)
        intrinsic :: norm2
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,2) - norm2(a))<tol*norm(a,2),&
                         'Euclidean norm does not match real(dp) `norm2` intrinsic')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,2)-norm2(b))<tol*norm(b,2),&
                         'Dimmed Euclidean norm does not match real(dp) `norm2` intrinsic')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Euclidean norms match real(dp) `norm2` intrinsic')") dim
            call check(error,all(abs(norm(b,2,dim)-norm2(b,dim))<tol*max(1.0_dp,norm(b,2,dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_norm2_d_3d
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_d_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: a(:), b(:,:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match real(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match real(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match real(dp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_dp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_d_3d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_d_3d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        real(dp), allocatable :: a(:), b(:,:,:)
        real(dp), allocatable :: bnrm(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' dp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 3d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_d_3d
    

    
    !> Test strided norm
    subroutine test_strided_1d_c(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp), parameter :: m = 8_ilp
        integer(ilp), parameter :: n = m**2
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        complex(sp), target :: a(n)
        complex(sp), allocatable :: slice(:)
        complex(sp), pointer :: twod(:,:)
        real(sp) :: rea(n),ima(n)
        
        call random_number(rea)
        call random_number(ima)
        a = cmplx(rea,ima,kind=sp)
        
        ! Test sliced array results
        slice = a(4:7:59)                
        call check(error,abs(norm(a(4:7:59),2)-norm(slice,2))<tol*max(1.0_sp,norm(slice,2)), &
                         'sliced complex(sp) norm(a(4:7:59),2)')
        if (allocated(error)) return
        
        ! Test 2d array results
        twod(1:m,1:m) => a                   
        call check(error,abs(norm(twod,2)-norm(a,2))<tol*max(1.0_sp,norm(twod,2)), &
                         '2d-reshaped complex(sp) norm(a,2)')
        if (allocated(error)) return        
        
        ! Test row norm (strided access)
        slice = twod(3,:) 
        call check(error,abs(norm(twod(3,:),2)-norm(slice,2))<tol*max(1.0_sp,norm(twod(3,:),2)), &
                         'row complex(sp) norm(t(3,:),2)')
        if (allocated(error)) return                


        ! Test column norm (strided access)
        slice = twod(::2,3) 
        call check(error,abs(norm(twod(::2,3),2)-norm(slice,2))<tol*max(1.0_sp,norm(twod(::2,3),2)), &
                         'column complex(sp) norm(t(::2,3),2)')
        if (allocated(error)) return                

        
        
    end subroutine test_strided_1d_c    
    
    
    !> Test several norms with different dimensions
    subroutine test_norm_c_1d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**1
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        complex(sp), allocatable :: a(:), b(:)
        character(64) :: msg
        
        allocate(a(n), b(2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_sp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_sp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_c_1d
    
    !> Test several norms with different dimensions
    subroutine test_norm_c_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**2
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        complex(sp), allocatable :: a(:), b(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_sp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_sp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_c_2d
    
    !> Test several norms with different dimensions
    subroutine test_norm_c_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**3
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        complex(sp), allocatable :: a(:), b(:,:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_sp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_sp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_c_3d
    
    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_c_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        complex(sp), allocatable :: a(:), b(:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match complex(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match complex(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match complex(sp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_sp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_c_2d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_c_2d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        complex(sp), allocatable :: a(:), b(:,:)
        real(sp), allocatable :: bnrm(:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' sp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 2d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_c_2d
    

    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_c_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        complex(sp), allocatable :: a(:), b(:,:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match complex(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match complex(sp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match complex(sp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_sp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_c_3d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_c_3d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        complex(sp), allocatable :: a(:), b(:,:,:)
        real(sp), allocatable :: bnrm(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' sp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 3d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_sp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_c_3d
    

    
    !> Test strided norm
    subroutine test_strided_1d_z(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp), parameter :: m = 8_ilp
        integer(ilp), parameter :: n = m**2
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        complex(dp), target :: a(n)
        complex(dp), allocatable :: slice(:)
        complex(dp), pointer :: twod(:,:)
        real(dp) :: rea(n),ima(n)
        
        call random_number(rea)
        call random_number(ima)
        a = cmplx(rea,ima,kind=dp)
        
        ! Test sliced array results
        slice = a(4:7:59)                
        call check(error,abs(norm(a(4:7:59),2)-norm(slice,2))<tol*max(1.0_dp,norm(slice,2)), &
                         'sliced complex(dp) norm(a(4:7:59),2)')
        if (allocated(error)) return
        
        ! Test 2d array results
        twod(1:m,1:m) => a                   
        call check(error,abs(norm(twod,2)-norm(a,2))<tol*max(1.0_dp,norm(twod,2)), &
                         '2d-reshaped complex(dp) norm(a,2)')
        if (allocated(error)) return        
        
        ! Test row norm (strided access)
        slice = twod(3,:) 
        call check(error,abs(norm(twod(3,:),2)-norm(slice,2))<tol*max(1.0_dp,norm(twod(3,:),2)), &
                         'row complex(dp) norm(t(3,:),2)')
        if (allocated(error)) return                


        ! Test column norm (strided access)
        slice = twod(::2,3) 
        call check(error,abs(norm(twod(::2,3),2)-norm(slice,2))<tol*max(1.0_dp,norm(twod(::2,3),2)), &
                         'column complex(dp) norm(t(::2,3),2)')
        if (allocated(error)) return                

        
        
    end subroutine test_strided_1d_z    
    
    
    !> Test several norms with different dimensions
    subroutine test_norm_z_1d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**1
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        complex(dp), allocatable :: a(:), b(:)
        character(64) :: msg
        
        allocate(a(n), b(2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_dp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_dp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_z_1d
    
    !> Test several norms with different dimensions
    subroutine test_norm_z_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**2
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        complex(dp), allocatable :: a(:), b(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_dp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_dp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_z_2d
    
    !> Test several norms with different dimensions
    subroutine test_norm_z_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,order
        integer(ilp), parameter :: n   = 2_ilp**3
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        complex(dp), allocatable :: a(:), b(:,:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))        
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        do order = 1, 10            
           write(msg,"('reshaped order-',i0,' p-norm is the same')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),trim(msg))
           if (allocated(error)) return           
        end do
        
        ! Infinity norms
        call check(error,abs(norm(a,'inf')-norm(b,'inf'))<tol*max(1.0_dp,norm(a,'inf')),&
                         'reshaped +infinity norm is the same')
        if (allocated(error)) return        

        ! Infinity norms
        call check(error,abs(norm(a,'-inf')-norm(b,'-inf'))<tol*max(1.0_dp,norm(a,'-inf')),&
                         'reshaped -infinity norm is the same')
        if (allocated(error)) return           
        
    end subroutine test_norm_z_3d
    
    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_z_2d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        complex(dp), allocatable :: a(:), b(:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match complex(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match complex(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match complex(dp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_dp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_z_2d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_z_2d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 2
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        complex(dp), allocatable :: a(:), b(:,:)
        real(dp), allocatable :: bnrm(:)
        character(64) :: msg
        
        allocate(a(n), b(2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' dp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 2d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_z_2d
    

    !> Test Euclidean norm; compare with Fortran intrinsic norm2 for reals
    
    !> Test Infinity norm; compare with Fortran intrinsic max(abs(a))
    subroutine test_maxabs_z_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: j,dim
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n   = 2_ilp**ndim
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        complex(dp), allocatable :: a(:), b(:,:,:)
        intrinsic :: maxval, abs
        character(128) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test some norms
        call check(error,abs(norm(a,'inf') - maxval(abs(a)))<tol*norm(a,'inf'),&
                         'Infinity norm does not match complex(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return               
        
        ! Infinity norms
        call check(error,abs(norm(b,'inf')-maxval(abs(b)))<tol*norm(b,'inf'),&
                         'Dimmed Infinity norm does not match complex(dp) `maxval(abs(.))` intrinsics')
        if (allocated(error)) return                       
        
        ! Test norm as collapsed around dimension
        do dim = 1, ndim
            write(msg,"('Not all dim=',i0,' Infinity norms match complex(dp) `maxval(abs(.))` intrinsics')") dim
            call check(error,all(abs(norm(b,'inf',dim)-maxval(abs(b),dim))<tol*max(1.0_dp,norm(b,'inf',dim))),&
                       trim(msg)) 
            if (allocated(error)) return             
        end do
        
    end subroutine test_maxabs_z_3d    
    
    ! Test norm along a dimension and compare it against individually evaluated norms
    subroutine test_norm_dimmed_z_3d(error)
        type(error_type), allocatable, intent(out) :: error
       
        integer(ilp) :: j,dim,order
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim, dim=1,ndim)]
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        integer(ilp) :: coords(ndim)
        real :: x(ndim)
        complex(dp), allocatable :: a(:), b(:,:,:)
        real(dp), allocatable :: bnrm(:,:)
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))        
        
        do order = 1, 5
        
           do dim = 1, ndim
            
               bnrm = norm(b, order, dim)
               
               ! Assert size
               write(msg,"('dim=',i0,', order=',i0,' dp norm has the wrong shape')") dim, order
               call check(error,all( shape(bnrm)==pack(shape(b),dims/=dim) ), trim(msg))
               if (allocated(error)) return                    
               
           end do 
           
           ! Compare ND whole vector norm with unrolled vector norm
           write(msg,"('Unrolled (1d) vs 3d order=',i0,' norm')") order
           call check(error,abs(norm(a,order)-norm(b,order))<tol*max(1.0_dp,norm(a,order)),&
                      trim(msg))
           if (allocated(error)) return              
           
            
        end do
        
    end subroutine test_norm_dimmed_z_3d
    


    ! gcc-15 bugfix utility
    subroutine add_test(tests,new_test)
        type(unittest_type), allocatable, intent(inout) :: tests(:)    
        type(unittest_type), intent(in) :: new_test
        
        integer :: n
        type(unittest_type), allocatable :: new_tests(:)
        
        if (allocated(tests)) then 
            n = size(tests)
        else
            n = 0
        end if
        
        allocate(new_tests(n+1))
        if (n>0) new_tests(1:n) = tests(1:n)
                 new_tests(1+n) = new_test
        call move_alloc(from=new_tests,to=tests)        
        
    end subroutine add_test

end module test_linalg_norm

program test_norm
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_norm, only : test_vector_norms
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_norm", test_vector_norms) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_norm


