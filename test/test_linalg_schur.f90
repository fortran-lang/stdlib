! Test Schur decomposition
module test_linalg_schur
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg_constants
    use stdlib_linalg_state, only: LINALG_VALUE_ERROR,linalg_state_type
    use stdlib_linalg, only: schur,schur_space
    use ieee_arithmetic, only: ieee_value,ieee_quiet_nan

    implicit none (type,external)
    
    public :: test_schur_decomposition

    contains

    !> schur decomposition tests
    subroutine test_schur_decomposition(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        allocate(tests(0))
        
        call add_test(tests,new_unittest("schur_api_s",test_schur_api_s))
        call add_test(tests,new_unittest("schur_random_s",test_schur_random_s))
        call add_test(tests,new_unittest("schur_symmetric_s",test_schur_symmetric_s))
        call add_test(tests,new_unittest("schur_api_d",test_schur_api_d))
        call add_test(tests,new_unittest("schur_random_d",test_schur_random_d))
        call add_test(tests,new_unittest("schur_symmetric_d",test_schur_symmetric_d))
        call add_test(tests,new_unittest("schur_api_c",test_schur_api_c))
        call add_test(tests,new_unittest("schur_random_c",test_schur_random_c))
        call add_test(tests,new_unittest("schur_symmetric_c",test_schur_symmetric_c))
        call add_test(tests,new_unittest("schur_api_z",test_schur_api_z))
        call add_test(tests,new_unittest("schur_random_z",test_schur_random_z))
        call add_test(tests,new_unittest("schur_symmetric_z",test_schur_symmetric_z))

    end subroutine test_schur_decomposition

    !> schur decomposition of a random matrix
    subroutine test_schur_api_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n   = 15_ilp
        integer(ilp) :: lwork
        complex(sp) :: eigs(n)
        real(sp), dimension(n,n) :: a,t,z
        real(sp), allocatable :: storage(:)
        type(linalg_state_type) :: state
        
        call random_number(a)
        
        ! Test simple API
        call schur(a,t,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test output transformation matrix
        call schur(a,t,z,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      

        ! Test output eigenvalues
        call schur(a,t,eigvals=eigs,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test storage query
        call schur_space(a,lwork,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test with user-defined storage
        allocate(storage(lwork))        
        call schur(a,t,eigvals=eigs,storage=storage,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
    end subroutine test_schur_api_s    
    
    subroutine test_schur_random_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: rtol = 1.0e-4_sp
        real(sp), parameter :: eps  = sqrt(epsilon(0.0_sp))
        integer(ilp) :: lwork
        real(sp), allocatable :: storage(:)
        real(sp), dimension(n,n) :: a,t,z,aorig
        type(linalg_state_type) :: state

        call random_number(a)
        aorig = a

        ! 1) Run schur (standard)
        call schur(a,t,z,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (real(sp))')
        if (allocated(error)) return             

        ! 2) Run schur (overwrite A)
        call schur(a,t,z,overwrite_a=.true.,err=state)

        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(aorig,z,t)<=max(rtol*abs(aorig),eps)), &
                          'converged solution (real(sp) - overwrite A)')
        if (allocated(error)) return            
        
        ! 3) Use working storage
        a = aorig   
        call schur_space(a,lwork,err=state)
         
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        allocate(storage(lwork))         
        call schur(a,t,z,storage=storage,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (real(sp) - external storage)')
        if (allocated(error)) return            

    contains
    
        pure function schur_error(a,z,t) result(err)
            real(sp), intent(in), dimension(:,:) :: a,z,t
            real(sp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),transpose(z)) - a)
        end function schur_error
        
    end subroutine test_schur_random_s

    !> Test symmetric matrix (real eigenvalues)
    subroutine test_schur_symmetric_s(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: rtol = 1.0e-4_sp
        real(sp), parameter :: eps  = sqrt(epsilon(0.0_sp))
        real(sp) :: reigs(n)
        real(sp), dimension(n,n) :: a, t, z
        type(linalg_state_type) :: state

        ! Define a symmetric 3x3 matrix with real eigenvalues
        a = reshape([ 3, 1, 0, &
                      1, 3, 1, &
                      0, 1, 3], shape=[n, n])

        ! Return real eigenvalues (Should trigger an error if they have an imaginary part)
        call schur(a, t, z, eigvals=reigs, err=state)
        
        ! Check return code
        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        ! Check solution
        call check(error, all(schur_error(a, z, t) <= max(rtol * abs(a), eps)), &
                          'converged solution (real symmetric, real eigs)')
        if (allocated(error)) return

    contains
    
        pure function schur_error(a,z,t) result(err)
            real(sp), intent(in), dimension(:,:) :: a,z,t
            real(sp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),transpose(z)) - a)
        end function schur_error        

    end subroutine test_schur_symmetric_s

    subroutine test_schur_api_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n   = 15_ilp
        integer(ilp) :: lwork
        complex(dp) :: eigs(n)
        real(dp), dimension(n,n) :: a,t,z
        real(dp), allocatable :: storage(:)
        type(linalg_state_type) :: state
        
        call random_number(a)
        
        ! Test simple API
        call schur(a,t,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test output transformation matrix
        call schur(a,t,z,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      

        ! Test output eigenvalues
        call schur(a,t,eigvals=eigs,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test storage query
        call schur_space(a,lwork,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test with user-defined storage
        allocate(storage(lwork))        
        call schur(a,t,eigvals=eigs,storage=storage,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
    end subroutine test_schur_api_d    
    
    subroutine test_schur_random_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: rtol = 1.0e-4_dp
        real(dp), parameter :: eps  = sqrt(epsilon(0.0_dp))
        integer(ilp) :: lwork
        real(dp), allocatable :: storage(:)
        real(dp), dimension(n,n) :: a,t,z,aorig
        type(linalg_state_type) :: state

        call random_number(a)
        aorig = a

        ! 1) Run schur (standard)
        call schur(a,t,z,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (real(dp))')
        if (allocated(error)) return             

        ! 2) Run schur (overwrite A)
        call schur(a,t,z,overwrite_a=.true.,err=state)

        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(aorig,z,t)<=max(rtol*abs(aorig),eps)), &
                          'converged solution (real(dp) - overwrite A)')
        if (allocated(error)) return            
        
        ! 3) Use working storage
        a = aorig   
        call schur_space(a,lwork,err=state)
         
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        allocate(storage(lwork))         
        call schur(a,t,z,storage=storage,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (real(dp) - external storage)')
        if (allocated(error)) return            

    contains
    
        pure function schur_error(a,z,t) result(err)
            real(dp), intent(in), dimension(:,:) :: a,z,t
            real(dp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),transpose(z)) - a)
        end function schur_error
        
    end subroutine test_schur_random_d

    !> Test symmetric matrix (real eigenvalues)
    subroutine test_schur_symmetric_d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: rtol = 1.0e-4_dp
        real(dp), parameter :: eps  = sqrt(epsilon(0.0_dp))
        real(dp) :: reigs(n)
        real(dp), dimension(n,n) :: a, t, z
        type(linalg_state_type) :: state

        ! Define a symmetric 3x3 matrix with real eigenvalues
        a = reshape([ 3, 1, 0, &
                      1, 3, 1, &
                      0, 1, 3], shape=[n, n])

        ! Return real eigenvalues (Should trigger an error if they have an imaginary part)
        call schur(a, t, z, eigvals=reigs, err=state)
        
        ! Check return code
        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        ! Check solution
        call check(error, all(schur_error(a, z, t) <= max(rtol * abs(a), eps)), &
                          'converged solution (real symmetric, real eigs)')
        if (allocated(error)) return

    contains
    
        pure function schur_error(a,z,t) result(err)
            real(dp), intent(in), dimension(:,:) :: a,z,t
            real(dp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),transpose(z)) - a)
        end function schur_error        

    end subroutine test_schur_symmetric_d

    subroutine test_schur_api_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n   = 15_ilp
        integer(ilp) :: lwork
        complex(sp) :: eigs(n)
        complex(sp), dimension(n,n) :: a,t,z
        complex(sp), allocatable :: storage(:)
        real(sp) :: rea(n,n),ima(n,n)
        type(linalg_state_type) :: state
        
        call random_number(rea)
        call random_number(ima)
        a = cmplx(rea,ima,kind=sp)
        
        ! Test simple API
        call schur(a,t,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test output transformation matrix
        call schur(a,t,z,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      

        ! Test output eigenvalues
        call schur(a,t,eigvals=eigs,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test storage query
        call schur_space(a,lwork,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test with user-defined storage
        allocate(storage(lwork))        
        call schur(a,t,eigvals=eigs,storage=storage,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
    end subroutine test_schur_api_c    
    
    subroutine test_schur_random_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: rtol = 1.0e-4_sp
        real(sp), parameter :: eps  = sqrt(epsilon(0.0_sp))
        integer(ilp) :: lwork
        complex(sp), allocatable :: storage(:)
        complex(sp), dimension(n,n) :: a,t,z,aorig
        real(sp), dimension(n,n) :: a_re,a_im
        type(linalg_state_type) :: state

        call random_number(a_re)
        call random_number(a_im)
        a = cmplx(a_re,a_im,kind=sp)
        aorig = a

        ! 1) Run schur (standard)
        call schur(a,t,z,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (complex(sp))')
        if (allocated(error)) return             

        ! 2) Run schur (overwrite A)
        call schur(a,t,z,overwrite_a=.true.,err=state)

        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(aorig,z,t)<=max(rtol*abs(aorig),eps)), &
                          'converged solution (complex(sp) - overwrite A)')
        if (allocated(error)) return            
        
        ! 3) Use working storage
        a = aorig   
        call schur_space(a,lwork,err=state)
         
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        allocate(storage(lwork))         
        call schur(a,t,z,storage=storage,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (complex(sp) - external storage)')
        if (allocated(error)) return            

    contains
    
        pure function schur_error(a,z,t) result(err)
            complex(sp), intent(in), dimension(:,:) :: a,z,t
            real(sp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),conjg(transpose(z))) - a)
        end function schur_error
        
    end subroutine test_schur_random_c

    !> Test symmetric matrix (real eigenvalues)
    subroutine test_schur_symmetric_c(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(sp), parameter :: rtol = 1.0e-4_sp
        real(sp), parameter :: eps  = sqrt(epsilon(0.0_sp))
        real(sp) :: reigs(n)
        complex(sp), dimension(n,n) :: a, t, z
        type(linalg_state_type) :: state

        ! Define a symmetric 3x3 matrix with real eigenvalues
        a = reshape([ 3, 1, 0, &
                      1, 3, 1, &
                      0, 1, 3], shape=[n, n])

        ! Return real eigenvalues (Should trigger an error if they have an imaginary part)
        call schur(a, t, z, eigvals=reigs, err=state)
        
        ! Check return code
        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        ! Check solution
        call check(error, all(schur_error(a, z, t) <= max(rtol * abs(a), eps)), &
                          'converged solution (real symmetric, real eigs)')
        if (allocated(error)) return

    contains
    
        pure function schur_error(a,z,t) result(err)
            complex(sp), intent(in), dimension(:,:) :: a,z,t
            real(sp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),conjg(transpose(z))) - a)
        end function schur_error        

    end subroutine test_schur_symmetric_c

    subroutine test_schur_api_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n   = 15_ilp
        integer(ilp) :: lwork
        complex(dp) :: eigs(n)
        complex(dp), dimension(n,n) :: a,t,z
        complex(dp), allocatable :: storage(:)
        real(dp) :: rea(n,n),ima(n,n)
        type(linalg_state_type) :: state
        
        call random_number(rea)
        call random_number(ima)
        a = cmplx(rea,ima,kind=dp)
        
        ! Test simple API
        call schur(a,t,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test output transformation matrix
        call schur(a,t,z,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      

        ! Test output eigenvalues
        call schur(a,t,eigvals=eigs,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test storage query
        call schur_space(a,lwork,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
        ! Test with user-defined storage
        allocate(storage(lwork))        
        call schur(a,t,eigvals=eigs,storage=storage,err=state)
        call check(error,state%ok(),state%print())
        if (allocated(error)) return      
        
    end subroutine test_schur_api_z    
    
    subroutine test_schur_random_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: rtol = 1.0e-4_dp
        real(dp), parameter :: eps  = sqrt(epsilon(0.0_dp))
        integer(ilp) :: lwork
        complex(dp), allocatable :: storage(:)
        complex(dp), dimension(n,n) :: a,t,z,aorig
        real(dp), dimension(n,n) :: a_re,a_im
        type(linalg_state_type) :: state

        call random_number(a_re)
        call random_number(a_im)
        a = cmplx(a_re,a_im,kind=dp)
        aorig = a

        ! 1) Run schur (standard)
        call schur(a,t,z,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (complex(dp))')
        if (allocated(error)) return             

        ! 2) Run schur (overwrite A)
        call schur(a,t,z,overwrite_a=.true.,err=state)

        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(aorig,z,t)<=max(rtol*abs(aorig),eps)), &
                          'converged solution (complex(dp) - overwrite A)')
        if (allocated(error)) return            
        
        ! 3) Use working storage
        a = aorig   
        call schur_space(a,lwork,err=state)
         
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
        allocate(storage(lwork))         
        call schur(a,t,z,storage=storage,err=state)
        
        ! Check return code
        call check(error,state%ok(),state%print())
        if (allocated(error)) return            
        
        ! Check solution
        call check(error, all(schur_error(a,z,t)<=max(rtol*abs(a),eps)), &
                          'converged solution (complex(dp) - external storage)')
        if (allocated(error)) return            

    contains
    
        pure function schur_error(a,z,t) result(err)
            complex(dp), intent(in), dimension(:,:) :: a,z,t
            real(dp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),conjg(transpose(z))) - a)
        end function schur_error
        
    end subroutine test_schur_random_z

    !> Test symmetric matrix (real eigenvalues)
    subroutine test_schur_symmetric_z(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp), parameter :: n = 3_ilp
        real(dp), parameter :: rtol = 1.0e-4_dp
        real(dp), parameter :: eps  = sqrt(epsilon(0.0_dp))
        real(dp) :: reigs(n)
        complex(dp), dimension(n,n) :: a, t, z
        type(linalg_state_type) :: state

        ! Define a symmetric 3x3 matrix with real eigenvalues
        a = reshape([ 3, 1, 0, &
                      1, 3, 1, &
                      0, 1, 3], shape=[n, n])

        ! Return real eigenvalues (Should trigger an error if they have an imaginary part)
        call schur(a, t, z, eigvals=reigs, err=state)
        
        ! Check return code
        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        ! Check solution
        call check(error, all(schur_error(a, z, t) <= max(rtol * abs(a), eps)), &
                          'converged solution (real symmetric, real eigs)')
        if (allocated(error)) return

    contains
    
        pure function schur_error(a,z,t) result(err)
            complex(dp), intent(in), dimension(:,:) :: a,z,t
            real(dp), dimension(size(a,1),size(a,2)) :: err
            
            err = abs(matmul(matmul(z,t),conjg(transpose(z))) - a)
        end function schur_error        

    end subroutine test_schur_symmetric_z


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

end module test_linalg_schur

program test_schur
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_schur, only : test_schur_decomposition
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("linalg_schur", test_schur_decomposition) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_schur
