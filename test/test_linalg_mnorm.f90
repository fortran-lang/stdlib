module test_linalg_mnorm
    use testdrive, only: error_type, check, new_unittest, unittest_type
    use stdlib_linalg_constants
    use stdlib_linalg, only: mnorm, linalg_state_type
    use stdlib_linalg_state, only: linalg_state_type

    implicit none (type,external)

    contains

    !> Matrix norm tests
    subroutine test_matrix_norms(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)
        
        allocate(tests(0))
        
        call add_test(tests,new_unittest("test_matrix_norms_s",test_matrix_norms_s))
        call add_test(tests,new_unittest("test_mnorm_s_3d",test_mnorm_s_3d))
        call add_test(tests,new_unittest("test_matrix_norms_d",test_matrix_norms_d))
        call add_test(tests,new_unittest("test_mnorm_d_3d",test_mnorm_d_3d))
        call add_test(tests,new_unittest("test_matrix_norms_c",test_matrix_norms_c))
        call add_test(tests,new_unittest("test_mnorm_c_3d",test_mnorm_c_3d))
        call add_test(tests,new_unittest("test_matrix_norms_z",test_matrix_norms_z))
        call add_test(tests,new_unittest("test_mnorm_z_3d",test_mnorm_z_3d))
        
    end subroutine test_matrix_norms
    
    !> Test 1-norm, 2-norm (Euclidean), and infinity norm for real(sp) matrices
    subroutine test_matrix_norms_s(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp) :: i
        integer(ilp), parameter :: mtx_dim = 5
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp), allocatable :: A(:,:)
        type(linalg_state_type) :: err

        allocate(A(mtx_dim,mtx_dim))

        ! Initialize matrix with small values to avoid overflow
        A = reshape([(0.01_sp*(i-mtx_dim/2_ilp), i=1_ilp,mtx_dim*mtx_dim)], [mtx_dim,mtx_dim])

        ! 1-norm (Maximum absolute column sum)
        call check(error, abs(mnorm(A, '1', err) - maxval(sum(abs(A), dim=1),1)) < tol*mnorm(A, '1', err), &
                   'Matrix 1-norm does not match expected value')
        if (allocated(error)) return

        ! 2-norm (Frobenius norm)
        call check(error, abs(mnorm(A, err=err) - sqrt(sum(A**2))) < tol*mnorm(A, err=err), &
                   'Matrix Frobenius norm does not match expected value')
        if (allocated(error)) return

        ! Inf-norm (Maximum absolute row sum)
        call check(error, abs(mnorm(A, 'Inf', err) -  maxval(sum(abs(A), dim=2),1)) < tol*mnorm(A, 'Inf', err), &
                   'Matrix Infinity norm does not match expected value')
        if (allocated(error)) return

    end subroutine test_matrix_norms_s
    
    !> Test N-D norms
    subroutine test_mnorm_s_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: i,j,k,l,dim1,dim2,dim(2),dim_sizes(2),ptr(3)
        character(3), parameter :: orders(*) = ['1  ','2  ','fro','inf']
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n    = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim1, dim1=1,ndim)]
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp) :: one_nrm
        real(sp), allocatable :: bnrm(:)
        real(sp), allocatable :: a(:), b(:,:,:), one_mat(:,:)
        character(:), allocatable :: order
        
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test norm as collapsed around dimensions
        do k = 1, size(orders)
            order = trim(orders(k))
            do dim1 = 1, ndim
                do dim2 = 1, ndim
                    
                    if (dim1==dim2) cycle
                                    
                    dim       = [dim1,dim2]
                    dim_sizes = [size(b,dim1,kind=ilp),size(b,dim2,kind=ilp)]
                    
                    ! Get norms collapsed on these dims
                    bnrm = mnorm(b,order,dim)
                                   
                    ! Assert size
                    write(msg,"('dim=[',i0,',',i0,'] order=',a,' sp norm returned wrong shape')") dim, order
                    call check(error,all(shape(bnrm)==pack(shape(b),dims/=dim1 .and. dims/=dim2) ), trim(msg))
                    if (allocated(error)) return    
                    
                    ! Assert some matrix results: check that those on same index i.e. (l,l,l,:,l,l,:) etc.
                    ! are equal to the corresponding 2d-array result
                    do l = 1, minval(shape(b))
                    
                       ptr = l
                    
                       allocate(one_mat(dim_sizes(1),dim_sizes(2)))
                       do j = 1, dim_sizes(2)
                           ptr(dim(2)) = j
                           do i = 1, dim_sizes(1)
                               ptr(dim(1)) = i
                               one_mat(i,j) = b(ptr(1), ptr(2), ptr(3)) 
                           end do
                       end do
                       one_nrm = mnorm(one_mat,order)
                    
                       write(msg,"('dim=[',i0,',',i0,'] order=',a,' sp ',i0,'-th norm is wrong')") dim, order, l
                       call check(error, abs(one_nrm-bnrm((l)))<tol*one_nrm, trim(msg))
                       if (allocated(error)) return    
                       deallocate(one_mat) 
                    
                    end do
                                   
                end do
            end do
        end do
        
    end subroutine test_mnorm_s_3d
    
    !> Test 1-norm, 2-norm (Euclidean), and infinity norm for real(dp) matrices
    subroutine test_matrix_norms_d(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp) :: i
        integer(ilp), parameter :: mtx_dim = 5
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp), allocatable :: A(:,:)
        type(linalg_state_type) :: err

        allocate(A(mtx_dim,mtx_dim))

        ! Initialize matrix with small values to avoid overflow
        A = reshape([(0.01_dp*(i-mtx_dim/2_ilp), i=1_ilp,mtx_dim*mtx_dim)], [mtx_dim,mtx_dim])

        ! 1-norm (Maximum absolute column sum)
        call check(error, abs(mnorm(A, '1', err) - maxval(sum(abs(A), dim=1),1)) < tol*mnorm(A, '1', err), &
                   'Matrix 1-norm does not match expected value')
        if (allocated(error)) return

        ! 2-norm (Frobenius norm)
        call check(error, abs(mnorm(A, err=err) - sqrt(sum(A**2))) < tol*mnorm(A, err=err), &
                   'Matrix Frobenius norm does not match expected value')
        if (allocated(error)) return

        ! Inf-norm (Maximum absolute row sum)
        call check(error, abs(mnorm(A, 'Inf', err) -  maxval(sum(abs(A), dim=2),1)) < tol*mnorm(A, 'Inf', err), &
                   'Matrix Infinity norm does not match expected value')
        if (allocated(error)) return

    end subroutine test_matrix_norms_d
    
    !> Test N-D norms
    subroutine test_mnorm_d_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: i,j,k,l,dim1,dim2,dim(2),dim_sizes(2),ptr(3)
        character(3), parameter :: orders(*) = ['1  ','2  ','fro','inf']
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n    = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim1, dim1=1,ndim)]
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp) :: one_nrm
        real(dp), allocatable :: bnrm(:)
        real(dp), allocatable :: a(:), b(:,:,:), one_mat(:,:)
        character(:), allocatable :: order
        
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test norm as collapsed around dimensions
        do k = 1, size(orders)
            order = trim(orders(k))
            do dim1 = 1, ndim
                do dim2 = 1, ndim
                    
                    if (dim1==dim2) cycle
                                    
                    dim       = [dim1,dim2]
                    dim_sizes = [size(b,dim1,kind=ilp),size(b,dim2,kind=ilp)]
                    
                    ! Get norms collapsed on these dims
                    bnrm = mnorm(b,order,dim)
                                   
                    ! Assert size
                    write(msg,"('dim=[',i0,',',i0,'] order=',a,' dp norm returned wrong shape')") dim, order
                    call check(error,all(shape(bnrm)==pack(shape(b),dims/=dim1 .and. dims/=dim2) ), trim(msg))
                    if (allocated(error)) return    
                    
                    ! Assert some matrix results: check that those on same index i.e. (l,l,l,:,l,l,:) etc.
                    ! are equal to the corresponding 2d-array result
                    do l = 1, minval(shape(b))
                    
                       ptr = l
                    
                       allocate(one_mat(dim_sizes(1),dim_sizes(2)))
                       do j = 1, dim_sizes(2)
                           ptr(dim(2)) = j
                           do i = 1, dim_sizes(1)
                               ptr(dim(1)) = i
                               one_mat(i,j) = b(ptr(1), ptr(2), ptr(3)) 
                           end do
                       end do
                       one_nrm = mnorm(one_mat,order)
                    
                       write(msg,"('dim=[',i0,',',i0,'] order=',a,' dp ',i0,'-th norm is wrong')") dim, order, l
                       call check(error, abs(one_nrm-bnrm((l)))<tol*one_nrm, trim(msg))
                       if (allocated(error)) return    
                       deallocate(one_mat) 
                    
                    end do
                                   
                end do
            end do
        end do
        
    end subroutine test_mnorm_d_3d
    
    !> Test 1-norm, 2-norm (Euclidean), and infinity norm for complex(sp) matrices
    subroutine test_matrix_norms_c(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp) :: i
        integer(ilp), parameter :: mtx_dim = 5
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        complex(sp), allocatable :: A(:,:)
        type(linalg_state_type) :: err

        allocate(A(mtx_dim,mtx_dim))

        ! Initialize matrix with small values to avoid overflow
        A = reshape([(0.01_sp*(i-mtx_dim/2_ilp), i=1_ilp,mtx_dim*mtx_dim)], [mtx_dim,mtx_dim])

        ! 1-norm (Maximum absolute column sum)
        call check(error, abs(mnorm(A, '1', err) - maxval(sum(abs(A), dim=1),1)) < tol*mnorm(A, '1', err), &
                   'Matrix 1-norm does not match expected value')
        if (allocated(error)) return

        ! 2-norm (Frobenius norm)
        call check(error, abs(mnorm(A, err=err) - sqrt(sum(A**2))) < tol*mnorm(A, err=err), &
                   'Matrix Frobenius norm does not match expected value')
        if (allocated(error)) return

        ! Inf-norm (Maximum absolute row sum)
        call check(error, abs(mnorm(A, 'Inf', err) -  maxval(sum(abs(A), dim=2),1)) < tol*mnorm(A, 'Inf', err), &
                   'Matrix Infinity norm does not match expected value')
        if (allocated(error)) return

    end subroutine test_matrix_norms_c
    
    !> Test N-D norms
    subroutine test_mnorm_c_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: i,j,k,l,dim1,dim2,dim(2),dim_sizes(2),ptr(3)
        character(3), parameter :: orders(*) = ['1  ','2  ','fro','inf']
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n    = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim1, dim1=1,ndim)]
        real(sp), parameter :: tol = 10*sqrt(epsilon(0.0_sp))
        real(sp) :: one_nrm
        real(sp), allocatable :: bnrm(:)
        complex(sp), allocatable :: a(:), b(:,:,:), one_mat(:,:)
        character(:), allocatable :: order
        
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_sp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test norm as collapsed around dimensions
        do k = 1, size(orders)
            order = trim(orders(k))
            do dim1 = 1, ndim
                do dim2 = 1, ndim
                    
                    if (dim1==dim2) cycle
                                    
                    dim       = [dim1,dim2]
                    dim_sizes = [size(b,dim1,kind=ilp),size(b,dim2,kind=ilp)]
                    
                    ! Get norms collapsed on these dims
                    bnrm = mnorm(b,order,dim)
                                   
                    ! Assert size
                    write(msg,"('dim=[',i0,',',i0,'] order=',a,' sp norm returned wrong shape')") dim, order
                    call check(error,all(shape(bnrm)==pack(shape(b),dims/=dim1 .and. dims/=dim2) ), trim(msg))
                    if (allocated(error)) return    
                    
                    ! Assert some matrix results: check that those on same index i.e. (l,l,l,:,l,l,:) etc.
                    ! are equal to the corresponding 2d-array result
                    do l = 1, minval(shape(b))
                    
                       ptr = l
                    
                       allocate(one_mat(dim_sizes(1),dim_sizes(2)))
                       do j = 1, dim_sizes(2)
                           ptr(dim(2)) = j
                           do i = 1, dim_sizes(1)
                               ptr(dim(1)) = i
                               one_mat(i,j) = b(ptr(1), ptr(2), ptr(3)) 
                           end do
                       end do
                       one_nrm = mnorm(one_mat,order)
                    
                       write(msg,"('dim=[',i0,',',i0,'] order=',a,' sp ',i0,'-th norm is wrong')") dim, order, l
                       call check(error, abs(one_nrm-bnrm((l)))<tol*one_nrm, trim(msg))
                       if (allocated(error)) return    
                       deallocate(one_mat) 
                    
                    end do
                                   
                end do
            end do
        end do
        
    end subroutine test_mnorm_c_3d
    
    !> Test 1-norm, 2-norm (Euclidean), and infinity norm for complex(dp) matrices
    subroutine test_matrix_norms_z(error)
        type(error_type), allocatable, intent(out) :: error
        
        integer(ilp) :: i
        integer(ilp), parameter :: mtx_dim = 5
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        complex(dp), allocatable :: A(:,:)
        type(linalg_state_type) :: err

        allocate(A(mtx_dim,mtx_dim))

        ! Initialize matrix with small values to avoid overflow
        A = reshape([(0.01_dp*(i-mtx_dim/2_ilp), i=1_ilp,mtx_dim*mtx_dim)], [mtx_dim,mtx_dim])

        ! 1-norm (Maximum absolute column sum)
        call check(error, abs(mnorm(A, '1', err) - maxval(sum(abs(A), dim=1),1)) < tol*mnorm(A, '1', err), &
                   'Matrix 1-norm does not match expected value')
        if (allocated(error)) return

        ! 2-norm (Frobenius norm)
        call check(error, abs(mnorm(A, err=err) - sqrt(sum(A**2))) < tol*mnorm(A, err=err), &
                   'Matrix Frobenius norm does not match expected value')
        if (allocated(error)) return

        ! Inf-norm (Maximum absolute row sum)
        call check(error, abs(mnorm(A, 'Inf', err) -  maxval(sum(abs(A), dim=2),1)) < tol*mnorm(A, 'Inf', err), &
                   'Matrix Infinity norm does not match expected value')
        if (allocated(error)) return

    end subroutine test_matrix_norms_z
    
    !> Test N-D norms
    subroutine test_mnorm_z_3d(error)
        type(error_type), allocatable, intent(out) :: error

        integer(ilp) :: i,j,k,l,dim1,dim2,dim(2),dim_sizes(2),ptr(3)
        character(3), parameter :: orders(*) = ['1  ','2  ','fro','inf']
        integer(ilp), parameter :: ndim = 3
        integer(ilp), parameter :: n    = 2_ilp**ndim
        integer(ilp), parameter :: dims(*) = [(dim1, dim1=1,ndim)]
        real(dp), parameter :: tol = 10*sqrt(epsilon(0.0_dp))
        real(dp) :: one_nrm
        real(dp), allocatable :: bnrm(:)
        complex(dp), allocatable :: a(:), b(:,:,:), one_mat(:,:)
        character(:), allocatable :: order
        
        character(64) :: msg
        
        allocate(a(n), b(2,2,2))
        
        ! Init as a range,but with small elements such that all power norms will 
        ! never overflow, even in single precision
        a = [(0.01_dp*(j-n/2_ilp), j=1_ilp,n)]        
        b = reshape(a, shape(b))
        
        ! Test norm as collapsed around dimensions
        do k = 1, size(orders)
            order = trim(orders(k))
            do dim1 = 1, ndim
                do dim2 = 1, ndim
                    
                    if (dim1==dim2) cycle
                                    
                    dim       = [dim1,dim2]
                    dim_sizes = [size(b,dim1,kind=ilp),size(b,dim2,kind=ilp)]
                    
                    ! Get norms collapsed on these dims
                    bnrm = mnorm(b,order,dim)
                                   
                    ! Assert size
                    write(msg,"('dim=[',i0,',',i0,'] order=',a,' dp norm returned wrong shape')") dim, order
                    call check(error,all(shape(bnrm)==pack(shape(b),dims/=dim1 .and. dims/=dim2) ), trim(msg))
                    if (allocated(error)) return    
                    
                    ! Assert some matrix results: check that those on same index i.e. (l,l,l,:,l,l,:) etc.
                    ! are equal to the corresponding 2d-array result
                    do l = 1, minval(shape(b))
                    
                       ptr = l
                    
                       allocate(one_mat(dim_sizes(1),dim_sizes(2)))
                       do j = 1, dim_sizes(2)
                           ptr(dim(2)) = j
                           do i = 1, dim_sizes(1)
                               ptr(dim(1)) = i
                               one_mat(i,j) = b(ptr(1), ptr(2), ptr(3)) 
                           end do
                       end do
                       one_nrm = mnorm(one_mat,order)
                    
                       write(msg,"('dim=[',i0,',',i0,'] order=',a,' dp ',i0,'-th norm is wrong')") dim, order, l
                       call check(error, abs(one_nrm-bnrm((l)))<tol*one_nrm, trim(msg))
                       if (allocated(error)) return    
                       deallocate(one_mat) 
                    
                    end do
                                   
                end do
            end do
        end do
        
    end subroutine test_mnorm_z_3d
    

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

end module test_linalg_mnorm

program test_mnorm
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type
    use test_linalg_mnorm, only : test_matrix_norms
    implicit none
    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("matrix_norms", test_matrix_norms) &
        ]

    do is = 1, size(testsuites)
        write(error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program test_mnorm
