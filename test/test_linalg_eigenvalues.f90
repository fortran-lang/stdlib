! Test eigenvalues and eigendecompositions
module test_linalg_eigenvalues
    use stdlib_linalg_constants
    use stdlib_linalg_state
    use stdlib_linalg, only: eig, eigh, eigvals, eigvalsh, diag, eye
    use testdrive, only: error_type, check, new_unittest, unittest_type    

    implicit none (type,external)
    private
    
    public :: test_eig_eigh

    contains

    !> SVD tests
    subroutine test_eig_eigh(tests)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: tests(:)

        allocate(tests(0))

        call add_test(tests,new_unittest("test_eig_real_s",test_eig_real_s))
        call add_test(tests,new_unittest("test_eigvals_identity_s",test_eigvals_identity_s))
        call add_test(tests,new_unittest("test_eigvals_diagonal_B_s",test_eigvals_diagonal_B_s))
        call add_test(tests,new_unittest("test_eigvals_nondiagonal_B_s",test_eigvals_nondiagonal_B_s))
        call add_test(tests,new_unittest("test_eigh_real_s",test_eigh_real_s))
        call add_test(tests,new_unittest("test_eig_real_d",test_eig_real_d))
        call add_test(tests,new_unittest("test_eigvals_identity_d",test_eigvals_identity_d))
        call add_test(tests,new_unittest("test_eigvals_diagonal_B_d",test_eigvals_diagonal_B_d))
        call add_test(tests,new_unittest("test_eigvals_nondiagonal_B_d",test_eigvals_nondiagonal_B_d))
        call add_test(tests,new_unittest("test_eigh_real_d",test_eigh_real_d))
        
        call add_test(tests,new_unittest("test_eig_complex_c",test_eig_complex_c))
        call add_test(tests,new_unittest("test_eig_generalized_complex_c",test_eigvals_generalized_complex_c))
        call add_test(tests,new_unittest("test_eig_issue_927_c",test_issue_927_c))
        call add_test(tests,new_unittest("test_eig_complex_z",test_eig_complex_z))
        call add_test(tests,new_unittest("test_eig_generalized_complex_z",test_eigvals_generalized_complex_z))
        call add_test(tests,new_unittest("test_eig_issue_927_z",test_issue_927_z))

    end subroutine test_eig_eigh

    !> Simple real matrix eigenvalues
    subroutine test_eig_real_s(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(sp), parameter :: zero    = 0.0_sp
        real(sp), parameter :: two     = 2.0_sp
        real(sp), parameter :: sqrt2o2 = sqrt(two)*0.5_sp
        real(sp), parameter :: tol     = sqrt(epsilon(zero))

        !> Local variables
        type(linalg_state_type) :: state
        real(sp) :: A(3,3),B(2,2)
        complex(sp) :: lambda(3),Bvec(2,2),Bres(2,2)

        !> Matrix with real eigenvalues
        A = reshape([1,0,0, &
                     0,2,0, &
                     0,0,3],[3,3])
        
        call eig(A,lambda,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(aimag(lambda)==zero.and.real(lambda,kind=sp)==[1,2,3]),'expected results')
        if (allocated(error)) return        
        
        !> Matrix with complex eigenvalues
        B = transpose(reshape([1, -1, &
                               1,  1],[2,2]))
                               
        !> Expected right eigenvectors
        Bres(1,1:2) = sqrt2o2
        Bres(2,1)   = cmplx(zero,-sqrt2o2,kind=sp)
        Bres(2,2)   = cmplx(zero,+sqrt2o2,kind=sp)
        
        call eig(B,lambda,right=Bvec,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(Bres-Bvec)<=tol),'expected results')
        if (allocated(error)) return        
                
    end subroutine test_eig_real_s

    ! Symmetric matrix eigenvalues
    subroutine test_eigh_real_s(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(sp), parameter :: zero   = 0.0_sp
        real(sp), parameter :: tol    = sqrt(epsilon(zero))
        real(sp), parameter :: A(4,4) = reshape([6,3,1,5, &
                                               3,0,5,1, &
                                               1,5,6,2, &
                                               5,1,2,2],[4,4])
        
        !> Local variables
        real(sp) :: Amat(4,4),lambda(4),vect(4,4),Av(4,4),lv(4,4)
        type(linalg_state_type) :: state
        
        Amat = A
        
        call eigh(Amat,lambda,vect,err=state)
        
        Av = matmul(A,vect)
        lv = matmul(vect,diag(lambda))
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(Av-lv)<=tol*abs(Av)),'expected results')
        if (allocated(error)) return    
        
        !> Test functional versions: no state interface
        lambda = eigvalsh(Amat)
   
        !> State interface
        lambda = eigvalsh(Amat,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        !> Functional version, lower A
        Amat = A
        lambda = eigvalsh(Amat,upper_a=.false.,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
    end subroutine test_eigh_real_s

    !> Test generalized eigenvalue problem with B = identity
    subroutine test_eigvals_identity_s(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(sp), parameter :: zero = 0.0_sp
        real(sp), parameter :: tol = sqrt(epsilon(zero))

        !> Local variables
        type(linalg_state_type) :: state
        real(sp) :: A(3, 3), B(3, 3)
        complex(sp) :: lambda(3)

        !> Matrix A
        A = reshape([3, 0, 0, &
                     0, 5, 0, &
                     0, 0, 7], [3, 3])

        !> Identity matrix B
        B = reshape([1, 0, 0, &
                     0, 1, 0, &
                     0, 0, 1], [3, 3])

        !> Generalized problem
        lambda = eigvals(A, B, err=state)

        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(real(lambda,sp) - [3, 5, 7]) <= tol), &
                          'expected results for B=identity')
        if (allocated(error)) return
    end subroutine test_eigvals_identity_s

    !> Test generalized eigenvalue problem with B = diagonal
    subroutine test_eigvals_diagonal_B_s(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(sp), parameter :: zero = 0.0_sp
        real(sp), parameter :: tol = sqrt(epsilon(zero))

        !> Local variables
        type(linalg_state_type) :: state
        real(sp) :: A(3, 3), B(3, 3)
        complex(sp) :: lambda(3)

        !> Matrix A
        A = reshape([3, 0, 0, &
                     0, 5, 0, &
                     0, 0, 7], [3, 3])

        !> Diagonal matrix B
        B = reshape([2, 0, 0, &
                     0, 4, 0, &
                     0, 0, 8], [3, 3])

        lambda = eigvals(A, B, err=state)

        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(real(lambda,sp) - [1.5_sp, 1.25_sp, 0.875_sp]) <= tol),&
                              'expected results for B=diagonal')
        if (allocated(error)) return
        
    end subroutine test_eigvals_diagonal_B_s

    !> Test generalized eigenvalue problem with B = non-diagonal
    subroutine test_eigvals_nondiagonal_B_s(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(sp), parameter :: zero = 0.0_sp
        real(sp), parameter :: tol = 1.0e-3_sp

        !> Local variables
        type(linalg_state_type) :: state
        real(sp) :: A(3, 3), B(3, 3)
        complex(sp) :: lambda(3)

        !> Matrix A
        A = reshape([3, 2, 0, &
                     2, 5, 1, &
                     0, 1, 7], [3, 3])

        !> Non-diagonal matrix B
        B = reshape([2, 1, 0, &
                     1, 3, 0, &
                     0, 0, 4], [3, 3])

        lambda = eigvals(A, B, err=state)

        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(lambda - [1.1734_sp, 1.5766_sp, 2.0000_sp]) <= tol), 'expected results for B=nondiagonal')
        
        print *, 'lambda ',lambda
        print *, 'expected ',[1.0,2.5,3.75]
        
        if (allocated(error)) return
    end subroutine test_eigvals_nondiagonal_B_s

    subroutine test_eig_real_d(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(dp), parameter :: zero    = 0.0_dp
        real(dp), parameter :: two     = 2.0_dp
        real(dp), parameter :: sqrt2o2 = sqrt(two)*0.5_dp
        real(dp), parameter :: tol     = sqrt(epsilon(zero))

        !> Local variables
        type(linalg_state_type) :: state
        real(dp) :: A(3,3),B(2,2)
        complex(dp) :: lambda(3),Bvec(2,2),Bres(2,2)

        !> Matrix with real eigenvalues
        A = reshape([1,0,0, &
                     0,2,0, &
                     0,0,3],[3,3])
        
        call eig(A,lambda,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(aimag(lambda)==zero.and.real(lambda,kind=dp)==[1,2,3]),'expected results')
        if (allocated(error)) return        
        
        !> Matrix with complex eigenvalues
        B = transpose(reshape([1, -1, &
                               1,  1],[2,2]))
                               
        !> Expected right eigenvectors
        Bres(1,1:2) = sqrt2o2
        Bres(2,1)   = cmplx(zero,-sqrt2o2,kind=dp)
        Bres(2,2)   = cmplx(zero,+sqrt2o2,kind=dp)
        
        call eig(B,lambda,right=Bvec,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(Bres-Bvec)<=tol),'expected results')
        if (allocated(error)) return        
                
    end subroutine test_eig_real_d

    ! Symmetric matrix eigenvalues
    subroutine test_eigh_real_d(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(dp), parameter :: zero   = 0.0_dp
        real(dp), parameter :: tol    = sqrt(epsilon(zero))
        real(dp), parameter :: A(4,4) = reshape([6,3,1,5, &
                                               3,0,5,1, &
                                               1,5,6,2, &
                                               5,1,2,2],[4,4])
        
        !> Local variables
        real(dp) :: Amat(4,4),lambda(4),vect(4,4),Av(4,4),lv(4,4)
        type(linalg_state_type) :: state
        
        Amat = A
        
        call eigh(Amat,lambda,vect,err=state)
        
        Av = matmul(A,vect)
        lv = matmul(vect,diag(lambda))
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(Av-lv)<=tol*abs(Av)),'expected results')
        if (allocated(error)) return    
        
        !> Test functional versions: no state interface
        lambda = eigvalsh(Amat)
   
        !> State interface
        lambda = eigvalsh(Amat,err=state)
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        !> Functional version, lower A
        Amat = A
        lambda = eigvalsh(Amat,upper_a=.false.,err=state)

        call check(error,state%ok(),state%print())
        if (allocated(error)) return
        
    end subroutine test_eigh_real_d

    !> Test generalized eigenvalue problem with B = identity
    subroutine test_eigvals_identity_d(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(dp), parameter :: zero = 0.0_dp
        real(dp), parameter :: tol = sqrt(epsilon(zero))

        !> Local variables
        type(linalg_state_type) :: state
        real(dp) :: A(3, 3), B(3, 3)
        complex(dp) :: lambda(3)

        !> Matrix A
        A = reshape([3, 0, 0, &
                     0, 5, 0, &
                     0, 0, 7], [3, 3])

        !> Identity matrix B
        B = reshape([1, 0, 0, &
                     0, 1, 0, &
                     0, 0, 1], [3, 3])

        !> Generalized problem
        lambda = eigvals(A, B, err=state)

        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(real(lambda,dp) - [3, 5, 7]) <= tol), &
                          'expected results for B=identity')
        if (allocated(error)) return
    end subroutine test_eigvals_identity_d

    !> Test generalized eigenvalue problem with B = diagonal
    subroutine test_eigvals_diagonal_B_d(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(dp), parameter :: zero = 0.0_dp
        real(dp), parameter :: tol = sqrt(epsilon(zero))

        !> Local variables
        type(linalg_state_type) :: state
        real(dp) :: A(3, 3), B(3, 3)
        complex(dp) :: lambda(3)

        !> Matrix A
        A = reshape([3, 0, 0, &
                     0, 5, 0, &
                     0, 0, 7], [3, 3])

        !> Diagonal matrix B
        B = reshape([2, 0, 0, &
                     0, 4, 0, &
                     0, 0, 8], [3, 3])

        lambda = eigvals(A, B, err=state)

        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(real(lambda,dp) - [1.5_dp, 1.25_dp, 0.875_dp]) <= tol),&
                              'expected results for B=diagonal')
        if (allocated(error)) return
        
    end subroutine test_eigvals_diagonal_B_d

    !> Test generalized eigenvalue problem with B = non-diagonal
    subroutine test_eigvals_nondiagonal_B_d(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(dp), parameter :: zero = 0.0_dp
        real(dp), parameter :: tol = 1.0e-3_dp

        !> Local variables
        type(linalg_state_type) :: state
        real(dp) :: A(3, 3), B(3, 3)
        complex(dp) :: lambda(3)

        !> Matrix A
        A = reshape([3, 2, 0, &
                     2, 5, 1, &
                     0, 1, 7], [3, 3])

        !> Non-diagonal matrix B
        B = reshape([2, 1, 0, &
                     1, 3, 0, &
                     0, 0, 4], [3, 3])

        lambda = eigvals(A, B, err=state)

        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(lambda - [1.1734_dp, 1.5766_dp, 2.0000_dp]) <= tol), 'expected results for B=nondiagonal')
        
        print *, 'lambda ',lambda
        print *, 'expected ',[1.0,2.5,3.75]
        
        if (allocated(error)) return
    end subroutine test_eigvals_nondiagonal_B_d


    !> Simple complex matrix eigenvalues
    subroutine test_eig_complex_c(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(sp), parameter :: zero    = 0.0_sp
        real(sp), parameter :: two     = 2.0_sp
        real(sp), parameter :: sqrt2o2 = sqrt(two)*0.5_sp
        real(sp), parameter :: tol     = sqrt(epsilon(zero))
        complex(sp), parameter :: cone  = (1.0_sp,0.0_sp)
        complex(sp), parameter :: cimg  = (0.0_sp,1.0_sp)
        complex(sp), parameter :: czero = (0.0_sp,0.0_sp)

        !> Local vaciables
        type(linalg_state_type) :: state
        complex(sp) :: A(2,2),lambda(2),Avec(2,2),Ares(2,2),lres(2)

        !> Matcix with real eigenvalues
        A = transpose(reshape([ cone, cimg, &
                               -cimg, cone], [2,2]))
                
        call eig(A,lambda,right=Avec,err=state)
        
        !> Expected eigenvalues and eigenvectors
        lres(1)   = two
        lres(2)   = zero
        
        !> Eigenvectors may vary: do not use for error
        Ares(1,1) = cmplx(zero,sqrt2o2,kind=sp)
        Ares(1,2) = cmplx(sqrt2o2,zero,kind=sp)
        Ares(2,1) = cmplx(sqrt2o2,zero,kind=sp)
        Ares(2,2) = cmplx(zero,sqrt2o2,kind=sp)        
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(lambda-lres)<=tol), 'results match expected')
        if (allocated(error)) return        
        
    end subroutine test_eig_complex_c

    !> Complex generalized eigenvalue problem with eigvals
    subroutine test_eigvals_generalized_complex_c(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(sp), parameter :: zero    = 0.0_sp
        real(sp), parameter :: one     = 1.0_sp
        real(sp), parameter :: tol     = sqrt(epsilon(zero))
        complex(sp), parameter :: cone  = (one, zero)
        complex(sp), parameter :: cimg  = (zero, one)
        complex(sp), parameter :: czero = (zero, zero)

        !> Local variables
        type(linalg_state_type) :: state
        complex(sp) :: A(2,2), B(2,2), lambda(2), lres(2)

        !> Matrices A and B for the generalized problem A * x = lambda * B * x
        A = transpose(reshape([ cone, cimg, &
                               -cimg, cone], [2,2]))
        B = transpose(reshape([ cone, czero, &
                                czero, cone], [2,2]))
        
        lambda = eigvals(A, B, err=state)
        
        !> Expected eigenvalues
        lres(1) = czero
        lres(2) = 2*cone
        
        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(lambda - lres) <= tol) .or. &
                          all(abs(lambda - lres([2,1])) <= tol), 'results match expected')
        if (allocated(error)) return        
        
    end subroutine test_eigvals_generalized_complex_c

    ! Generalized eigenvalues should not crash
    subroutine test_issue_927_c(error)
      type(error_type), allocatable, intent(out) :: error 

      complex(sp) :: A_Z(3,3),S_Z(3,3),vecs_r(3,3),eigs(3)
      real(sp) :: A_D(3,3),S_D(3,3)
      type(linalg_state_type) :: state
      integer :: i

      ! Set matrix
      A_Z = reshape( [ [1, 6, 3], &
                        [9, 2, 1], &
                        [8, 3, 4] ], [3,3] )

      S_Z = eye(3, mold=0.0_sp)

      A_D = real(A_Z)
      S_D = real(S_Z)

      call eig(A_D,S_D,eigs,right=vecs_r,err=state) 
      call check(error, state%ok(), 'test issue 927 (complex(sp)): '//state%print())
      if (allocated(error)) return      

      call eig(A_Z,S_Z,eigs,right=vecs_r,err=state) !Fails
      call check(error, state%ok(), 'test issue 927 (complex(sp)): '//state%print())
      if (allocated(error)) return      

    end subroutine test_issue_927_c

    subroutine test_eig_complex_z(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(dp), parameter :: zero    = 0.0_dp
        real(dp), parameter :: two     = 2.0_dp
        real(dp), parameter :: sqrt2o2 = sqrt(two)*0.5_dp
        real(dp), parameter :: tol     = sqrt(epsilon(zero))
        complex(dp), parameter :: cone  = (1.0_dp,0.0_dp)
        complex(dp), parameter :: cimg  = (0.0_dp,1.0_dp)
        complex(dp), parameter :: czero = (0.0_dp,0.0_dp)

        !> Local vaciables
        type(linalg_state_type) :: state
        complex(dp) :: A(2,2),lambda(2),Avec(2,2),Ares(2,2),lres(2)

        !> Matcix with real eigenvalues
        A = transpose(reshape([ cone, cimg, &
                               -cimg, cone], [2,2]))
                
        call eig(A,lambda,right=Avec,err=state)
        
        !> Expected eigenvalues and eigenvectors
        lres(1)   = two
        lres(2)   = zero
        
        !> Eigenvectors may vary: do not use for error
        Ares(1,1) = cmplx(zero,sqrt2o2,kind=dp)
        Ares(1,2) = cmplx(sqrt2o2,zero,kind=dp)
        Ares(2,1) = cmplx(sqrt2o2,zero,kind=dp)
        Ares(2,2) = cmplx(zero,sqrt2o2,kind=dp)        
        
        call check(error,state%ok(),state%print())
        if (allocated(error)) return

        call check(error, all(abs(lambda-lres)<=tol), 'results match expected')
        if (allocated(error)) return        
        
    end subroutine test_eig_complex_z

    !> Complex generalized eigenvalue problem with eigvals
    subroutine test_eigvals_generalized_complex_z(error)
        type(error_type), allocatable, intent(out) :: error

        !> Reference solution
        real(dp), parameter :: zero    = 0.0_dp
        real(dp), parameter :: one     = 1.0_dp
        real(dp), parameter :: tol     = sqrt(epsilon(zero))
        complex(dp), parameter :: cone  = (one, zero)
        complex(dp), parameter :: cimg  = (zero, one)
        complex(dp), parameter :: czero = (zero, zero)

        !> Local variables
        type(linalg_state_type) :: state
        complex(dp) :: A(2,2), B(2,2), lambda(2), lres(2)

        !> Matrices A and B for the generalized problem A * x = lambda * B * x
        A = transpose(reshape([ cone, cimg, &
                               -cimg, cone], [2,2]))
        B = transpose(reshape([ cone, czero, &
                                czero, cone], [2,2]))
        
        lambda = eigvals(A, B, err=state)
        
        !> Expected eigenvalues
        lres(1) = czero
        lres(2) = 2*cone
        
        call check(error, state%ok(), state%print())
        if (allocated(error)) return

        call check(error, all(abs(lambda - lres) <= tol) .or. &
                          all(abs(lambda - lres([2,1])) <= tol), 'results match expected')
        if (allocated(error)) return        
        
    end subroutine test_eigvals_generalized_complex_z

    ! Generalized eigenvalues should not crash
    subroutine test_issue_927_z(error)
      type(error_type), allocatable, intent(out) :: error 

      complex(dp) :: A_Z(3,3),S_Z(3,3),vecs_r(3,3),eigs(3)
      real(dp) :: A_D(3,3),S_D(3,3)
      type(linalg_state_type) :: state
      integer :: i

      ! Set matrix
      A_Z = reshape( [ [1, 6, 3], &
                        [9, 2, 1], &
                        [8, 3, 4] ], [3,3] )

      S_Z = eye(3, mold=0.0_dp)

      A_D = real(A_Z)
      S_D = real(S_Z)

      call eig(A_D,S_D,eigs,right=vecs_r,err=state) 
      call check(error, state%ok(), 'test issue 927 (complex(dp)): '//state%print())
      if (allocated(error)) return      

      call eig(A_Z,S_Z,eigs,right=vecs_r,err=state) !Fails
      call check(error, state%ok(), 'test issue 927 (complex(dp)): '//state%print())
      if (allocated(error)) return      

    end subroutine test_issue_927_z


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

end module test_linalg_eigenvalues

program test_eigenvalues
     use, intrinsic :: iso_fortran_env, only : error_unit
     use testdrive, only : run_testsuite, new_testsuite, testsuite_type
     use test_linalg_eigenvalues, only : test_eig_eigh
     implicit none
     integer :: stat, is
     type(testsuite_type), allocatable :: testsuites(:)
     character(len=*), parameter :: fmt = '("#", *(1x, a))'

     stat = 0

     testsuites = [ &
         new_testsuite("linalg_eigenvalues", test_eig_eigh) &
         ]

     do is = 1, size(testsuites)
         write(error_unit, fmt) "Testing:", testsuites(is)%name
         call run_testsuite(testsuites(is)%collect, error_unit, stat)
     end do

     if (stat > 0) then
         write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
         error stop
     end if
end program test_eigenvalues
