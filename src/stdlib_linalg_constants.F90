module stdlib_linalg_constants
     use stdlib_kinds, only: sp, dp, xdp, qp, int32, int64, lk
     use, intrinsic :: ieee_arithmetic, only: ieee_is_nan 
     !$ use omp_lib
     implicit none
     public

     ! Checks whether BLAS is provided by an external library
#ifdef STDLIB_EXTERNAL_BLAS     
     logical(lk), parameter :: external_blas_ilp32 = .true._lk
#else
     logical(lk), parameter :: external_blas_ilp32 = .false._lk
#endif   
#ifdef STDLIB_EXTERNAL_BLAS_I64    
     logical(lk), parameter :: external_blas_ilp64 = .true._lk
#else
     logical(lk), parameter :: external_blas_ilp64 = .false._lk
#endif   

#ifdef STDLIB_EXTERNAL_LAPACK
     logical(lk), parameter :: external_lapack_ilp32 = .true._lk
#else
     logical(lk), parameter :: external_lapack_ilp32 = .false._lk
#endif   
#ifdef STDLIB_EXTERNAL_LAPACK_I64    
     logical(lk), parameter :: external_lapack_ilp64 = .true._lk
#else
     logical(lk), parameter :: external_lapack_ilp64 = .false._lk
#endif   

     ! Generic checks for external libraries
     logical(lk), parameter :: external_blas   = external_blas_ilp32   .or. external_blas_ilp64
     logical(lk), parameter :: external_lapack = external_lapack_ilp32 .or. external_lapack_ilp64

     ! Support both 32-bit (ilp) and 64-bit (ilp64) integer kinds
     integer, parameter :: ilp   = int32
     integer, parameter :: ilp64 =  -1 
     private            :: int32, int64
     
end module stdlib_linalg_constants
