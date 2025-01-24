module stdlib_blas_constants_sp
    use stdlib_linalg_constants
    implicit none
    
    
    real(sp),    parameter ::     negone = -1.00_sp
    real(sp),    parameter ::       zero = 0.00_sp
    real(sp),    parameter ::       half = 0.50_sp
    real(sp),    parameter ::        one = 1.00_sp
    real(sp),    parameter ::        two = 2.00_sp
    real(sp),    parameter ::      three = 3.00_sp
    real(sp),    parameter ::       four = 4.00_sp
    real(sp),    parameter ::      eight = 8.00_sp
    real(sp),    parameter ::        ten = 10.00_sp
    
    complex(sp), parameter :: czero   = ( 0.0_sp,0.0_sp)
    complex(sp), parameter :: chalf   = ( 0.5_sp,0.0_sp)
    complex(sp), parameter :: cone    = ( 1.0_sp,0.0_sp)
    complex(sp), parameter :: cnegone = (-1.0_sp,0.0_sp)

    ! scaling constants 
    integer,     parameter :: maxexp = maxexponent(zero) 
    integer,     parameter :: minexp = minexponent(zero) 
    
    real(sp),    parameter :: rradix = real(radix(zero),sp) 
    real(sp),    parameter :: ulp    = epsilon(zero) 
    real(sp),    parameter :: eps    = ulp*half
    real(sp),    parameter :: safmin = rradix**max(minexp-1,1-maxexp) 
    real(sp),    parameter :: safmax = one/safmin 
    real(sp),    parameter :: smlnum = safmin/ulp
    real(sp),    parameter :: bignum = safmax*ulp 
    real(sp),    parameter :: rtmin = sqrt(smlnum) 
    real(sp),    parameter :: rtmax = sqrt(bignum) 

    ! Blue's scaling constants 
    ! ssml>=1/s and sbig==1/S with s,S as defined in https://doi.org/10.1145/355769.355771 
    real(sp),    parameter :: tsml = rradix**ceiling((minexp-1)*half) 
    real(sp),    parameter :: tbig = rradix**floor((maxexp-digits(zero)+1)*half) 
    real(sp),    parameter :: ssml = rradix**(-floor((minexp-digits(zero))*half)) 
    real(sp),    parameter :: sbig = rradix**(-ceiling((maxexp+digits(zero)-1)*half))
    
end module

module stdlib_blas_constants_dp
    use stdlib_linalg_constants
    implicit none
    
    
    real(dp),    parameter ::     negone = -1.00_dp
    real(dp),    parameter ::       zero = 0.00_dp
    real(dp),    parameter ::       half = 0.50_dp
    real(dp),    parameter ::        one = 1.00_dp
    real(dp),    parameter ::        two = 2.00_dp
    real(dp),    parameter ::      three = 3.00_dp
    real(dp),    parameter ::       four = 4.00_dp
    real(dp),    parameter ::      eight = 8.00_dp
    real(dp),    parameter ::        ten = 10.00_dp
    
    complex(dp), parameter :: czero   = ( 0.0_dp,0.0_dp)
    complex(dp), parameter :: chalf   = ( 0.5_dp,0.0_dp)
    complex(dp), parameter :: cone    = ( 1.0_dp,0.0_dp)
    complex(dp), parameter :: cnegone = (-1.0_dp,0.0_dp)

    ! scaling constants 
    integer,     parameter :: maxexp = maxexponent(zero) 
    integer,     parameter :: minexp = minexponent(zero) 
    
    real(dp),    parameter :: rradix = real(radix(zero),dp) 
    real(dp),    parameter :: ulp    = epsilon(zero) 
    real(dp),    parameter :: eps    = ulp*half
    real(dp),    parameter :: safmin = rradix**max(minexp-1,1-maxexp) 
    real(dp),    parameter :: safmax = one/safmin 
    real(dp),    parameter :: smlnum = safmin/ulp
    real(dp),    parameter :: bignum = safmax*ulp 
    real(dp),    parameter :: rtmin = sqrt(smlnum) 
    real(dp),    parameter :: rtmax = sqrt(bignum) 

    ! Blue's scaling constants 
    ! ssml>=1/s and sbig==1/S with s,S as defined in https://doi.org/10.1145/355769.355771 
    real(dp),    parameter :: tsml = rradix**ceiling((minexp-1)*half) 
    real(dp),    parameter :: tbig = rradix**floor((maxexp-digits(zero)+1)*half) 
    real(dp),    parameter :: ssml = rradix**(-floor((minexp-digits(zero))*half)) 
    real(dp),    parameter :: sbig = rradix**(-ceiling((maxexp+digits(zero)-1)*half))
    
end module

