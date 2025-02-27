
submodule(stdlib_intrinsics) stdlib_intrinsics_sum
    !! ([Specification](../page/specs/stdlib_intrinsics.html))
    use stdlib_kinds
    use stdlib_constants
    implicit none

    integer, parameter :: ilp = int64
    
contains

!================= 1D Base implementations ============
! This implementation is based on https://github.com/jalvesz/fast_math
pure module function stdlib_sum_1d_int8(a) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int8), intent(in) :: a(:)
    integer(int8) :: s
    integer(int8) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_int8
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_int8
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_int8_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int8), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    integer(int8) :: s
    integer(int8) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_int8 , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_int8
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_int8 , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_int8
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_sum_1d_int16(a) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int16), intent(in) :: a(:)
    integer(int16) :: s
    integer(int16) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_int16
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_int16
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_int16_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int16), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    integer(int16) :: s
    integer(int16) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_int16 , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_int16
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_int16 , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_int16
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_sum_1d_int32(a) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int32), intent(in) :: a(:)
    integer(int32) :: s
    integer(int32) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_int32
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_int32
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_int32_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int32), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    integer(int32) :: s
    integer(int32) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_int32 , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_int32
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_int32 , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_int32
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_sum_1d_int64(a) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int64), intent(in) :: a(:)
    integer(int64) :: s
    integer(int64) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_int64
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_int64
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_int64_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    integer(int64), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    integer(int64) :: s
    integer(int64) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_int64 , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_int64
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_int64 , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_int64
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_sum_1d_sp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    real(sp), intent(in) :: a(:)
    real(sp) :: s
    real(sp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_sp
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_sp
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_sp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    real(sp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    real(sp) :: s
    real(sp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_sp , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_sp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_sp , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_sp
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_sum_1d_dp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    real(dp), intent(in) :: a(:)
    real(dp) :: s
    real(dp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_dp
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_dp
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_dp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    real(dp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    real(dp) :: s
    real(dp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_dp , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_dp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_dp , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_dp
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_sum_1d_csp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(sp), intent(in) :: a(:)
    complex(sp) :: s
    complex(sp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_csp
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_csp
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_csp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(sp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    complex(sp) :: s
    complex(sp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_csp , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_csp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_csp , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_csp
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_sum_1d_cdp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(dp), intent(in) :: a(:)
    complex(dp) :: s
    complex(dp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    abatch(1:r)       = a(1:r)
    abatch(r+1:chunk) = zero_cdp
    do i = r+1, n-r, chunk
     abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)
    end do

    s = zero_cdp
    do i = 1, chunk/2
      s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_1d_cdp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(dp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    complex(dp) :: s
    complex(dp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = merge( zero_cdp , a(1:r) , mask(1:r) )
    abatch(r+1:chunk) = zero_cdp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + merge( zero_cdp , a(i:i+chunk-1), mask(i:i+chunk-1) )
    end do
    
    s = zero_cdp
    do i = 1, chunk/2
        s = s + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_sum_kahan_1d_sp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    real(sp), intent(in) :: a(:)
    real(sp) :: s
    real(sp) :: sbatch(chunk)
    real(sp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    sbatch(1:r) = a(1:r)
    sbatch(r+1:chunk)  = zero_sp
    cbatch = zero_sp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk) )
    end do 

    s = zero_sp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function

pure module function stdlib_sum_kahan_1d_sp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    real(sp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    real(sp) :: s
    real(sp) :: sbatch(chunk)
    real(sp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    sbatch(1:r) = merge( zero_sp , a(1:r) , mask(1:r) )
    sbatch(r+1:chunk)  = zero_sp
    cbatch = zero_sp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk), mask(i:i+chunk-1) )
    end do 

    s = zero_sp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function
pure module function stdlib_sum_kahan_1d_dp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    real(dp), intent(in) :: a(:)
    real(dp) :: s
    real(dp) :: sbatch(chunk)
    real(dp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    sbatch(1:r) = a(1:r)
    sbatch(r+1:chunk)  = zero_dp
    cbatch = zero_dp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk) )
    end do 

    s = zero_dp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function

pure module function stdlib_sum_kahan_1d_dp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    real(dp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    real(dp) :: s
    real(dp) :: sbatch(chunk)
    real(dp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    sbatch(1:r) = merge( zero_dp , a(1:r) , mask(1:r) )
    sbatch(r+1:chunk)  = zero_dp
    cbatch = zero_dp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk), mask(i:i+chunk-1) )
    end do 

    s = zero_dp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function
pure module function stdlib_sum_kahan_1d_csp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(sp), intent(in) :: a(:)
    complex(sp) :: s
    complex(sp) :: sbatch(chunk)
    complex(sp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    sbatch(1:r) = a(1:r)
    sbatch(r+1:chunk)  = zero_csp
    cbatch = zero_csp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk) )
    end do 

    s = zero_csp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function

pure module function stdlib_sum_kahan_1d_csp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(sp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    complex(sp) :: s
    complex(sp) :: sbatch(chunk)
    complex(sp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    sbatch(1:r) = merge( zero_csp , a(1:r) , mask(1:r) )
    sbatch(r+1:chunk)  = zero_csp
    cbatch = zero_csp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk), mask(i:i+chunk-1) )
    end do 

    s = zero_csp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function
pure module function stdlib_sum_kahan_1d_cdp(a) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(dp), intent(in) :: a(:)
    complex(dp) :: s
    complex(dp) :: sbatch(chunk)
    complex(dp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    sbatch(1:r) = a(1:r)
    sbatch(r+1:chunk)  = zero_cdp
    cbatch = zero_cdp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk) )
    end do 

    s = zero_cdp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function

pure module function stdlib_sum_kahan_1d_cdp_mask(a,mask) result(s)
    integer(ilp), parameter :: chunk = 64
    complex(dp), intent(in) :: a(:)
    logical, intent(in) :: mask(:)
    complex(dp) :: s
    complex(dp) :: sbatch(chunk)
    complex(dp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)
    
    sbatch(1:r) = merge( zero_cdp , a(1:r) , mask(1:r) )
    sbatch(r+1:chunk)  = zero_cdp
    cbatch = zero_cdp
    do i = r+1, n-r, chunk
      call kahan_kernel( a(i:i+chunk-1) , sbatch(1:chunk) , cbatch(1:chunk), mask(i:i+chunk-1) )
    end do 

    s = zero_cdp
    do i = 1,chunk
        call kahan_kernel( sbatch(i) , s , cbatch(i) )
    end do
end function

!================= N-D implementations ============
pure module function stdlib_sum_2d_int8( x , mask ) result( s )
    integer(int8), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    integer(int8) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int8) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int8), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int8) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int8), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_int8( x , dim, mask ) result( s )
    integer(int8), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    integer(int8) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_int8( x , mask ) result( s )
    integer(int8), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    integer(int8) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int8) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int8), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int8) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int8), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_int8( x , dim, mask ) result( s )
    integer(int8), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    integer(int8) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_int8( x , mask ) result( s )
    integer(int8), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int8) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int8) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int8), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int8) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int8), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_int8( x , dim, mask ) result( s )
    integer(int8), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int8) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_2d_int16( x , mask ) result( s )
    integer(int16), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    integer(int16) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int16) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int16), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int16) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int16), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_int16( x , dim, mask ) result( s )
    integer(int16), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    integer(int16) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_int16( x , mask ) result( s )
    integer(int16), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    integer(int16) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int16) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int16), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int16) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int16), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_int16( x , dim, mask ) result( s )
    integer(int16), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    integer(int16) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_int16( x , mask ) result( s )
    integer(int16), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int16) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int16) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int16), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int16) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int16), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_int16( x , dim, mask ) result( s )
    integer(int16), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int16) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_2d_int32( x , mask ) result( s )
    integer(int32), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    integer(int32) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int32) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int32), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int32) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int32), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_int32( x , dim, mask ) result( s )
    integer(int32), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    integer(int32) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_int32( x , mask ) result( s )
    integer(int32), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    integer(int32) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int32) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int32), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int32) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int32), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_int32( x , dim, mask ) result( s )
    integer(int32), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    integer(int32) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_int32( x , mask ) result( s )
    integer(int32), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int32) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int32) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int32), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int32) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int32), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_int32( x , dim, mask ) result( s )
    integer(int32), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int32) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_2d_int64( x , mask ) result( s )
    integer(int64), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    integer(int64) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int64) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int64), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int64) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int64), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_int64( x , dim, mask ) result( s )
    integer(int64), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    integer(int64) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_int64( x , mask ) result( s )
    integer(int64), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    integer(int64) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int64) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int64), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int64) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int64), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_int64( x , dim, mask ) result( s )
    integer(int64), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    integer(int64) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_int64( x , mask ) result( s )
    integer(int64), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int64) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure integer(int64) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      integer(int64), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure integer(int64) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      integer(int64), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_int64( x , dim, mask ) result( s )
    integer(int64), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    integer(int64) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_2d_sp( x , mask ) result( s )
    real(sp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    real(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure real(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_sp( x , dim, mask ) result( s )
    real(sp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_sp( x , mask ) result( s )
    real(sp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    real(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure real(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_sp( x , dim, mask ) result( s )
    real(sp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_sp( x , mask ) result( s )
    real(sp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    real(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure real(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_sp( x , dim, mask ) result( s )
    real(sp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3), size(x,&
        & 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_2d_dp( x , mask ) result( s )
    real(dp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    real(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure real(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_dp( x , dim, mask ) result( s )
    real(dp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_dp( x , mask ) result( s )
    real(dp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    real(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure real(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_dp( x , dim, mask ) result( s )
    real(dp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_dp( x , mask ) result( s )
    real(dp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    real(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure real(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_dp( x , dim, mask ) result( s )
    real(dp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3), size(x,&
        & 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_2d_csp( x , mask ) result( s )
    complex(sp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    complex(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure complex(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_csp( x , dim, mask ) result( s )
    complex(sp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_csp( x , mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    complex(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure complex(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_csp( x , dim, mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_csp( x , mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure complex(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_csp( x , dim, mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_2d_cdp( x , mask ) result( s )
    complex(dp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    complex(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure complex(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_2d_dim_cdp( x , dim, mask ) result( s )
    complex(dp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_3d_cdp( x , mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    complex(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure complex(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_3d_dim_cdp( x , dim, mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_4d_cdp( x , mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum(b)
    end function
    pure complex(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum(b,m)
    end function
end function

pure module function stdlib_sum_4d_dim_cdp( x , dim, mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function

pure module function stdlib_sum_kahan_2d_sp( x , mask ) result( s )
    real(sp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    real(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure real(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_2d_dim_sp( x , dim, mask ) result( s )
    real(sp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_3d_sp( x , mask ) result( s )
    real(sp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    real(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure real(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_3d_dim_sp( x , dim, mask ) result( s )
    real(sp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_4d_sp( x , mask ) result( s )
    real(sp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    real(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure real(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_4d_dim_sp( x , dim, mask ) result( s )
    real(sp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3), size(x,&
        & 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_2d_dp( x , mask ) result( s )
    real(dp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    real(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure real(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_2d_dim_dp( x , dim, mask ) result( s )
    real(dp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_3d_dp( x , mask ) result( s )
    real(dp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    real(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure real(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_3d_dim_dp( x , dim, mask ) result( s )
    real(dp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_4d_dp( x , mask ) result( s )
    real(dp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    real(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure real(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure real(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      real(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_4d_dim_dp( x , dim, mask ) result( s )
    real(dp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3), size(x,&
        & 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_2d_csp( x , mask ) result( s )
    complex(sp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    complex(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure complex(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_2d_dim_csp( x , dim, mask ) result( s )
    complex(sp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_3d_csp( x , mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    complex(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure complex(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_3d_dim_csp( x , dim, mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_4d_csp( x , mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(sp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(sp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure complex(sp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(sp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_4d_dim_csp( x , dim, mask ) result( s )
    complex(sp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_2d_cdp( x , mask ) result( s )
    complex(dp), intent(in) :: x(:,:)
    logical, intent(in), optional :: mask(:,:)
    complex(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure complex(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_2d_dim_cdp( x , dim, mask ) result( s )
    complex(dp), intent(in) :: x(:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:)
    complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :) )
            end do
        end if
    else 
        if(dim<2)then
            do j = 1, size(x,dim=2)
                s(j) = stdlib_sum_kahan( x(:, j), mask=mask(:, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j) = stdlib_sum_kahan( x(j, :), mask=mask(j, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_3d_cdp( x , mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:)
    logical, intent(in), optional :: mask(:,:,:)
    complex(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure complex(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_3d_dim_cdp( x , dim, mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:)
    complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2 )
            end do
        end if
    else 
        if(dim<3)then
            do j = 1, size(x,dim=3)
                s(:, j) = stdlib_sum_kahan( x(:, :, j), dim=dim, mask=mask(:, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :) = stdlib_sum_kahan( x(j, :, :), dim=2, mask=mask(j, :, :) )
            end do
        end if
    end if

end function
pure module function stdlib_sum_kahan_4d_cdp( x , mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:,:)
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(dp) :: s
    if(.not.present(mask)) then
        s = sum_recast(x,size(x,kind=ilp))
    else
        s = sum_recast_mask(x,mask,size(x,kind=ilp))
    end if
contains
    pure complex(dp) function sum_recast(b,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      sum_recast = stdlib_sum_kahan(b)
    end function
    pure complex(dp) function sum_recast_mask(b,m,n)
      integer(ilp), intent(in) :: n
      complex(dp), intent(in) :: b(n)
      logical, intent(in) :: m(n)
      sum_recast_mask = stdlib_sum_kahan(b,m)
    end function
end function

pure module function stdlib_sum_kahan_4d_dim_cdp( x , dim, mask ) result( s )
    complex(dp), intent(in) :: x(:,:,:,:)
    integer, intent(in):: dim
    logical, intent(in), optional :: mask(:,:,:,:)
    complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
        & size(x, 4), mask=3<dim))
    integer :: j 

    if(.not.present(mask)) then
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3 )
            end do
        end if
    else 
        if(dim<4)then
            do j = 1, size(x,dim=4)
                s(:, :, j) = stdlib_sum_kahan( x(:, :, :, j), dim=dim, mask=mask(:, :, :, j) )
            end do
        else
            do j = 1, size(x,dim=1)
                s(j, :, :) = stdlib_sum_kahan( x(j, :, :, :), dim=3, mask=mask(j, :, :, :) )
            end do
        end if
    end if

end function

end submodule stdlib_intrinsics_sum
