

submodule(stdlib_intrinsics) stdlib_intrinsics_dot_product
    !!Replacement for certain Fortran intrinsic functions offering either faster and/or more accurate implementations.
    !! ([Specification](../page/specs/stdlib_intrinsics.html))
    use stdlib_kinds
    use stdlib_constants
    implicit none

    integer, parameter :: ilp = int64
    
contains
! This implementation is based on https://github.com/jalvesz/fast_math
pure module function stdlib_dot_product_int8(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    integer(int8), intent(in) :: a(:)
    integer(int8), intent(in) :: b(:)
    integer(int8) :: p
    integer(int8) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_int8
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*b(i:i+chunk-1)
    end do

    p = zero_int8
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_dot_product_int16(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    integer(int16), intent(in) :: a(:)
    integer(int16), intent(in) :: b(:)
    integer(int16) :: p
    integer(int16) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_int16
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*b(i:i+chunk-1)
    end do

    p = zero_int16
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_dot_product_int32(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    integer(int32), intent(in) :: a(:)
    integer(int32), intent(in) :: b(:)
    integer(int32) :: p
    integer(int32) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_int32
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*b(i:i+chunk-1)
    end do

    p = zero_int32
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_dot_product_int64(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    integer(int64), intent(in) :: a(:)
    integer(int64), intent(in) :: b(:)
    integer(int64) :: p
    integer(int64) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_int64
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*b(i:i+chunk-1)
    end do

    p = zero_int64
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_dot_product_sp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    real(sp), intent(in) :: a(:)
    real(sp), intent(in) :: b(:)
    real(sp) :: p
    real(sp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_sp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*b(i:i+chunk-1)
    end do

    p = zero_sp
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_dot_product_dp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    real(dp), intent(in) :: a(:)
    real(dp), intent(in) :: b(:)
    real(dp) :: p
    real(dp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_dp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + a(i:i+chunk-1)*b(i:i+chunk-1)
    end do

    p = zero_dp
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_dot_product_csp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    complex(sp), intent(in) :: a(:)
    complex(sp), intent(in) :: b(:)
    complex(sp) :: p
    complex(sp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = conjg(a(1:r))*b(1:r)
    abatch(r+1:chunk) = zero_csp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + conjg(a(i:i+chunk-1))*b(i:i+chunk-1)
    end do

    p = zero_csp
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function
pure module function stdlib_dot_product_cdp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    complex(dp), intent(in) :: a(:)
    complex(dp), intent(in) :: b(:)
    complex(dp) :: p
    complex(dp) :: abatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = conjg(a(1:r))*b(1:r)
    abatch(r+1:chunk) = zero_cdp
    do i = r+1, n-r, chunk
        abatch(1:chunk) = abatch(1:chunk) + conjg(a(i:i+chunk-1))*b(i:i+chunk-1)
    end do

    p = zero_cdp
    do i = 1, chunk/2
        p = p + abatch(i)+abatch(chunk/2+i)
    end do
end function

pure module function stdlib_dot_product_kahan_sp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    real(sp), intent(in) :: a(:)
    real(sp), intent(in) :: b(:)
    real(sp) :: p
    real(sp) :: abatch(chunk)
    real(sp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_sp
    cbatch = zero_sp
    do i = r+1, n-r, chunk
        call kahan_kernel( a(i:i+chunk-1)*b(i:i+chunk-1) , abatch(1:chunk) , cbatch(1:chunk) )
    end do     

    p = zero_sp
    do i = 1, chunk
        call kahan_kernel( abatch(i) , p , cbatch(i) )
    end do      
end function
pure module function stdlib_dot_product_kahan_dp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    real(dp), intent(in) :: a(:)
    real(dp), intent(in) :: b(:)
    real(dp) :: p
    real(dp) :: abatch(chunk)
    real(dp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = a(1:r)*b(1:r)
    abatch(r+1:chunk) = zero_dp
    cbatch = zero_dp
    do i = r+1, n-r, chunk
        call kahan_kernel( a(i:i+chunk-1)*b(i:i+chunk-1) , abatch(1:chunk) , cbatch(1:chunk) )
    end do     

    p = zero_dp
    do i = 1, chunk
        call kahan_kernel( abatch(i) , p , cbatch(i) )
    end do      
end function
pure module function stdlib_dot_product_kahan_csp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    complex(sp), intent(in) :: a(:)
    complex(sp), intent(in) :: b(:)
    complex(sp) :: p
    complex(sp) :: abatch(chunk)
    complex(sp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = conjg(a(1:r))*b(1:r)
    abatch(r+1:chunk) = zero_csp
    cbatch = zero_csp
    do i = r+1, n-r, chunk
        call kahan_kernel( conjg(a(i:i+chunk-1))*b(i:i+chunk-1) , abatch(1:chunk) , cbatch(1:chunk) )
    end do     

    p = zero_csp
    do i = 1, chunk
        call kahan_kernel( abatch(i) , p , cbatch(i) )
    end do      
end function
pure module function stdlib_dot_product_kahan_cdp(a,b) result(p)
    integer(ilp), parameter :: chunk = 64
    complex(dp), intent(in) :: a(:)
    complex(dp), intent(in) :: b(:)
    complex(dp) :: p
    complex(dp) :: abatch(chunk)
    complex(dp) :: cbatch(chunk)
    integer(ilp) :: i, n, r
    ! -----------------------------
    n = size(a,kind=ilp)
    r = mod(n,chunk)

    abatch(1:r)       = conjg(a(1:r))*b(1:r)
    abatch(r+1:chunk) = zero_cdp
    cbatch = zero_cdp
    do i = r+1, n-r, chunk
        call kahan_kernel( conjg(a(i:i+chunk-1))*b(i:i+chunk-1) , abatch(1:chunk) , cbatch(1:chunk) )
    end do     

    p = zero_cdp
    do i = 1, chunk
        call kahan_kernel( abatch(i) , p , cbatch(i) )
    end do      
end function

end submodule stdlib_intrinsics_dot_product
