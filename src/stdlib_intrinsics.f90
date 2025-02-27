
module stdlib_intrinsics
    !!Alternative implementations of some Fortran intrinsic functions offering either faster and/or more accurate evaluation.
    !! ([Specification](../page/specs/stdlib_intrinsics.html))
    use stdlib_kinds
    implicit none
    private

    interface stdlib_sum
        !! version: experimental 
        !!
        !!### Summary 
        !! Sum elements of rank N arrays. 
        !! ([Specification](../page/specs/stdlib_intrinsics.html#stdlib_sum))
        !!
        !!### Description
        !! 
        !! This interface provides standard conforming call for sum of elements of any rank.
        !! The 1-D base implementation follows a chunked approach for optimizing performance and increasing accuracy.
        !! The `N-D` interfaces calls upon the `(N-1)-D` implementation. 
        !! Supported data types include `real`, `complex` and `integer`.
        !!
        pure module function stdlib_sum_1d_int8(a) result(s)
            integer(int8), intent(in) :: a(:)
            integer(int8) :: s
        end function
        pure module function stdlib_sum_1d_int8_mask(a,mask) result(s)
            integer(int8), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            integer(int8) :: s
        end function
        pure module function stdlib_sum_2d_int8( x, mask ) result( s )
            integer(int8), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            integer(int8) :: s
        end function
        pure module function stdlib_sum_2d_dim_int8( x , dim, mask ) result( s )
            integer(int8), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            integer(int8) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_int8( x, mask ) result( s )
            integer(int8), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            integer(int8) :: s
        end function
        pure module function stdlib_sum_3d_dim_int8( x , dim, mask ) result( s )
            integer(int8), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            integer(int8) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_int8( x, mask ) result( s )
            integer(int8), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int8) :: s
        end function
        pure module function stdlib_sum_4d_dim_int8( x , dim, mask ) result( s )
            integer(int8), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int8) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
                & 3), size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_1d_int16(a) result(s)
            integer(int16), intent(in) :: a(:)
            integer(int16) :: s
        end function
        pure module function stdlib_sum_1d_int16_mask(a,mask) result(s)
            integer(int16), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            integer(int16) :: s
        end function
        pure module function stdlib_sum_2d_int16( x, mask ) result( s )
            integer(int16), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            integer(int16) :: s
        end function
        pure module function stdlib_sum_2d_dim_int16( x , dim, mask ) result( s )
            integer(int16), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            integer(int16) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_int16( x, mask ) result( s )
            integer(int16), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            integer(int16) :: s
        end function
        pure module function stdlib_sum_3d_dim_int16( x , dim, mask ) result( s )
            integer(int16), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            integer(int16) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_int16( x, mask ) result( s )
            integer(int16), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int16) :: s
        end function
        pure module function stdlib_sum_4d_dim_int16( x , dim, mask ) result( s )
            integer(int16), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int16) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim),&
                & merge(size(x, 3), size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_1d_int32(a) result(s)
            integer(int32), intent(in) :: a(:)
            integer(int32) :: s
        end function
        pure module function stdlib_sum_1d_int32_mask(a,mask) result(s)
            integer(int32), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            integer(int32) :: s
        end function
        pure module function stdlib_sum_2d_int32( x, mask ) result( s )
            integer(int32), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            integer(int32) :: s
        end function
        pure module function stdlib_sum_2d_dim_int32( x , dim, mask ) result( s )
            integer(int32), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            integer(int32) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_int32( x, mask ) result( s )
            integer(int32), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            integer(int32) :: s
        end function
        pure module function stdlib_sum_3d_dim_int32( x , dim, mask ) result( s )
            integer(int32), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            integer(int32) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_int32( x, mask ) result( s )
            integer(int32), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int32) :: s
        end function
        pure module function stdlib_sum_4d_dim_int32( x , dim, mask ) result( s )
            integer(int32), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int32) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim),&
                & merge(size(x, 3), size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_1d_int64(a) result(s)
            integer(int64), intent(in) :: a(:)
            integer(int64) :: s
        end function
        pure module function stdlib_sum_1d_int64_mask(a,mask) result(s)
            integer(int64), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            integer(int64) :: s
        end function
        pure module function stdlib_sum_2d_int64( x, mask ) result( s )
            integer(int64), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            integer(int64) :: s
        end function
        pure module function stdlib_sum_2d_dim_int64( x , dim, mask ) result( s )
            integer(int64), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            integer(int64) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_int64( x, mask ) result( s )
            integer(int64), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            integer(int64) :: s
        end function
        pure module function stdlib_sum_3d_dim_int64( x , dim, mask ) result( s )
            integer(int64), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            integer(int64) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_int64( x, mask ) result( s )
            integer(int64), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int64) :: s
        end function
        pure module function stdlib_sum_4d_dim_int64( x , dim, mask ) result( s )
            integer(int64), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            integer(int64) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim),&
                & merge(size(x, 3), size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_1d_sp(a) result(s)
            real(sp), intent(in) :: a(:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_1d_sp_mask(a,mask) result(s)
            real(sp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_2d_sp( x, mask ) result( s )
            real(sp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_2d_dim_sp( x , dim, mask ) result( s )
            real(sp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_sp( x, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_3d_dim_sp( x , dim, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_sp( x, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_4d_dim_sp( x , dim, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
                & size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_1d_dp(a) result(s)
            real(dp), intent(in) :: a(:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_1d_dp_mask(a,mask) result(s)
            real(dp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_2d_dp( x, mask ) result( s )
            real(dp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_2d_dim_dp( x , dim, mask ) result( s )
            real(dp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_dp( x, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_3d_dim_dp( x , dim, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_dp( x, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_4d_dim_dp( x , dim, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
                & size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_1d_csp(a) result(s)
            complex(sp), intent(in) :: a(:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_1d_csp_mask(a,mask) result(s)
            complex(sp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_2d_csp( x, mask ) result( s )
            complex(sp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_2d_dim_csp( x , dim, mask ) result( s )
            complex(sp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_csp( x, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_3d_dim_csp( x , dim, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_csp( x, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_4d_dim_csp( x , dim, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
                & 3), size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_1d_cdp(a) result(s)
            complex(dp), intent(in) :: a(:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_1d_cdp_mask(a,mask) result(s)
            complex(dp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_2d_cdp( x, mask ) result( s )
            complex(dp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_2d_dim_cdp( x , dim, mask ) result( s )
            complex(dp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_3d_cdp( x, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_3d_dim_cdp( x , dim, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_4d_cdp( x, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_4d_dim_cdp( x , dim, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
                & 3), size(x, 4), mask=3<dim))
        end function
    end interface
    public :: stdlib_sum

    interface stdlib_sum_kahan
        !! version: experimental 
        !!
        !!### Summary 
        !! Sum elements of rank N arrays. 
        !! ([Specification](../page/specs/stdlib_intrinsics.html#stdlib_sum_kahan))
        !!
        !!### Description
        !! 
        !! This interface provides standard conforming call for sum of elements of any rank.
        !! The 1-D base implementation follows a chunked approach combined with a kahan kernel for optimizing performance and increasing accuracy.
        !! The `N-D` interfaces calls upon the `(N-1)-D` implementation. 
        !! Supported data types include `real` and `complex`.
        !!
        pure module function stdlib_sum_kahan_1d_sp(a) result(s)
            real(sp), intent(in) :: a(:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_kahan_1d_sp_mask(a,mask) result(s)
            real(sp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_sp( x, mask ) result( s )
            real(sp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_dim_sp( x , dim, mask ) result( s )
            real(sp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_kahan_3d_sp( x, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_kahan_3d_dim_sp( x , dim, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_kahan_4d_sp( x, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            real(sp) :: s
        end function
        pure module function stdlib_sum_kahan_4d_dim_sp( x , dim, mask ) result( s )
            real(sp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            real(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
                & size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_kahan_1d_dp(a) result(s)
            real(dp), intent(in) :: a(:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_kahan_1d_dp_mask(a,mask) result(s)
            real(dp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_dp( x, mask ) result( s )
            real(dp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_dim_dp( x , dim, mask ) result( s )
            real(dp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_kahan_3d_dp( x, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_kahan_3d_dim_dp( x , dim, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_kahan_4d_dp( x, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            real(dp) :: s
        end function
        pure module function stdlib_sum_kahan_4d_dim_dp( x , dim, mask ) result( s )
            real(dp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            real(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
                & size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_kahan_1d_csp(a) result(s)
            complex(sp), intent(in) :: a(:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_kahan_1d_csp_mask(a,mask) result(s)
            complex(sp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_csp( x, mask ) result( s )
            complex(sp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_dim_csp( x , dim, mask ) result( s )
            complex(sp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_kahan_3d_csp( x, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_kahan_3d_dim_csp( x , dim, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_kahan_4d_csp( x, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(sp) :: s
        end function
        pure module function stdlib_sum_kahan_4d_dim_csp( x , dim, mask ) result( s )
            complex(sp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(sp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
                & 3), size(x, 4), mask=3<dim))
        end function
        pure module function stdlib_sum_kahan_1d_cdp(a) result(s)
            complex(dp), intent(in) :: a(:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_kahan_1d_cdp_mask(a,mask) result(s)
            complex(dp), intent(in) :: a(:)
            logical, intent(in) :: mask(:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_cdp( x, mask ) result( s )
            complex(dp), intent(in) :: x(:,:)
            logical, intent(in), optional :: mask(:,:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_kahan_2d_dim_cdp( x , dim, mask ) result( s )
            complex(dp), intent(in) :: x(:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:)
            complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function
        pure module function stdlib_sum_kahan_3d_cdp( x, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:)
            logical, intent(in), optional :: mask(:,:,:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_kahan_3d_dim_cdp( x , dim, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:)
            complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function
        pure module function stdlib_sum_kahan_4d_cdp( x, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:,:)
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(dp) :: s
        end function
        pure module function stdlib_sum_kahan_4d_dim_cdp( x , dim, mask ) result( s )
            complex(dp), intent(in) :: x(:,:,:,:)
            integer, intent(in):: dim
            logical, intent(in), optional :: mask(:,:,:,:)
            complex(dp) :: s(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
                & 3), size(x, 4), mask=3<dim))
        end function
    end interface
    public :: stdlib_sum_kahan

    interface stdlib_dot_product
        !! version: experimental 
        !!
        !!### Summary 
        !! dot_product of rank 1 arrays. 
        !! ([Specification](../page/specs/stdlib_intrinsics.html#stdlib_dot_product))
        !!
        !!### Description
        !! 
        !! compute the dot_product of rank 1 arrays.
        !! The 1-D base implementation follows a chunked approach for optimizing performance and increasing accuracy.
        !! Supported data types include `real`, `complex` and `integer`.
        !!
        pure module function stdlib_dot_product_int8(a,b) result(p)
            integer(int8), intent(in) :: a(:)
            integer(int8), intent(in) :: b(:)
            integer(int8) :: p
        end function
        pure module function stdlib_dot_product_int16(a,b) result(p)
            integer(int16), intent(in) :: a(:)
            integer(int16), intent(in) :: b(:)
            integer(int16) :: p
        end function
        pure module function stdlib_dot_product_int32(a,b) result(p)
            integer(int32), intent(in) :: a(:)
            integer(int32), intent(in) :: b(:)
            integer(int32) :: p
        end function
        pure module function stdlib_dot_product_int64(a,b) result(p)
            integer(int64), intent(in) :: a(:)
            integer(int64), intent(in) :: b(:)
            integer(int64) :: p
        end function
        pure module function stdlib_dot_product_sp(a,b) result(p)
            real(sp), intent(in) :: a(:)
            real(sp), intent(in) :: b(:)
            real(sp) :: p
        end function
        pure module function stdlib_dot_product_dp(a,b) result(p)
            real(dp), intent(in) :: a(:)
            real(dp), intent(in) :: b(:)
            real(dp) :: p
        end function
        pure module function stdlib_dot_product_csp(a,b) result(p)
            complex(sp), intent(in) :: a(:)
            complex(sp), intent(in) :: b(:)
            complex(sp) :: p
        end function
        pure module function stdlib_dot_product_cdp(a,b) result(p)
            complex(dp), intent(in) :: a(:)
            complex(dp), intent(in) :: b(:)
            complex(dp) :: p
        end function
    end interface
    public :: stdlib_dot_product

    interface stdlib_dot_product_kahan
        !! version: experimental 
        !!
        !!### Summary 
        !! dot_product of rank 1 arrays. 
        !! ([Specification](../page/specs/stdlib_intrinsics.html#stdlib_dot_product_kahan))
        !!
        !!### Description
        !! 
        !! compute the dot_product of rank 1 arrays.
        !! The implementation follows a chunked approach combined with a kahan kernel for optimizing performance and increasing accuracy.
        !! Supported data types include `real` and `complex`.
        !!
        pure module function stdlib_dot_product_kahan_sp(a,b) result(p)
            real(sp), intent(in) :: a(:)
            real(sp), intent(in) :: b(:)
            real(sp) :: p
        end function
        pure module function stdlib_dot_product_kahan_dp(a,b) result(p)
            real(dp), intent(in) :: a(:)
            real(dp), intent(in) :: b(:)
            real(dp) :: p
        end function
        pure module function stdlib_dot_product_kahan_csp(a,b) result(p)
            complex(sp), intent(in) :: a(:)
            complex(sp), intent(in) :: b(:)
            complex(sp) :: p
        end function
        pure module function stdlib_dot_product_kahan_cdp(a,b) result(p)
            complex(dp), intent(in) :: a(:)
            complex(dp), intent(in) :: b(:)
            complex(dp) :: p
        end function
    end interface
    public :: stdlib_dot_product_kahan

    interface kahan_kernel 
        module procedure :: kahan_kernel_sp
        module procedure :: kahan_kernel_m_sp
        module procedure :: kahan_kernel_dp
        module procedure :: kahan_kernel_m_dp
        module procedure :: kahan_kernel_csp
        module procedure :: kahan_kernel_m_csp
        module procedure :: kahan_kernel_cdp
        module procedure :: kahan_kernel_m_cdp
    end interface
    public :: kahan_kernel
    
contains

elemental subroutine kahan_kernel_sp(a,s,c)
    real(sp), intent(in) :: a
    real(sp), intent(inout) :: s
    real(sp), intent(inout) :: c
    real(sp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = t
end subroutine  
elemental subroutine kahan_kernel_m_sp(a,s,c,m)
    real(sp), intent(in) :: a
    real(sp), intent(inout) :: s
    real(sp), intent(inout) :: c
    logical, intent(in) :: m
    real(sp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = merge( s , t , m )
end subroutine 
elemental subroutine kahan_kernel_dp(a,s,c)
    real(dp), intent(in) :: a
    real(dp), intent(inout) :: s
    real(dp), intent(inout) :: c
    real(dp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = t
end subroutine  
elemental subroutine kahan_kernel_m_dp(a,s,c,m)
    real(dp), intent(in) :: a
    real(dp), intent(inout) :: s
    real(dp), intent(inout) :: c
    logical, intent(in) :: m
    real(dp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = merge( s , t , m )
end subroutine 
elemental subroutine kahan_kernel_csp(a,s,c)
    complex(sp), intent(in) :: a
    complex(sp), intent(inout) :: s
    complex(sp), intent(inout) :: c
    complex(sp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = t
end subroutine  
elemental subroutine kahan_kernel_m_csp(a,s,c,m)
    complex(sp), intent(in) :: a
    complex(sp), intent(inout) :: s
    complex(sp), intent(inout) :: c
    logical, intent(in) :: m
    complex(sp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = merge( s , t , m )
end subroutine 
elemental subroutine kahan_kernel_cdp(a,s,c)
    complex(dp), intent(in) :: a
    complex(dp), intent(inout) :: s
    complex(dp), intent(inout) :: c
    complex(dp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = t
end subroutine  
elemental subroutine kahan_kernel_m_cdp(a,s,c,m)
    complex(dp), intent(in) :: a
    complex(dp), intent(inout) :: s
    complex(dp), intent(inout) :: c
    logical, intent(in) :: m
    complex(dp) :: t, y
    y = a - c
    t = s + y
    c = (t - s) - y
    s = merge( s , t , m )
end subroutine 

end module stdlib_intrinsics
