
module stdlib_math
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, qp

    implicit none
    private
    public :: clip

    interface clip
        module procedure clip_int8
        module procedure clip_int16
        module procedure clip_int32
        module procedure clip_int64
        module procedure clip_sp
        module procedure clip_dp
        module procedure clip_qp
    end interface clip

contains
    
    elemental function clip_int8(x, xmin, xmax) result(res)
        integer(int8), intent(in) :: x
        integer(int8), intent(in) :: xmin
        integer(int8), intent(in) :: xmax
        integer(int8) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int8
    
    elemental function clip_int16(x, xmin, xmax) result(res)
        integer(int16), intent(in) :: x
        integer(int16), intent(in) :: xmin
        integer(int16), intent(in) :: xmax
        integer(int16) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int16
    
    elemental function clip_int32(x, xmin, xmax) result(res)
        integer(int32), intent(in) :: x
        integer(int32), intent(in) :: xmin
        integer(int32), intent(in) :: xmax
        integer(int32) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int32
    
    elemental function clip_int64(x, xmin, xmax) result(res)
        integer(int64), intent(in) :: x
        integer(int64), intent(in) :: xmin
        integer(int64), intent(in) :: xmax
        integer(int64) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int64
    
    elemental function clip_sp(x, xmin, xmax) result(res)
        real(sp), intent(in) :: x
        real(sp), intent(in) :: xmin
        real(sp), intent(in) :: xmax
        real(sp) :: res

        res = max(min(x, xmax), xmin)
    end function clip_sp
    
    elemental function clip_dp(x, xmin, xmax) result(res)
        real(dp), intent(in) :: x
        real(dp), intent(in) :: xmin
        real(dp), intent(in) :: xmax
        real(dp) :: res

        res = max(min(x, xmax), xmin)
    end function clip_dp
    
    elemental function clip_qp(x, xmin, xmax) result(res)
        real(qp), intent(in) :: x
        real(qp), intent(in) :: xmin
        real(qp), intent(in) :: xmax
        real(qp) :: res

        res = max(min(x, xmax), xmin)
    end function clip_qp
    
end module stdlib_math
