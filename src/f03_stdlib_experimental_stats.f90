module stdlib_experimental_stats


use stdlib_experimental_kinds, only: sp, dp, qp, &
    int8, int16, int32, int64
implicit none
private
! Public API
public :: mean

interface mean
    module function mean_1_sp_sp(x) result(res)
        real(sp), intent(in) :: x(:)
        real(sp) :: res
    end function mean_1_sp_sp
    module function mean_1_dp_dp(x) result(res)
        real(dp), intent(in) :: x(:)
        real(dp) :: res
    end function mean_1_dp_dp
    module function mean_1_qp_qp(x) result(res)
        real(qp), intent(in) :: x(:)
        real(qp) :: res
    end function mean_1_qp_qp

    module function mean_1_int8_dp(x) result(res)
        integer(int8), intent(in) :: x(:)
        real(dp) :: res
    end function mean_1_int8_dp
    module function mean_1_int16_dp(x) result(res)
        integer(int16), intent(in) :: x(:)
        real(dp) :: res
    end function mean_1_int16_dp
    module function mean_1_int32_dp(x) result(res)
        integer(int32), intent(in) :: x(:)
        real(dp) :: res
    end function mean_1_int32_dp
    module function mean_1_int64_dp(x) result(res)
        integer(int64), intent(in) :: x(:)
        real(dp) :: res
    end function mean_1_int64_dp


    module function mean_2_all_sp_sp(x) result(res)
        real(sp), intent(in) :: x(:,:)
        real(sp) :: res
    end function mean_2_all_sp_sp
    module function mean_2_all_dp_dp(x) result(res)
        real(dp), intent(in) :: x(:,:)
        real(dp) :: res
    end function mean_2_all_dp_dp
    module function mean_2_all_qp_qp(x) result(res)
        real(qp), intent(in) :: x(:,:)
        real(qp) :: res
    end function mean_2_all_qp_qp

    module function mean_2_all_int8_dp(x) result(res)
        integer(int8), intent(in) :: x(:,:)
        real(dp) :: res
    end function mean_2_all_int8_dp
    module function mean_2_all_int16_dp(x) result(res)
        integer(int16), intent(in) :: x(:,:)
        real(dp) :: res
    end function mean_2_all_int16_dp
    module function mean_2_all_int32_dp(x) result(res)
        integer(int32), intent(in) :: x(:,:)
        real(dp) :: res
    end function mean_2_all_int32_dp
    module function mean_2_all_int64_dp(x) result(res)
        integer(int64), intent(in) :: x(:,:)
        real(dp) :: res
    end function mean_2_all_int64_dp

    module function mean_2_sp_sp(x, dim) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(sp) :: res(size(x)/size(x, dim))
    end function mean_2_sp_sp
    module function mean_2_dp_dp(x, dim) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(dp) :: res(size(x)/size(x, dim))
    end function mean_2_dp_dp
    module function mean_2_qp_qp(x, dim) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(qp) :: res(size(x)/size(x, dim))
    end function mean_2_qp_qp

    module function mean_2_int8_dp(x, dim) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(dp) :: res(size(x)/size(x, dim))
    end function mean_2_int8_dp
    module function mean_2_int16_dp(x, dim) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(dp) :: res(size(x)/size(x, dim))
    end function mean_2_int16_dp
    module function mean_2_int32_dp(x, dim) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(dp) :: res(size(x)/size(x, dim))
    end function mean_2_int32_dp
    module function mean_2_int64_dp(x, dim) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        real(dp) :: res(size(x)/size(x, dim))
    end function mean_2_int64_dp





module function mean_3_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:)
    real(sp) :: res
end function mean_3_all_sp_sp
module function mean_4_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:)
    real(sp) :: res
end function mean_4_all_sp_sp
module function mean_5_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:)
    real(sp) :: res
end function mean_5_all_sp_sp
module function mean_6_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:)
    real(sp) :: res
end function mean_6_all_sp_sp
module function mean_7_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_7_all_sp_sp
module function mean_8_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_8_all_sp_sp
module function mean_9_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_9_all_sp_sp
module function mean_10_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_10_all_sp_sp
module function mean_11_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_11_all_sp_sp
module function mean_12_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_12_all_sp_sp
module function mean_13_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_13_all_sp_sp
module function mean_14_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_14_all_sp_sp
module function mean_15_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res
end function mean_15_all_sp_sp
module function mean_3_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:)
    real(dp) :: res
end function mean_3_all_dp_dp
module function mean_4_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:)
    real(dp) :: res
end function mean_4_all_dp_dp
module function mean_5_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res
end function mean_5_all_dp_dp
module function mean_6_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res
end function mean_6_all_dp_dp
module function mean_7_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_7_all_dp_dp
module function mean_8_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_8_all_dp_dp
module function mean_9_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_9_all_dp_dp
module function mean_10_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_10_all_dp_dp
module function mean_11_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_11_all_dp_dp
module function mean_12_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_12_all_dp_dp
module function mean_13_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_13_all_dp_dp
module function mean_14_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_14_all_dp_dp
module function mean_15_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_15_all_dp_dp
module function mean_3_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:)
    real(qp) :: res
end function mean_3_all_qp_qp
module function mean_4_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:)
    real(qp) :: res
end function mean_4_all_qp_qp
module function mean_5_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:)
    real(qp) :: res
end function mean_5_all_qp_qp
module function mean_6_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:)
    real(qp) :: res
end function mean_6_all_qp_qp
module function mean_7_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_7_all_qp_qp
module function mean_8_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_8_all_qp_qp
module function mean_9_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_9_all_qp_qp
module function mean_10_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_10_all_qp_qp
module function mean_11_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_11_all_qp_qp
module function mean_12_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_12_all_qp_qp
module function mean_13_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_13_all_qp_qp
module function mean_14_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_14_all_qp_qp
module function mean_15_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res
end function mean_15_all_qp_qp

module function mean_3_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:)
    real(dp) :: res
end function mean_3_all_int8_dp
module function mean_4_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:)
    real(dp) :: res
end function mean_4_all_int8_dp
module function mean_5_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res
end function mean_5_all_int8_dp
module function mean_6_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res
end function mean_6_all_int8_dp
module function mean_7_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_7_all_int8_dp
module function mean_8_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_8_all_int8_dp
module function mean_9_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_9_all_int8_dp
module function mean_10_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_10_all_int8_dp
module function mean_11_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_11_all_int8_dp
module function mean_12_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_12_all_int8_dp
module function mean_13_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_13_all_int8_dp
module function mean_14_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_14_all_int8_dp
module function mean_15_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_15_all_int8_dp
module function mean_3_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:)
    real(dp) :: res
end function mean_3_all_int16_dp
module function mean_4_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:)
    real(dp) :: res
end function mean_4_all_int16_dp
module function mean_5_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res
end function mean_5_all_int16_dp
module function mean_6_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res
end function mean_6_all_int16_dp
module function mean_7_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_7_all_int16_dp
module function mean_8_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_8_all_int16_dp
module function mean_9_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_9_all_int16_dp
module function mean_10_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_10_all_int16_dp
module function mean_11_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_11_all_int16_dp
module function mean_12_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_12_all_int16_dp
module function mean_13_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_13_all_int16_dp
module function mean_14_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_14_all_int16_dp
module function mean_15_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_15_all_int16_dp
module function mean_3_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:)
    real(dp) :: res
end function mean_3_all_int32_dp
module function mean_4_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:)
    real(dp) :: res
end function mean_4_all_int32_dp
module function mean_5_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res
end function mean_5_all_int32_dp
module function mean_6_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res
end function mean_6_all_int32_dp
module function mean_7_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_7_all_int32_dp
module function mean_8_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_8_all_int32_dp
module function mean_9_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_9_all_int32_dp
module function mean_10_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_10_all_int32_dp
module function mean_11_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_11_all_int32_dp
module function mean_12_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_12_all_int32_dp
module function mean_13_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_13_all_int32_dp
module function mean_14_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_14_all_int32_dp
module function mean_15_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_15_all_int32_dp
module function mean_3_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:)
    real(dp) :: res
end function mean_3_all_int64_dp
module function mean_4_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:)
    real(dp) :: res
end function mean_4_all_int64_dp
module function mean_5_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res
end function mean_5_all_int64_dp
module function mean_6_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res
end function mean_6_all_int64_dp
module function mean_7_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_7_all_int64_dp
module function mean_8_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_8_all_int64_dp
module function mean_9_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_9_all_int64_dp
module function mean_10_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_10_all_int64_dp
module function mean_11_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_11_all_int64_dp
module function mean_12_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_12_all_int64_dp
module function mean_13_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_13_all_int64_dp
module function mean_14_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_14_all_int64_dp
module function mean_15_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res
end function mean_15_all_int64_dp

module function mean_3_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )
end function mean_3_sp_sp
module function mean_4_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )
end function mean_4_sp_sp
module function mean_5_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )
end function mean_5_sp_sp
module function mean_6_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )
end function mean_6_sp_sp
module function mean_7_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )
end function mean_7_sp_sp
module function mean_8_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )
end function mean_8_sp_sp
module function mean_9_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )
end function mean_9_sp_sp
module function mean_10_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )
end function mean_10_sp_sp
module function mean_11_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )
end function mean_11_sp_sp
module function mean_12_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )
end function mean_12_sp_sp
module function mean_13_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )
end function mean_13_sp_sp
module function mean_14_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )
end function mean_14_sp_sp
module function mean_15_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )
end function mean_15_sp_sp
module function mean_3_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )
end function mean_3_dp_dp
module function mean_4_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )
end function mean_4_dp_dp
module function mean_5_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )
end function mean_5_dp_dp
module function mean_6_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )
end function mean_6_dp_dp
module function mean_7_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )
end function mean_7_dp_dp
module function mean_8_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )
end function mean_8_dp_dp
module function mean_9_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )
end function mean_9_dp_dp
module function mean_10_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )
end function mean_10_dp_dp
module function mean_11_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )
end function mean_11_dp_dp
module function mean_12_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )
end function mean_12_dp_dp
module function mean_13_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )
end function mean_13_dp_dp
module function mean_14_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )
end function mean_14_dp_dp
module function mean_15_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )
end function mean_15_dp_dp
module function mean_3_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )
end function mean_3_qp_qp
module function mean_4_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )
end function mean_4_qp_qp
module function mean_5_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )
end function mean_5_qp_qp
module function mean_6_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )
end function mean_6_qp_qp
module function mean_7_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )
end function mean_7_qp_qp
module function mean_8_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )
end function mean_8_qp_qp
module function mean_9_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )
end function mean_9_qp_qp
module function mean_10_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )
end function mean_10_qp_qp
module function mean_11_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )
end function mean_11_qp_qp
module function mean_12_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )
end function mean_12_qp_qp
module function mean_13_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )
end function mean_13_qp_qp
module function mean_14_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )
end function mean_14_qp_qp
module function mean_15_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )
end function mean_15_qp_qp

module function mean_3_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )
end function mean_3_int8_dp
module function mean_4_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )
end function mean_4_int8_dp
module function mean_5_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )
end function mean_5_int8_dp
module function mean_6_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )
end function mean_6_int8_dp
module function mean_7_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )
end function mean_7_int8_dp
module function mean_8_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )
end function mean_8_int8_dp
module function mean_9_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )
end function mean_9_int8_dp
module function mean_10_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )
end function mean_10_int8_dp
module function mean_11_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )
end function mean_11_int8_dp
module function mean_12_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )
end function mean_12_int8_dp
module function mean_13_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )
end function mean_13_int8_dp
module function mean_14_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )
end function mean_14_int8_dp
module function mean_15_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )
end function mean_15_int8_dp
module function mean_3_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )
end function mean_3_int16_dp
module function mean_4_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )
end function mean_4_int16_dp
module function mean_5_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )
end function mean_5_int16_dp
module function mean_6_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )
end function mean_6_int16_dp
module function mean_7_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )
end function mean_7_int16_dp
module function mean_8_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )
end function mean_8_int16_dp
module function mean_9_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )
end function mean_9_int16_dp
module function mean_10_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )
end function mean_10_int16_dp
module function mean_11_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )
end function mean_11_int16_dp
module function mean_12_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )
end function mean_12_int16_dp
module function mean_13_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )
end function mean_13_int16_dp
module function mean_14_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )
end function mean_14_int16_dp
module function mean_15_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )
end function mean_15_int16_dp
module function mean_3_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )
end function mean_3_int32_dp
module function mean_4_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )
end function mean_4_int32_dp
module function mean_5_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )
end function mean_5_int32_dp
module function mean_6_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )
end function mean_6_int32_dp
module function mean_7_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )
end function mean_7_int32_dp
module function mean_8_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )
end function mean_8_int32_dp
module function mean_9_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )
end function mean_9_int32_dp
module function mean_10_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )
end function mean_10_int32_dp
module function mean_11_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )
end function mean_11_int32_dp
module function mean_12_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )
end function mean_12_int32_dp
module function mean_13_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )
end function mean_13_int32_dp
module function mean_14_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )
end function mean_14_int32_dp
module function mean_15_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )
end function mean_15_int32_dp
module function mean_3_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )
end function mean_3_int64_dp
module function mean_4_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )
end function mean_4_int64_dp
module function mean_5_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )
end function mean_5_int64_dp
module function mean_6_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )
end function mean_6_int64_dp
module function mean_7_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )
end function mean_7_int64_dp
module function mean_8_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )
end function mean_8_int64_dp
module function mean_9_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )
end function mean_9_int64_dp
module function mean_10_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )
end function mean_10_int64_dp
module function mean_11_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )
end function mean_11_int64_dp
module function mean_12_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )
end function mean_12_int64_dp
module function mean_13_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )
end function mean_13_int64_dp
module function mean_14_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )
end function mean_14_int64_dp
module function mean_15_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )
end function mean_15_int64_dp

end interface

end module
