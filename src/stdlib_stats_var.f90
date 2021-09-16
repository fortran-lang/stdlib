submodule (stdlib_stats) stdlib_stats_var

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function var_all_1_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_1_rsp_rsp
      module function var_all_2_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_2_rsp_rsp
      module function var_all_3_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_3_rsp_rsp
      module function var_all_4_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_4_rsp_rsp
      module function var_all_1_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_1_rdp_rdp
      module function var_all_2_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_2_rdp_rdp
      module function var_all_3_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_3_rdp_rdp
      module function var_all_4_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_4_rdp_rdp
      module function var_all_1_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_1_rqp_rqp
      module function var_all_2_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_2_rqp_rqp
      module function var_all_3_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_3_rqp_rqp
      module function var_all_4_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum((x - mean)**2) / (n - merge(1, 0 , optval(corrected, .true.)))

      end function var_all_4_rqp_rqp
      module function var_all_1_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_1_csp_csp
      module function var_all_2_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_2_csp_csp
      module function var_all_3_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_3_csp_csp
      module function var_all_4_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_4_csp_csp
      module function var_all_1_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_1_cdp_cdp
      module function var_all_2_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_2_cdp_cdp
      module function var_all_3_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_3_cdp_cdp
      module function var_all_4_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_4_cdp_cdp
      module function var_all_1_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_1_cqp_cqp
      module function var_all_2_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_2_cqp_cqp
      module function var_all_3_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_3_cqp_cqp
      module function var_all_4_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), qp)
        mean = sum(x) / n

          res = sum(abs(x - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_4_cqp_cqp


      module function var_all_1_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_1_iint8_dp
      module function var_all_2_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_2_iint8_dp
      module function var_all_3_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_3_iint8_dp
      module function var_all_4_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_4_iint8_dp
      module function var_all_1_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_1_iint16_dp
      module function var_all_2_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_2_iint16_dp
      module function var_all_3_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_3_iint16_dp
      module function var_all_4_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_4_iint16_dp
      module function var_all_1_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_1_iint32_dp
      module function var_all_2_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_2_iint32_dp
      module function var_all_3_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_3_iint32_dp
      module function var_all_4_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_4_iint32_dp
      module function var_all_1_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_1_iint64_dp
      module function var_all_2_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_2_iint64_dp
      module function var_all_3_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_3_iint64_dp
      module function var_all_4_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)
        mean = sum(real(x, dp)) / n

        res = sum((real(x, dp) - mean)**2) / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_all_4_iint64_dp


      module function var_1_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        integer :: i
        real(sp) :: n
        real(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_rsp_rsp
      module function var_2_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n
        real(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_rsp_rsp
      module function var_3_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n
        real(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_rsp_rsp
      module function var_4_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(sp) :: n
        real(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i, :, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, i, :) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_rsp_rsp
      module function var_1_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_rdp_rdp
      module function var_2_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_rdp_rdp
      module function var_3_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_rdp_rdp
      module function var_4_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i, :, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, i, :) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_rdp_rdp
      module function var_1_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        integer :: i
        real(qp) :: n
        real(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_rqp_rqp
      module function var_2_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n
        real(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_rqp_rqp
      module function var_3_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n
        real(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_rqp_rqp
      module function var_4_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n
        real(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(i, :, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, i, :, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, i, :) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + (x(:, :, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_rqp_rqp
      module function var_1_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res

        integer :: i
        real(sp) :: n
        complex(sp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_csp_csp
      module function var_2_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n
        complex(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_csp_csp
      module function var_3_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n
        complex(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_csp_csp
      module function var_4_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(sp) :: n
        complex(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
            & 3), size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = 0._sp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i, :, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, i, :) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_csp_csp
      module function var_1_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        complex(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_cdp_cdp
      module function var_2_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        complex(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_cdp_cdp
      module function var_3_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        complex(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_cdp_cdp
      module function var_4_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n
        complex(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
            & 3), size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i, :, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, i, :) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_cdp_cdp
      module function var_1_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res

        integer :: i
        real(qp) :: n
        complex(qp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_cqp_cqp
      module function var_2_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n
        complex(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_cqp_cqp
      module function var_3_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n
        complex(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_cqp_cqp
      module function var_4_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n
        complex(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
            & 3), size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = 0._qp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(i, :, :, :) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, i, :, :) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, i, :) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(x, dim) / n
            do i = 1, size(x, dim)
                res = res + abs(x(:, :, :, i) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_cqp_cqp


      module function var_1_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_iint8_dp
      module function var_2_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_iint8_dp
      module function var_3_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_iint8_dp
      module function var_4_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i, :), dp) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_iint8_dp
      module function var_1_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_iint16_dp
      module function var_2_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_iint16_dp
      module function var_3_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_iint16_dp
      module function var_4_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i, :), dp) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_iint16_dp
      module function var_1_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_iint32_dp
      module function var_2_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_iint32_dp
      module function var_3_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_iint32_dp
      module function var_4_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i, :), dp) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_iint32_dp
      module function var_1_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_1_iint64_dp
      module function var_2_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_2_iint64_dp
      module function var_3_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_3_iint64_dp
      module function var_4_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = 0._dp
        select case(dim)
          case(1)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(i, :, :, :), dp) - mean)**2
            end do
          case(2)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, i, :, :), dp) - mean)**2
            end do
          case(3)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, i, :), dp) - mean)**2
            end do
          case(4)
            n = size(x, dim)
            mean = sum(real(x, dp), dim) / n
            do i = 1, size(x, dim)
              res = res + (real(x(:, :, :, i), dp) - mean)**2
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, optval(corrected, .true.)))

      end function var_4_iint64_dp


      module function var_mask_all_1_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_rsp_rsp
      module function var_mask_all_2_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_rsp_rsp
      module function var_mask_all_3_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_rsp_rsp
      module function var_mask_all_4_rsp_rsp(x, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        real(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_rsp_rsp
      module function var_mask_all_1_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_rdp_rdp
      module function var_mask_all_2_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_rdp_rdp
      module function var_mask_all_3_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_rdp_rdp
      module function var_mask_all_4_rdp_rdp(x, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        real(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_rdp_rdp
      module function var_mask_all_1_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_rqp_rqp
      module function var_mask_all_2_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_rqp_rqp
      module function var_mask_all_3_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_rqp_rqp
      module function var_mask_all_4_rqp_rqp(x, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        real(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum((x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_rqp_rqp
      module function var_mask_all_1_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_csp_csp
      module function var_mask_all_2_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_csp_csp
      module function var_mask_all_3_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_csp_csp
      module function var_mask_all_4_csp_csp(x, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        real(sp) :: n
        complex(sp) :: mean

        n = real(count(mask, kind = int64), sp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_csp_csp
      module function var_mask_all_1_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_cdp_cdp
      module function var_mask_all_2_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_cdp_cdp
      module function var_mask_all_3_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_cdp_cdp
      module function var_mask_all_4_cdp_cdp(x, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n
        complex(dp) :: mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_cdp_cdp
      module function var_mask_all_1_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_cqp_cqp
      module function var_mask_all_2_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_cqp_cqp
      module function var_mask_all_3_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_cqp_cqp
      module function var_mask_all_4_cqp_cqp(x, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        real(qp) :: n
        complex(qp) :: mean

        n = real(count(mask, kind = int64), qp)
        mean = sum(x, mask) / n

          res = sum(abs(x - mean)**2, mask) / (n -&
                merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_cqp_cqp


      module function var_mask_all_1_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_iint8_dp
      module function var_mask_all_2_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_iint8_dp
      module function var_mask_all_3_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_iint8_dp
      module function var_mask_all_4_iint8_dp(x, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_iint8_dp
      module function var_mask_all_1_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_iint16_dp
      module function var_mask_all_2_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_iint16_dp
      module function var_mask_all_3_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_iint16_dp
      module function var_mask_all_4_iint16_dp(x, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_iint16_dp
      module function var_mask_all_1_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_iint32_dp
      module function var_mask_all_2_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_iint32_dp
      module function var_mask_all_3_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_iint32_dp
      module function var_mask_all_4_iint32_dp(x, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_iint32_dp
      module function var_mask_all_1_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_1_iint64_dp
      module function var_mask_all_2_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_2_iint64_dp
      module function var_mask_all_3_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_3_iint64_dp
      module function var_mask_all_4_iint64_dp(x, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        real(dp) :: n, mean

        n = real(count(mask, kind = int64), dp)
        mean = sum(real(x, dp), mask) / n

        res = sum((real(x, dp) - mean)**2, mask) / (n -&
              merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_all_4_iint64_dp


      module function var_mask_1_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        integer :: i
        real(sp) :: n
        real(sp) :: mean

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i) - mean)**2,&
                                  0._sp,&
                                  mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_rsp_rsp
      module function var_mask_2_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :) - mean)**2,&
                                  0._sp,&
                                  mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i) - mean)**2,&
                                  0._sp,&
                                  mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_rsp_rsp
      module function var_mask_3_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :, :) - mean)**2,&
                                  0._sp,&
                                  mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i, :) - mean)**2,&
                                  0._sp,&
                                  mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, i) - mean)**2,&
                                  0._sp,&
                                  mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_rsp_rsp
      module function var_mask_4_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :, :, :) - mean)**2,&
                                  0._sp,&
                                  mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i, :, :) - mean)**2,&
                                  0._sp,&
                                  mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, i, :) - mean)**2,&
                                  0._sp,&
                                  mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, :, i) - mean)**2,&
                                  0._sp,&
                                  mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_rsp_rsp
      module function var_mask_1_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i) - mean)**2,&
                                  0._dp,&
                                  mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_rdp_rdp
      module function var_mask_2_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :) - mean)**2,&
                                  0._dp,&
                                  mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i) - mean)**2,&
                                  0._dp,&
                                  mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_rdp_rdp
      module function var_mask_3_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :, :) - mean)**2,&
                                  0._dp,&
                                  mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i, :) - mean)**2,&
                                  0._dp,&
                                  mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, i) - mean)**2,&
                                  0._dp,&
                                  mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_rdp_rdp
      module function var_mask_4_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :, :, :) - mean)**2,&
                                  0._dp,&
                                  mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i, :, :) - mean)**2,&
                                  0._dp,&
                                  mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, i, :) - mean)**2,&
                                  0._dp,&
                                  mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, :, i) - mean)**2,&
                                  0._dp,&
                                  mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_rdp_rdp
      module function var_mask_1_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        integer :: i
        real(qp) :: n
        real(qp) :: mean

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i) - mean)**2,&
                                  0._qp,&
                                  mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_rqp_rqp
      module function var_mask_2_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :) - mean)**2,&
                                  0._qp,&
                                  mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i) - mean)**2,&
                                  0._qp,&
                                  mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_rqp_rqp
      module function var_mask_3_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :, :) - mean)**2,&
                                  0._qp,&
                                  mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i, :) - mean)**2,&
                                  0._qp,&
                                  mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, i) - mean)**2,&
                                  0._qp,&
                                  mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_rqp_rqp
      module function var_mask_4_rqp_rqp(x, dim, mask, corrected) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(i, :, :, :) - mean)**2,&
                                  0._qp,&
                                  mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, i, :, :) - mean)**2,&
                                  0._qp,&
                                  mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, i, :) - mean)**2,&
                                  0._qp,&
                                  mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( (x(:, :, :, i) - mean)**2,&
                                  0._qp,&
                                  mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_rqp_rqp
      module function var_mask_1_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(sp) :: res

        integer :: i
        real(sp) :: n
        complex(sp) :: mean

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i) - mean)**2,&
                                  0._sp,&
                                  mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_csp_csp
      module function var_mask_2_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        complex(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :) - mean)**2,&
                                  0._sp,&
                                  mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i) - mean)**2,&
                                  0._sp,&
                                  mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_csp_csp
      module function var_mask_3_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        complex(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :, :) - mean)**2,&
                                  0._sp,&
                                  mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i, :) - mean)**2,&
                                  0._sp,&
                                  mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, i) - mean)**2,&
                                  0._sp,&
                                  mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_csp_csp
      module function var_mask_4_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        complex(sp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
            & 3), size(x, 4), mask=3<dim))

        res = 0._sp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :, :, :) - mean)**2,&
                                  0._sp,&
                                  mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i, :, :) - mean)**2,&
                                  0._sp,&
                                  mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, i, :) - mean)**2,&
                                  0._sp,&
                                  mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, :, i) - mean)**2,&
                                  0._sp,&
                                  mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_csp_csp
      module function var_mask_1_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        complex(dp) :: mean

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i) - mean)**2,&
                                  0._dp,&
                                  mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_cdp_cdp
      module function var_mask_2_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        complex(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :) - mean)**2,&
                                  0._dp,&
                                  mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i) - mean)**2,&
                                  0._dp,&
                                  mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_cdp_cdp
      module function var_mask_3_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        complex(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :, :) - mean)**2,&
                                  0._dp,&
                                  mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i, :) - mean)**2,&
                                  0._dp,&
                                  mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, i) - mean)**2,&
                                  0._dp,&
                                  mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_cdp_cdp
      module function var_mask_4_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        complex(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
            & 3), size(x, 4), mask=3<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :, :, :) - mean)**2,&
                                  0._dp,&
                                  mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i, :, :) - mean)**2,&
                                  0._dp,&
                                  mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, i, :) - mean)**2,&
                                  0._dp,&
                                  mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, :, i) - mean)**2,&
                                  0._dp,&
                                  mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_cdp_cdp
      module function var_mask_1_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(qp) :: res

        integer :: i
        real(qp) :: n
        complex(qp) :: mean

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i) - mean)**2,&
                                  0._qp,&
                                  mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_cqp_cqp
      module function var_mask_2_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        complex(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :) - mean)**2,&
                                  0._qp,&
                                  mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i) - mean)**2,&
                                  0._qp,&
                                  mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_cqp_cqp
      module function var_mask_3_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        complex(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :, :) - mean)**2,&
                                  0._qp,&
                                  mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i, :) - mean)**2,&
                                  0._qp,&
                                  mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, i) - mean)**2,&
                                  0._qp,&
                                  mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_cqp_cqp
      module function var_mask_4_cqp_cqp(x, dim, mask, corrected) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        complex(qp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
            & 3), size(x, 4), mask=3<dim))

        res = 0._qp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(i, :, :, :) - mean)**2,&
                                  0._qp,&
                                  mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, i, :, :) - mean)**2,&
                                  0._qp,&
                                  mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, i, :) - mean)**2,&
                                  0._qp,&
                                  mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(x, dim, mask) / n
            do i = 1, size(x, dim)
                res = res + merge( abs(x(:, :, :, i) - mean)**2,&
                                  0._qp,&
                                  mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_cqp_cqp


      module function var_mask_1_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i), dp) - mean)**2,&
                                  0._dp, mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_iint8_dp
      module function var_mask_2_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :), dp) - mean)**2,&
                                  0._dp, mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i), dp) - mean)**2,&
                                  0._dp, mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_iint8_dp
      module function var_mask_3_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_iint8_dp
      module function var_mask_4_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_iint8_dp
      module function var_mask_1_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i), dp) - mean)**2,&
                                  0._dp, mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_iint16_dp
      module function var_mask_2_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :), dp) - mean)**2,&
                                  0._dp, mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i), dp) - mean)**2,&
                                  0._dp, mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_iint16_dp
      module function var_mask_3_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_iint16_dp
      module function var_mask_4_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_iint16_dp
      module function var_mask_1_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i), dp) - mean)**2,&
                                  0._dp, mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_iint32_dp
      module function var_mask_2_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :), dp) - mean)**2,&
                                  0._dp, mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i), dp) - mean)**2,&
                                  0._dp, mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_iint32_dp
      module function var_mask_3_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_iint32_dp
      module function var_mask_4_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_iint32_dp
      module function var_mask_1_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp) :: mean

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i), dp) - mean)**2,&
                                  0._dp, mask(i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_1_iint64_dp
      module function var_mask_2_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :), dp) - mean)**2,&
                                  0._dp, mask(i, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i), dp) - mean)**2,&
                                  0._dp, mask(:, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_2_iint64_dp
      module function var_mask_3_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_3_iint64_dp
      module function var_mask_4_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp) :: mean(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        res = 0._dp
        select case(dim)
          case(1)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(i, :, :, :), dp) - mean)**2,&
                                  0._dp, mask(i, :, :, :))
            end do
          case(2)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, i, :, :), dp) - mean)**2,&
                                  0._dp, mask(:, i, :, :))
            end do
          case(3)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, i, :), dp) - mean)**2,&
                                  0._dp, mask(:, :, i, :))
            end do
          case(4)
            n = count(mask, dim)
            mean = sum(real(x, dp), dim, mask) / n
            do i = 1, size(x, dim)
              res = res + merge((real(x(:, :, :, i), dp) - mean)**2,&
                                  0._dp, mask(:, :, :, i))
            end do
          case default
            call error_stop("ERROR (var): wrong dimension")
        end select
        res = res / (n - merge(1, 0, (optval(corrected, .true.) .and. n > 0)))

      end function var_mask_4_iint64_dp

end submodule
