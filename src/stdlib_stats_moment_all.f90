submodule (stdlib_stats) stdlib_stats_moment_all

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function moment_all_1_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_1_rsp_rsp
      module function moment_all_2_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_2_rsp_rsp
      module function moment_all_3_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_3_rsp_rsp
      module function moment_all_4_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_4_rsp_rsp
      module function moment_all_1_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_1_rdp_rdp
      module function moment_all_2_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_2_rdp_rdp
      module function moment_all_3_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_3_rdp_rdp
      module function moment_all_4_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_4_rdp_rdp
      module function moment_all_1_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_1_csp_csp
      module function moment_all_2_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_2_csp_csp
      module function moment_all_3_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_3_csp_csp
      module function moment_all_4_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_4_csp_csp
      module function moment_all_1_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_1_cdp_cdp
      module function moment_all_2_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_2_cdp_cdp
      module function moment_all_3_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_3_cdp_cdp
      module function moment_all_4_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((x - center_)**order) / n

      end function moment_all_4_cdp_cdp


      module function moment_all_1_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_1_iint8_dp
      module function moment_all_2_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_2_iint8_dp
      module function moment_all_3_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_3_iint8_dp
      module function moment_all_4_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_4_iint8_dp
      module function moment_all_1_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_1_iint16_dp
      module function moment_all_2_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_2_iint16_dp
      module function moment_all_3_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_3_iint16_dp
      module function moment_all_4_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_4_iint16_dp
      module function moment_all_1_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_1_iint32_dp
      module function moment_all_2_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_2_iint32_dp
      module function moment_all_3_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_3_iint32_dp
      module function moment_all_4_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_4_iint32_dp
      module function moment_all_1_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_1_iint64_dp
      module function moment_all_2_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_2_iint64_dp
      module function moment_all_3_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_3_iint64_dp
      module function moment_all_4_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x)
        end if
        res = sum((real(x, dp) - center_)**order) / n

      end function moment_all_4_iint64_dp


      module function moment_mask_all_1_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_1_rsp_rsp
      module function moment_mask_all_2_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_2_rsp_rsp
      module function moment_mask_all_3_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_3_rsp_rsp
      module function moment_mask_all_4_rsp_rsp(x, order, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res

        real(sp) :: n
        real(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_4_rsp_rsp
      module function moment_mask_all_1_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_1_rdp_rdp
      module function moment_mask_all_2_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_2_rdp_rdp
      module function moment_mask_all_3_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_3_rdp_rdp
      module function moment_mask_all_4_rdp_rdp(x, order, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_4_rdp_rdp
      module function moment_mask_all_1_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_1_csp_csp
      module function moment_mask_all_2_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_2_csp_csp
      module function moment_mask_all_3_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_3_csp_csp
      module function moment_mask_all_4_csp_csp(x, order, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        complex(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        complex(sp) :: res

        real(sp) :: n
        complex(sp) :: center_

        n = real(count(mask, kind = int64), sp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_4_csp_csp
      module function moment_mask_all_1_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_1_cdp_cdp
      module function moment_mask_all_2_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_2_cdp_cdp
      module function moment_mask_all_3_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_3_cdp_cdp
      module function moment_mask_all_4_cdp_cdp(x, order, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        complex(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res

        real(dp) :: n
        complex(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((x - center_)**order, mask) / n

      end function moment_mask_all_4_cdp_cdp


      module function moment_mask_all_1_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_1_iint8_dp
      module function moment_mask_all_2_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_2_iint8_dp
      module function moment_mask_all_3_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_3_iint8_dp
      module function moment_mask_all_4_iint8_dp(x, order, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_4_iint8_dp
      module function moment_mask_all_1_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_1_iint16_dp
      module function moment_mask_all_2_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_2_iint16_dp
      module function moment_mask_all_3_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_3_iint16_dp
      module function moment_mask_all_4_iint16_dp(x, order, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_4_iint16_dp
      module function moment_mask_all_1_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_1_iint32_dp
      module function moment_mask_all_2_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_2_iint32_dp
      module function moment_mask_all_3_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_3_iint32_dp
      module function moment_mask_all_4_iint32_dp(x, order, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_4_iint32_dp
      module function moment_mask_all_1_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_1_iint64_dp
      module function moment_mask_all_2_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_2_iint64_dp
      module function moment_mask_all_3_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_3_iint64_dp
      module function moment_mask_all_4_iint64_dp(x, order, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        real(dp),intent(in), optional :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        real(dp) :: n
        real(dp) :: center_

        n = real(count(mask, kind = int64), dp)

        if (present(center)) then
          center_ = center
        else
          center_ = mean(x, mask)
        end if
        res = sum((real(x, dp) - center_)**order, mask) / n

      end function moment_mask_all_4_iint64_dp

end submodule
