submodule (stdlib_stats) stdlib_stats_mean

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function mean_all_1_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_1_rsp_rsp
      module function mean_all_2_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_2_rsp_rsp
      module function mean_all_3_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_3_rsp_rsp
      module function mean_all_4_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_4_rsp_rsp
      module function mean_all_5_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_5_rsp_rsp
      module function mean_all_6_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_6_rsp_rsp
      module function mean_all_7_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_7_rsp_rsp
      module function mean_all_8_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_8_rsp_rsp
      module function mean_all_9_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_9_rsp_rsp
      module function mean_all_10_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_10_rsp_rsp
      module function mean_all_11_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_11_rsp_rsp
      module function mean_all_12_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_12_rsp_rsp
      module function mean_all_13_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_13_rsp_rsp
      module function mean_all_14_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_14_rsp_rsp
      module function mean_all_15_rsp_rsp (x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_15_rsp_rsp
      module function mean_all_1_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_1_rdp_rdp
      module function mean_all_2_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_2_rdp_rdp
      module function mean_all_3_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_3_rdp_rdp
      module function mean_all_4_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_4_rdp_rdp
      module function mean_all_5_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_5_rdp_rdp
      module function mean_all_6_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_6_rdp_rdp
      module function mean_all_7_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_7_rdp_rdp
      module function mean_all_8_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_8_rdp_rdp
      module function mean_all_9_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_9_rdp_rdp
      module function mean_all_10_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_10_rdp_rdp
      module function mean_all_11_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_11_rdp_rdp
      module function mean_all_12_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_12_rdp_rdp
      module function mean_all_13_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_13_rdp_rdp
      module function mean_all_14_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_14_rdp_rdp
      module function mean_all_15_rdp_rdp (x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_15_rdp_rdp
      module function mean_all_1_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_1_rqp_rqp
      module function mean_all_2_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_2_rqp_rqp
      module function mean_all_3_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_3_rqp_rqp
      module function mean_all_4_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_4_rqp_rqp
      module function mean_all_5_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_5_rqp_rqp
      module function mean_all_6_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_6_rqp_rqp
      module function mean_all_7_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_7_rqp_rqp
      module function mean_all_8_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_8_rqp_rqp
      module function mean_all_9_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_9_rqp_rqp
      module function mean_all_10_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_10_rqp_rqp
      module function mean_all_11_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_11_rqp_rqp
      module function mean_all_12_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_12_rqp_rqp
      module function mean_all_13_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_13_rqp_rqp
      module function mean_all_14_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_14_rqp_rqp
      module function mean_all_15_rqp_rqp (x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_15_rqp_rqp
      module function mean_all_1_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_1_csp_csp
      module function mean_all_2_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_2_csp_csp
      module function mean_all_3_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_3_csp_csp
      module function mean_all_4_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_4_csp_csp
      module function mean_all_5_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_5_csp_csp
      module function mean_all_6_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_6_csp_csp
      module function mean_all_7_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_7_csp_csp
      module function mean_all_8_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_8_csp_csp
      module function mean_all_9_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_9_csp_csp
      module function mean_all_10_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_10_csp_csp
      module function mean_all_11_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_11_csp_csp
      module function mean_all_12_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_12_csp_csp
      module function mean_all_13_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_13_csp_csp
      module function mean_all_14_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_14_csp_csp
      module function mean_all_15_csp_csp (x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), sp)

      end function mean_all_15_csp_csp
      module function mean_all_1_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_1_cdp_cdp
      module function mean_all_2_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_2_cdp_cdp
      module function mean_all_3_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_3_cdp_cdp
      module function mean_all_4_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_4_cdp_cdp
      module function mean_all_5_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_5_cdp_cdp
      module function mean_all_6_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_6_cdp_cdp
      module function mean_all_7_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_7_cdp_cdp
      module function mean_all_8_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_8_cdp_cdp
      module function mean_all_9_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_9_cdp_cdp
      module function mean_all_10_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_10_cdp_cdp
      module function mean_all_11_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_11_cdp_cdp
      module function mean_all_12_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_12_cdp_cdp
      module function mean_all_13_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_13_cdp_cdp
      module function mean_all_14_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_14_cdp_cdp
      module function mean_all_15_cdp_cdp (x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), dp)

      end function mean_all_15_cdp_cdp
      module function mean_all_1_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_1_cqp_cqp
      module function mean_all_2_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_2_cqp_cqp
      module function mean_all_3_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_3_cqp_cqp
      module function mean_all_4_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_4_cqp_cqp
      module function mean_all_5_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_5_cqp_cqp
      module function mean_all_6_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_6_cqp_cqp
      module function mean_all_7_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_7_cqp_cqp
      module function mean_all_8_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_8_cqp_cqp
      module function mean_all_9_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_9_cqp_cqp
      module function mean_all_10_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_10_cqp_cqp
      module function mean_all_11_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_11_cqp_cqp
      module function mean_all_12_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_12_cqp_cqp
      module function mean_all_13_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_13_cqp_cqp
      module function mean_all_14_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_14_cqp_cqp
      module function mean_all_15_cqp_cqp (x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        res = sum(x) / real(size(x, kind = int64), qp)

      end function mean_all_15_cqp_cqp

      module function mean_all_1_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_1_iint8_dp
      module function mean_all_2_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_2_iint8_dp
      module function mean_all_3_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_3_iint8_dp
      module function mean_all_4_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_4_iint8_dp
      module function mean_all_5_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_5_iint8_dp
      module function mean_all_6_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_6_iint8_dp
      module function mean_all_7_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_7_iint8_dp
      module function mean_all_8_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_8_iint8_dp
      module function mean_all_9_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_9_iint8_dp
      module function mean_all_10_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_10_iint8_dp
      module function mean_all_11_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_11_iint8_dp
      module function mean_all_12_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_12_iint8_dp
      module function mean_all_13_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_13_iint8_dp
      module function mean_all_14_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_14_iint8_dp
      module function mean_all_15_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_15_iint8_dp
      module function mean_all_1_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_1_iint16_dp
      module function mean_all_2_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_2_iint16_dp
      module function mean_all_3_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_3_iint16_dp
      module function mean_all_4_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_4_iint16_dp
      module function mean_all_5_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_5_iint16_dp
      module function mean_all_6_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_6_iint16_dp
      module function mean_all_7_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_7_iint16_dp
      module function mean_all_8_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_8_iint16_dp
      module function mean_all_9_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_9_iint16_dp
      module function mean_all_10_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_10_iint16_dp
      module function mean_all_11_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_11_iint16_dp
      module function mean_all_12_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_12_iint16_dp
      module function mean_all_13_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_13_iint16_dp
      module function mean_all_14_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_14_iint16_dp
      module function mean_all_15_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_15_iint16_dp
      module function mean_all_1_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_1_iint32_dp
      module function mean_all_2_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_2_iint32_dp
      module function mean_all_3_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_3_iint32_dp
      module function mean_all_4_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_4_iint32_dp
      module function mean_all_5_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_5_iint32_dp
      module function mean_all_6_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_6_iint32_dp
      module function mean_all_7_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_7_iint32_dp
      module function mean_all_8_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_8_iint32_dp
      module function mean_all_9_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_9_iint32_dp
      module function mean_all_10_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_10_iint32_dp
      module function mean_all_11_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_11_iint32_dp
      module function mean_all_12_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_12_iint32_dp
      module function mean_all_13_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_13_iint32_dp
      module function mean_all_14_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_14_iint32_dp
      module function mean_all_15_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_15_iint32_dp
      module function mean_all_1_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_1_iint64_dp
      module function mean_all_2_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_2_iint64_dp
      module function mean_all_3_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_3_iint64_dp
      module function mean_all_4_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_4_iint64_dp
      module function mean_all_5_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_5_iint64_dp
      module function mean_all_6_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_6_iint64_dp
      module function mean_all_7_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_7_iint64_dp
      module function mean_all_8_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_8_iint64_dp
      module function mean_all_9_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_9_iint64_dp
      module function mean_all_10_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_10_iint64_dp
      module function mean_all_11_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_11_iint64_dp
      module function mean_all_12_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_12_iint64_dp
      module function mean_all_13_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_13_iint64_dp
      module function mean_all_14_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_14_iint64_dp
      module function mean_all_15_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        res = sum(real(x, dp)) / real(size(x, kind = int64), dp)

      end function mean_all_15_iint64_dp


      module function mean_1_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_rsp_rsp
      module function mean_2_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_rsp_rsp
      module function mean_3_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_rsp_rsp
      module function mean_4_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_rsp_rsp
      module function mean_5_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_rsp_rsp
      module function mean_6_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_rsp_rsp
      module function mean_7_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_rsp_rsp
      module function mean_8_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_rsp_rsp
      module function mean_9_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_rsp_rsp
      module function mean_10_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_rsp_rsp
      module function mean_11_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_rsp_rsp
      module function mean_12_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_rsp_rsp
      module function mean_13_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_rsp_rsp
      module function mean_14_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_rsp_rsp
      module function mean_15_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_rsp_rsp
      module function mean_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_rdp_rdp
      module function mean_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_rdp_rdp
      module function mean_3_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_rdp_rdp
      module function mean_4_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_rdp_rdp
      module function mean_5_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_rdp_rdp
      module function mean_6_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_rdp_rdp
      module function mean_7_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_rdp_rdp
      module function mean_8_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_rdp_rdp
      module function mean_9_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_rdp_rdp
      module function mean_10_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_rdp_rdp
      module function mean_11_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_rdp_rdp
      module function mean_12_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_rdp_rdp
      module function mean_13_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_rdp_rdp
      module function mean_14_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_rdp_rdp
      module function mean_15_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_rdp_rdp
      module function mean_1_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_rqp_rqp
      module function mean_2_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_rqp_rqp
      module function mean_3_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_rqp_rqp
      module function mean_4_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_rqp_rqp
      module function mean_5_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_rqp_rqp
      module function mean_6_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_rqp_rqp
      module function mean_7_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_rqp_rqp
      module function mean_8_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_rqp_rqp
      module function mean_9_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_rqp_rqp
      module function mean_10_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_rqp_rqp
      module function mean_11_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_rqp_rqp
      module function mean_12_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_rqp_rqp
      module function mean_13_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_rqp_rqp
      module function mean_14_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_rqp_rqp
      module function mean_15_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_rqp_rqp
      module function mean_1_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_csp_csp
      module function mean_2_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_csp_csp
      module function mean_3_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_csp_csp
      module function mean_4_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_csp_csp
      module function mean_5_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_csp_csp
      module function mean_6_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_csp_csp
      module function mean_7_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_csp_csp
      module function mean_8_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_csp_csp
      module function mean_9_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_csp_csp
      module function mean_10_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_csp_csp
      module function mean_11_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_csp_csp
      module function mean_12_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_csp_csp
      module function mean_13_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_csp_csp
      module function mean_14_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_csp_csp
      module function mean_15_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim) / real(size(x, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_csp_csp
      module function mean_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_cdp_cdp
      module function mean_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_cdp_cdp
      module function mean_3_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_cdp_cdp
      module function mean_4_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_cdp_cdp
      module function mean_5_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_cdp_cdp
      module function mean_6_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_cdp_cdp
      module function mean_7_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_cdp_cdp
      module function mean_8_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_cdp_cdp
      module function mean_9_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_cdp_cdp
      module function mean_10_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_cdp_cdp
      module function mean_11_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_cdp_cdp
      module function mean_12_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_cdp_cdp
      module function mean_13_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_cdp_cdp
      module function mean_14_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_cdp_cdp
      module function mean_15_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_cdp_cdp
      module function mean_1_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_cqp_cqp
      module function mean_2_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_cqp_cqp
      module function mean_3_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_cqp_cqp
      module function mean_4_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_cqp_cqp
      module function mean_5_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_cqp_cqp
      module function mean_6_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_cqp_cqp
      module function mean_7_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_cqp_cqp
      module function mean_8_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_cqp_cqp
      module function mean_9_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_cqp_cqp
      module function mean_10_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_cqp_cqp
      module function mean_11_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_cqp_cqp
      module function mean_12_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_cqp_cqp
      module function mean_13_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_cqp_cqp
      module function mean_14_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_cqp_cqp
      module function mean_15_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim) / real(size(x, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_cqp_cqp


      module function mean_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_iint8_dp
      module function mean_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_iint8_dp
      module function mean_3_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_iint8_dp
      module function mean_4_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_iint8_dp
      module function mean_5_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_iint8_dp
      module function mean_6_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_iint8_dp
      module function mean_7_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_iint8_dp
      module function mean_8_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_iint8_dp
      module function mean_9_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_iint8_dp
      module function mean_10_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_iint8_dp
      module function mean_11_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_iint8_dp
      module function mean_12_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_iint8_dp
      module function mean_13_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_iint8_dp
      module function mean_14_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_iint8_dp
      module function mean_15_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_iint8_dp
      module function mean_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_iint16_dp
      module function mean_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_iint16_dp
      module function mean_3_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_iint16_dp
      module function mean_4_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_iint16_dp
      module function mean_5_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_iint16_dp
      module function mean_6_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_iint16_dp
      module function mean_7_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_iint16_dp
      module function mean_8_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_iint16_dp
      module function mean_9_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_iint16_dp
      module function mean_10_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_iint16_dp
      module function mean_11_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_iint16_dp
      module function mean_12_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_iint16_dp
      module function mean_13_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_iint16_dp
      module function mean_14_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_iint16_dp
      module function mean_15_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_iint16_dp
      module function mean_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_iint32_dp
      module function mean_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_iint32_dp
      module function mean_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_iint32_dp
      module function mean_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_iint32_dp
      module function mean_5_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_iint32_dp
      module function mean_6_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_iint32_dp
      module function mean_7_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_iint32_dp
      module function mean_8_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_iint32_dp
      module function mean_9_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_iint32_dp
      module function mean_10_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_iint32_dp
      module function mean_11_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_iint32_dp
      module function mean_12_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_iint32_dp
      module function mean_13_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_iint32_dp
      module function mean_14_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_iint32_dp
      module function mean_15_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_iint32_dp
      module function mean_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_1_iint64_dp
      module function mean_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_2_iint64_dp
      module function mean_3_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_3_iint64_dp
      module function mean_4_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_4_iint64_dp
      module function mean_5_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_5_iint64_dp
      module function mean_6_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_6_iint64_dp
      module function mean_7_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_7_iint64_dp
      module function mean_8_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_8_iint64_dp
      module function mean_9_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_9_iint64_dp
      module function mean_10_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_10_iint64_dp
      module function mean_11_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_11_iint64_dp
      module function mean_12_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_12_iint64_dp
      module function mean_13_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_13_iint64_dp
      module function mean_14_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_14_iint64_dp
      module function mean_15_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim) / real(size(x, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_15_iint64_dp

      module function mean_mask_all_1_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_1_rsp_rsp
      module function mean_mask_all_2_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_2_rsp_rsp
      module function mean_mask_all_3_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_3_rsp_rsp
      module function mean_mask_all_4_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_4_rsp_rsp
      module function mean_mask_all_5_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_5_rsp_rsp
      module function mean_mask_all_6_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_6_rsp_rsp
      module function mean_mask_all_7_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_7_rsp_rsp
      module function mean_mask_all_8_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_8_rsp_rsp
      module function mean_mask_all_9_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_9_rsp_rsp
      module function mean_mask_all_10_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_10_rsp_rsp
      module function mean_mask_all_11_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_11_rsp_rsp
      module function mean_mask_all_12_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_12_rsp_rsp
      module function mean_mask_all_13_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_13_rsp_rsp
      module function mean_mask_all_14_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_14_rsp_rsp
      module function mean_mask_all_15_rsp_rsp(x, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_15_rsp_rsp
      module function mean_mask_all_1_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_rdp_rdp
      module function mean_mask_all_2_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_rdp_rdp
      module function mean_mask_all_3_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_rdp_rdp
      module function mean_mask_all_4_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_rdp_rdp
      module function mean_mask_all_5_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_5_rdp_rdp
      module function mean_mask_all_6_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_6_rdp_rdp
      module function mean_mask_all_7_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_7_rdp_rdp
      module function mean_mask_all_8_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_8_rdp_rdp
      module function mean_mask_all_9_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_9_rdp_rdp
      module function mean_mask_all_10_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_10_rdp_rdp
      module function mean_mask_all_11_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_11_rdp_rdp
      module function mean_mask_all_12_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_12_rdp_rdp
      module function mean_mask_all_13_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_13_rdp_rdp
      module function mean_mask_all_14_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_14_rdp_rdp
      module function mean_mask_all_15_rdp_rdp(x, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_15_rdp_rdp
      module function mean_mask_all_1_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_1_rqp_rqp
      module function mean_mask_all_2_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_2_rqp_rqp
      module function mean_mask_all_3_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_3_rqp_rqp
      module function mean_mask_all_4_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_4_rqp_rqp
      module function mean_mask_all_5_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_5_rqp_rqp
      module function mean_mask_all_6_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_6_rqp_rqp
      module function mean_mask_all_7_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_7_rqp_rqp
      module function mean_mask_all_8_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_8_rqp_rqp
      module function mean_mask_all_9_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_9_rqp_rqp
      module function mean_mask_all_10_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_10_rqp_rqp
      module function mean_mask_all_11_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_11_rqp_rqp
      module function mean_mask_all_12_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_12_rqp_rqp
      module function mean_mask_all_13_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_13_rqp_rqp
      module function mean_mask_all_14_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_14_rqp_rqp
      module function mean_mask_all_15_rqp_rqp(x, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_15_rqp_rqp
      module function mean_mask_all_1_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_1_csp_csp
      module function mean_mask_all_2_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_2_csp_csp
      module function mean_mask_all_3_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_3_csp_csp
      module function mean_mask_all_4_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_4_csp_csp
      module function mean_mask_all_5_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_5_csp_csp
      module function mean_mask_all_6_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_6_csp_csp
      module function mean_mask_all_7_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_7_csp_csp
      module function mean_mask_all_8_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_8_csp_csp
      module function mean_mask_all_9_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_9_csp_csp
      module function mean_mask_all_10_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_10_csp_csp
      module function mean_mask_all_11_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_11_csp_csp
      module function mean_mask_all_12_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_12_csp_csp
      module function mean_mask_all_13_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_13_csp_csp
      module function mean_mask_all_14_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_14_csp_csp
      module function mean_mask_all_15_csp_csp(x, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), sp)

      end function mean_mask_all_15_csp_csp
      module function mean_mask_all_1_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_cdp_cdp
      module function mean_mask_all_2_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_cdp_cdp
      module function mean_mask_all_3_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_cdp_cdp
      module function mean_mask_all_4_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_cdp_cdp
      module function mean_mask_all_5_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_5_cdp_cdp
      module function mean_mask_all_6_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_6_cdp_cdp
      module function mean_mask_all_7_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_7_cdp_cdp
      module function mean_mask_all_8_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_8_cdp_cdp
      module function mean_mask_all_9_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_9_cdp_cdp
      module function mean_mask_all_10_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_10_cdp_cdp
      module function mean_mask_all_11_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_11_cdp_cdp
      module function mean_mask_all_12_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_12_cdp_cdp
      module function mean_mask_all_13_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_13_cdp_cdp
      module function mean_mask_all_14_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_14_cdp_cdp
      module function mean_mask_all_15_cdp_cdp(x, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_15_cdp_cdp
      module function mean_mask_all_1_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_1_cqp_cqp
      module function mean_mask_all_2_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_2_cqp_cqp
      module function mean_mask_all_3_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_3_cqp_cqp
      module function mean_mask_all_4_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_4_cqp_cqp
      module function mean_mask_all_5_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_5_cqp_cqp
      module function mean_mask_all_6_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_6_cqp_cqp
      module function mean_mask_all_7_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_7_cqp_cqp
      module function mean_mask_all_8_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_8_cqp_cqp
      module function mean_mask_all_9_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_9_cqp_cqp
      module function mean_mask_all_10_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_10_cqp_cqp
      module function mean_mask_all_11_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_11_cqp_cqp
      module function mean_mask_all_12_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_12_cqp_cqp
      module function mean_mask_all_13_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_13_cqp_cqp
      module function mean_mask_all_14_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_14_cqp_cqp
      module function mean_mask_all_15_cqp_cqp(x, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res

        res = sum(x, mask) / real(count(mask, kind = int64), qp)

      end function mean_mask_all_15_cqp_cqp


      module function mean_mask_all_1_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_iint8_dp
      module function mean_mask_all_2_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_iint8_dp
      module function mean_mask_all_3_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_iint8_dp
      module function mean_mask_all_4_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_iint8_dp
      module function mean_mask_all_5_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_5_iint8_dp
      module function mean_mask_all_6_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_6_iint8_dp
      module function mean_mask_all_7_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_7_iint8_dp
      module function mean_mask_all_8_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_8_iint8_dp
      module function mean_mask_all_9_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_9_iint8_dp
      module function mean_mask_all_10_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_10_iint8_dp
      module function mean_mask_all_11_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_11_iint8_dp
      module function mean_mask_all_12_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_12_iint8_dp
      module function mean_mask_all_13_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_13_iint8_dp
      module function mean_mask_all_14_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_14_iint8_dp
      module function mean_mask_all_15_iint8_dp(x, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_15_iint8_dp
      module function mean_mask_all_1_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_iint16_dp
      module function mean_mask_all_2_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_iint16_dp
      module function mean_mask_all_3_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_iint16_dp
      module function mean_mask_all_4_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_iint16_dp
      module function mean_mask_all_5_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_5_iint16_dp
      module function mean_mask_all_6_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_6_iint16_dp
      module function mean_mask_all_7_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_7_iint16_dp
      module function mean_mask_all_8_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_8_iint16_dp
      module function mean_mask_all_9_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_9_iint16_dp
      module function mean_mask_all_10_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_10_iint16_dp
      module function mean_mask_all_11_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_11_iint16_dp
      module function mean_mask_all_12_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_12_iint16_dp
      module function mean_mask_all_13_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_13_iint16_dp
      module function mean_mask_all_14_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_14_iint16_dp
      module function mean_mask_all_15_iint16_dp(x, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_15_iint16_dp
      module function mean_mask_all_1_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_iint32_dp
      module function mean_mask_all_2_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_iint32_dp
      module function mean_mask_all_3_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_iint32_dp
      module function mean_mask_all_4_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_iint32_dp
      module function mean_mask_all_5_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_5_iint32_dp
      module function mean_mask_all_6_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_6_iint32_dp
      module function mean_mask_all_7_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_7_iint32_dp
      module function mean_mask_all_8_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_8_iint32_dp
      module function mean_mask_all_9_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_9_iint32_dp
      module function mean_mask_all_10_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_10_iint32_dp
      module function mean_mask_all_11_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_11_iint32_dp
      module function mean_mask_all_12_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_12_iint32_dp
      module function mean_mask_all_13_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_13_iint32_dp
      module function mean_mask_all_14_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_14_iint32_dp
      module function mean_mask_all_15_iint32_dp(x, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_15_iint32_dp
      module function mean_mask_all_1_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:)
        logical, intent(in) :: mask(:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_1_iint64_dp
      module function mean_mask_all_2_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        logical, intent(in) :: mask(:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_2_iint64_dp
      module function mean_mask_all_3_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_3_iint64_dp
      module function mean_mask_all_4_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_4_iint64_dp
      module function mean_mask_all_5_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_5_iint64_dp
      module function mean_mask_all_6_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_6_iint64_dp
      module function mean_mask_all_7_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_7_iint64_dp
      module function mean_mask_all_8_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_8_iint64_dp
      module function mean_mask_all_9_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_9_iint64_dp
      module function mean_mask_all_10_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_10_iint64_dp
      module function mean_mask_all_11_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_11_iint64_dp
      module function mean_mask_all_12_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_12_iint64_dp
      module function mean_mask_all_13_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_13_iint64_dp
      module function mean_mask_all_14_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_14_iint64_dp
      module function mean_mask_all_15_iint64_dp(x, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res

        res = sum(real(x, dp), mask) / real(count(mask, kind = int64), dp)

      end function mean_mask_all_15_iint64_dp

      module function  mean_mask_1_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(sp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_rsp_rsp
      module function  mean_mask_2_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_rsp_rsp
      module function  mean_mask_3_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_rsp_rsp
      module function  mean_mask_4_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_rsp_rsp
      module function  mean_mask_5_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_rsp_rsp
      module function  mean_mask_6_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_rsp_rsp
      module function  mean_mask_7_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_rsp_rsp
      module function  mean_mask_8_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_rsp_rsp
      module function  mean_mask_9_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_rsp_rsp
      module function  mean_mask_10_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_rsp_rsp
      module function  mean_mask_11_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_rsp_rsp
      module function  mean_mask_12_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_rsp_rsp
      module function  mean_mask_13_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_rsp_rsp
      module function  mean_mask_14_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_rsp_rsp
      module function  mean_mask_15_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_rsp_rsp
      module function  mean_mask_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_rdp_rdp
      module function  mean_mask_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_rdp_rdp
      module function  mean_mask_3_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_rdp_rdp
      module function  mean_mask_4_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_rdp_rdp
      module function  mean_mask_5_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_rdp_rdp
      module function  mean_mask_6_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_rdp_rdp
      module function  mean_mask_7_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_rdp_rdp
      module function  mean_mask_8_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_rdp_rdp
      module function  mean_mask_9_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_rdp_rdp
      module function  mean_mask_10_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_rdp_rdp
      module function  mean_mask_11_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_rdp_rdp
      module function  mean_mask_12_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_rdp_rdp
      module function  mean_mask_13_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_rdp_rdp
      module function  mean_mask_14_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_rdp_rdp
      module function  mean_mask_15_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_rdp_rdp
      module function  mean_mask_1_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(qp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_rqp_rqp
      module function  mean_mask_2_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_rqp_rqp
      module function  mean_mask_3_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_rqp_rqp
      module function  mean_mask_4_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_rqp_rqp
      module function  mean_mask_5_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_rqp_rqp
      module function  mean_mask_6_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_rqp_rqp
      module function  mean_mask_7_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_rqp_rqp
      module function  mean_mask_8_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_rqp_rqp
      module function  mean_mask_9_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_rqp_rqp
      module function  mean_mask_10_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_rqp_rqp
      module function  mean_mask_11_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_rqp_rqp
      module function  mean_mask_12_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_rqp_rqp
      module function  mean_mask_13_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_rqp_rqp
      module function  mean_mask_14_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_rqp_rqp
      module function  mean_mask_15_rqp_rqp(x, dim, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_rqp_rqp
      module function  mean_mask_1_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        complex(sp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_csp_csp
      module function  mean_mask_2_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_csp_csp
      module function  mean_mask_3_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_csp_csp
      module function  mean_mask_4_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_csp_csp
      module function  mean_mask_5_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_csp_csp
      module function  mean_mask_6_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_csp_csp
      module function  mean_mask_7_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_csp_csp
      module function  mean_mask_8_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_csp_csp
      module function  mean_mask_9_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_csp_csp
      module function  mean_mask_10_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_csp_csp
      module function  mean_mask_11_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_csp_csp
      module function  mean_mask_12_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_csp_csp
      module function  mean_mask_13_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_csp_csp
      module function  mean_mask_14_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_csp_csp
      module function  mean_mask_15_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim, mask) / real(count(mask, dim), sp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_csp_csp
      module function  mean_mask_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        complex(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_cdp_cdp
      module function  mean_mask_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_cdp_cdp
      module function  mean_mask_3_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_cdp_cdp
      module function  mean_mask_4_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_cdp_cdp
      module function  mean_mask_5_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_cdp_cdp
      module function  mean_mask_6_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_cdp_cdp
      module function  mean_mask_7_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_cdp_cdp
      module function  mean_mask_8_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_cdp_cdp
      module function  mean_mask_9_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_cdp_cdp
      module function  mean_mask_10_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_cdp_cdp
      module function  mean_mask_11_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_cdp_cdp
      module function  mean_mask_12_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_cdp_cdp
      module function  mean_mask_13_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_cdp_cdp
      module function  mean_mask_14_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_cdp_cdp
      module function  mean_mask_15_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_cdp_cdp
      module function  mean_mask_1_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        complex(qp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_cqp_cqp
      module function  mean_mask_2_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_cqp_cqp
      module function  mean_mask_3_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_cqp_cqp
      module function  mean_mask_4_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_cqp_cqp
      module function  mean_mask_5_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_cqp_cqp
      module function  mean_mask_6_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_cqp_cqp
      module function  mean_mask_7_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_cqp_cqp
      module function  mean_mask_8_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_cqp_cqp
      module function  mean_mask_9_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_cqp_cqp
      module function  mean_mask_10_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_cqp_cqp
      module function  mean_mask_11_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_cqp_cqp
      module function  mean_mask_12_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_cqp_cqp
      module function  mean_mask_13_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_cqp_cqp
      module function  mean_mask_14_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_cqp_cqp
      module function  mean_mask_15_cqp_cqp(x, dim, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(x, dim, mask) / real(count(mask, dim), qp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_cqp_cqp


      module function mean_mask_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_iint8_dp
      module function mean_mask_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_iint8_dp
      module function mean_mask_3_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_iint8_dp
      module function mean_mask_4_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_iint8_dp
      module function mean_mask_5_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_iint8_dp
      module function mean_mask_6_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_iint8_dp
      module function mean_mask_7_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_iint8_dp
      module function mean_mask_8_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_iint8_dp
      module function mean_mask_9_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_iint8_dp
      module function mean_mask_10_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_iint8_dp
      module function mean_mask_11_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_iint8_dp
      module function mean_mask_12_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_iint8_dp
      module function mean_mask_13_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_iint8_dp
      module function mean_mask_14_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_iint8_dp
      module function mean_mask_15_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_iint8_dp
      module function mean_mask_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_iint16_dp
      module function mean_mask_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_iint16_dp
      module function mean_mask_3_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_iint16_dp
      module function mean_mask_4_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_iint16_dp
      module function mean_mask_5_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_iint16_dp
      module function mean_mask_6_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_iint16_dp
      module function mean_mask_7_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_iint16_dp
      module function mean_mask_8_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_iint16_dp
      module function mean_mask_9_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_iint16_dp
      module function mean_mask_10_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_iint16_dp
      module function mean_mask_11_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_iint16_dp
      module function mean_mask_12_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_iint16_dp
      module function mean_mask_13_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_iint16_dp
      module function mean_mask_14_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_iint16_dp
      module function mean_mask_15_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_iint16_dp
      module function mean_mask_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_iint32_dp
      module function mean_mask_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_iint32_dp
      module function mean_mask_3_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_iint32_dp
      module function mean_mask_4_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_iint32_dp
      module function mean_mask_5_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_iint32_dp
      module function mean_mask_6_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_iint32_dp
      module function mean_mask_7_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_iint32_dp
      module function mean_mask_8_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_iint32_dp
      module function mean_mask_9_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_iint32_dp
      module function mean_mask_10_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_iint32_dp
      module function mean_mask_11_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_iint32_dp
      module function mean_mask_12_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_iint32_dp
      module function mean_mask_13_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_iint32_dp
      module function mean_mask_14_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_iint32_dp
      module function mean_mask_15_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_iint32_dp
      module function mean_mask_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res

        if (dim >= 1 .and. dim <= 1) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_1_iint64_dp
      module function mean_mask_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_2_iint64_dp
      module function mean_mask_3_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_3_iint64_dp
      module function mean_mask_4_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_4_iint64_dp
      module function mean_mask_5_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        if (dim >= 1 .and. dim <= 5) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_5_iint64_dp
      module function mean_mask_6_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        if (dim >= 1 .and. dim <= 6) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_6_iint64_dp
      module function mean_mask_7_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        if (dim >= 1 .and. dim <= 7) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_7_iint64_dp
      module function mean_mask_8_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        if (dim >= 1 .and. dim <= 8) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_8_iint64_dp
      module function mean_mask_9_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        if (dim >= 1 .and. dim <= 9) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_9_iint64_dp
      module function mean_mask_10_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        if (dim >= 1 .and. dim <= 10) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_10_iint64_dp
      module function mean_mask_11_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        if (dim >= 1 .and. dim <= 11) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_11_iint64_dp
      module function mean_mask_12_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        if (dim >= 1 .and. dim <= 12) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_12_iint64_dp
      module function mean_mask_13_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        if (dim >= 1 .and. dim <= 13) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_13_iint64_dp
      module function mean_mask_14_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        if (dim >= 1 .and. dim <= 14) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_14_iint64_dp
      module function mean_mask_15_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        if (dim >= 1 .and. dim <= 15) then
          res = sum(real(x, dp), dim, mask) / real(count(mask, dim), dp)
        else
          call error_stop("ERROR (mean): wrong dimension")
        end if

      end function mean_mask_15_iint64_dp

end submodule
