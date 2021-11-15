submodule (stdlib_stats) stdlib_stats_moment_scalar

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function moment_scalar_2_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in) :: center
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_rsp_rsp
      module function moment_scalar_3_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in) :: center
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_rsp_rsp
      module function moment_scalar_4_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in) :: center
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_rsp_rsp
      module function moment_scalar_2_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_rdp_rdp
      module function moment_scalar_3_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_rdp_rdp
      module function moment_scalar_4_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_rdp_rdp
      module function moment_scalar_2_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in) :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_csp_csp
      module function moment_scalar_3_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in) :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_csp_csp
      module function moment_scalar_4_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in) :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_csp_csp
      module function moment_scalar_2_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in) :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_cdp_cdp
      module function moment_scalar_3_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in) :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_cdp_cdp
      module function moment_scalar_4_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in) :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_cdp_cdp

      module function moment_scalar_2_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_iint8_dp
      module function moment_scalar_3_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_iint8_dp
      module function moment_scalar_4_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_iint8_dp
      module function moment_scalar_2_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_iint16_dp
      module function moment_scalar_3_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_iint16_dp
      module function moment_scalar_4_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_iint16_dp
      module function moment_scalar_2_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_iint32_dp
      module function moment_scalar_3_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_iint32_dp
      module function moment_scalar_4_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_iint32_dp
      module function moment_scalar_2_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 2) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_2_iint64_dp
      module function moment_scalar_3_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 3) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_3_iint64_dp
      module function moment_scalar_4_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in) :: center
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        if (dim >= 1 .and. dim <= 4) then
          res = sum( (real(x, dp) - center)**order, dim) / size(x, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_scalar_4_iint64_dp


      module function moment_mask_scalar_2_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_rsp_rsp
      module function moment_mask_scalar_3_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_rsp_rsp
      module function moment_mask_scalar_4_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_rsp_rsp
      module function moment_mask_scalar_2_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_rdp_rdp
      module function moment_mask_scalar_3_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_rdp_rdp
      module function moment_mask_scalar_4_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_rdp_rdp
      module function moment_mask_scalar_2_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_csp_csp
      module function moment_mask_scalar_3_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_csp_csp
      module function moment_mask_scalar_4_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_csp_csp
      module function moment_mask_scalar_2_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_cdp_cdp
      module function moment_mask_scalar_3_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_cdp_cdp
      module function moment_mask_scalar_4_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum((x - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_cdp_cdp


      module function moment_mask_scalar_2_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_iint8_dp
      module function moment_mask_scalar_3_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_iint8_dp
      module function moment_mask_scalar_4_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_iint8_dp
      module function moment_mask_scalar_2_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_iint16_dp
      module function moment_mask_scalar_3_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_iint16_dp
      module function moment_mask_scalar_4_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_iint16_dp
      module function moment_mask_scalar_2_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_iint32_dp
      module function moment_mask_scalar_3_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_iint32_dp
      module function moment_mask_scalar_4_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_iint32_dp
      module function moment_mask_scalar_2_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        if (dim >= 1 .and. dim <= 2) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_2_iint64_dp
      module function moment_mask_scalar_3_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        if (dim >= 1 .and. dim <= 3) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_3_iint64_dp
      module function moment_mask_scalar_4_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in) :: center
        logical, intent(in) :: mask(:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        if (dim >= 1 .and. dim <= 4) then
          res = sum(( real(x, dp) - center)**order, dim, mask) / count(mask, dim)
        else
          call error_stop("ERROR (moment): wrong dimension")
        end if

      end function moment_mask_scalar_4_iint64_dp

end submodule
