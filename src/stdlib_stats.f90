module stdlib_stats
  !! Provides support for various statistical methods. This includes currently
  !! descriptive statistics
  !! ([Specification](../page/specs/stdlib_stats.html))
  use stdlib_kinds, only: sp, dp, xdp, qp, &
      int8, int16, int32, int64
  implicit none
  private
  ! Public API
  public :: corr, cov, mean, median, moment, var


  interface corr
    !! version: experimental
    !!
    !! Pearson correlation of array elements
    !! ([Specification](../page/specs/stdlib_stats.html#corr-pearson-correlation-of-array-elements))
      module function corr_1_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res
      end function corr_1_rsp_rsp
      module function corr_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res
      end function corr_1_rdp_rdp
      module function corr_1_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res
      end function corr_1_csp_csp
      module function corr_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res
      end function corr_1_cdp_cdp

      module function corr_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res
      end function corr_1_iint8_dp
      module function corr_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res
      end function corr_1_iint16_dp
      module function corr_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res
      end function corr_1_iint32_dp
      module function corr_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res
      end function corr_1_iint64_dp

      module function corr_mask_1_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(sp) :: res
      end function corr_mask_1_rsp_rsp
      module function corr_mask_1_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res
      end function corr_mask_1_rdp_rdp
      module function corr_mask_1_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(sp) :: res
      end function corr_mask_1_csp_csp
      module function corr_mask_1_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res
      end function corr_mask_1_cdp_cdp

      module function corr_mask_1_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res
      end function corr_mask_1_iint8_dp
      module function corr_mask_1_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res
      end function corr_mask_1_iint16_dp
      module function corr_mask_1_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res
      end function corr_mask_1_iint32_dp
      module function corr_mask_1_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        real(dp) :: res
      end function corr_mask_1_iint64_dp

      module function corr_2_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_rsp_rsp
      module function corr_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_rdp_rdp
      module function corr_2_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_csp_csp
      module function corr_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_cdp_cdp

      module function corr_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_iint8_dp
      module function corr_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_iint16_dp
      module function corr_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_iint32_dp
      module function corr_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_2_iint64_dp

      module function corr_mask_2_rsp_rsp(x, dim, mask) result(res)
        real(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_rsp_rsp
      module function corr_mask_2_rdp_rdp(x, dim, mask) result(res)
        real(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_rdp_rdp
      module function corr_mask_2_csp_csp(x, dim, mask) result(res)
        complex(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_csp_csp
      module function corr_mask_2_cdp_cdp(x, dim, mask) result(res)
        complex(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_cdp_cdp

      module function corr_mask_2_iint8_dp(x, dim, mask) result(res)
        integer(int8), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_iint8_dp
      module function corr_mask_2_iint16_dp(x, dim, mask) result(res)
        integer(int16), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_iint16_dp
      module function corr_mask_2_iint32_dp(x, dim, mask) result(res)
        integer(int32), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_iint32_dp
      module function corr_mask_2_iint64_dp(x, dim, mask) result(res)
        integer(int64), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function corr_mask_2_iint64_dp

  end interface corr


  interface cov
    !! version: experimental
    !!
    !! Covariance of array elements
    !! ([Specification](../page/specs/stdlib_stats.html#cov-covariance-of-array-elements))
      module function cov_1_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res
      end function cov_1_rsp_rsp
      module function cov_1_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_1_rdp_rdp
      module function cov_1_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res
      end function cov_1_csp_csp
      module function cov_1_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_1_cdp_cdp

      module function cov_1_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_1_iint8_dp
      module function cov_1_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_1_iint16_dp
      module function cov_1_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_1_iint32_dp
      module function cov_1_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_1_iint64_dp

      module function cov_mask_1_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(sp) :: res
      end function cov_mask_1_rsp_rsp
      module function cov_mask_1_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_mask_1_rdp_rdp
      module function cov_mask_1_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(sp) :: res
      end function cov_mask_1_csp_csp
      module function cov_mask_1_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_mask_1_cdp_cdp

      module function cov_mask_1_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_mask_1_iint8_dp
      module function cov_mask_1_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_mask_1_iint16_dp
      module function cov_mask_1_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_mask_1_iint32_dp
      module function cov_mask_1_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:)
        logical, intent(in), optional :: corrected
        real(dp) :: res
      end function cov_mask_1_iint64_dp

      module function cov_2_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_rsp_rsp
      module function cov_2_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_rdp_rdp
      module function cov_2_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_csp_csp
      module function cov_2_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_cdp_cdp

      module function cov_2_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_iint8_dp
      module function cov_2_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_iint16_dp
      module function cov_2_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_iint32_dp
      module function cov_2_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in), optional :: mask
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                        , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_2_iint64_dp

      module function cov_mask_2_rsp_rsp(x, dim, mask, corrected) result(res)
        real(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_rsp_rsp
      module function cov_mask_2_rdp_rdp(x, dim, mask, corrected) result(res)
        real(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_rdp_rdp
      module function cov_mask_2_csp_csp(x, dim, mask, corrected) result(res)
        complex(sp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_csp_csp
      module function cov_mask_2_cdp_cdp(x, dim, mask, corrected) result(res)
        complex(dp), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_cdp_cdp

      module function cov_mask_2_iint8_dp(x, dim, mask, corrected) result(res)
        integer(int8), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_iint8_dp
      module function cov_mask_2_iint16_dp(x, dim, mask, corrected) result(res)
        integer(int16), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_iint16_dp
      module function cov_mask_2_iint32_dp(x, dim, mask, corrected) result(res)
        integer(int32), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_iint32_dp
      module function cov_mask_2_iint64_dp(x, dim, mask, corrected) result(res)
        integer(int64), intent(in) :: x(:, :)
        integer, intent(in) :: dim
        logical, intent(in) :: mask(:,:)
        logical, intent(in), optional :: corrected
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                            , merge(size(x, 1), size(x, 2), mask = 1<dim))
      end function cov_mask_2_iint64_dp
  end interface cov


  interface mean
    !! version: experimental
    !!
    !! Mean of array elements
    !! ([Specification](../page/specs/stdlib_stats.html#mean-mean-of-array-elements))
        module function mean_all_1_rsp_rsp (x, mask) result(res)
          real(sp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function mean_all_1_rsp_rsp
        module function mean_all_2_rsp_rsp (x, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function mean_all_2_rsp_rsp
        module function mean_all_3_rsp_rsp (x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function mean_all_3_rsp_rsp
        module function mean_all_4_rsp_rsp (x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function mean_all_4_rsp_rsp
        module function mean_all_1_rdp_rdp (x, mask) result(res)
          real(dp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_1_rdp_rdp
        module function mean_all_2_rdp_rdp (x, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_2_rdp_rdp
        module function mean_all_3_rdp_rdp (x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_3_rdp_rdp
        module function mean_all_4_rdp_rdp (x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_4_rdp_rdp
        module function mean_all_1_csp_csp (x, mask) result(res)
          complex(sp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function mean_all_1_csp_csp
        module function mean_all_2_csp_csp (x, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function mean_all_2_csp_csp
        module function mean_all_3_csp_csp (x, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function mean_all_3_csp_csp
        module function mean_all_4_csp_csp (x, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function mean_all_4_csp_csp
        module function mean_all_1_cdp_cdp (x, mask) result(res)
          complex(dp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function mean_all_1_cdp_cdp
        module function mean_all_2_cdp_cdp (x, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function mean_all_2_cdp_cdp
        module function mean_all_3_cdp_cdp (x, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function mean_all_3_cdp_cdp
        module function mean_all_4_cdp_cdp (x, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function mean_all_4_cdp_cdp

        module function mean_all_1_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_1_iint8_dp
        module function mean_all_2_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_2_iint8_dp
        module function mean_all_3_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_3_iint8_dp
        module function mean_all_4_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_4_iint8_dp
        module function mean_all_1_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_1_iint16_dp
        module function mean_all_2_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_2_iint16_dp
        module function mean_all_3_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_3_iint16_dp
        module function mean_all_4_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_4_iint16_dp
        module function mean_all_1_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_1_iint32_dp
        module function mean_all_2_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_2_iint32_dp
        module function mean_all_3_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_3_iint32_dp
        module function mean_all_4_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_4_iint32_dp
        module function mean_all_1_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_1_iint64_dp
        module function mean_all_2_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_2_iint64_dp
        module function mean_all_3_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_3_iint64_dp
        module function mean_all_4_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_all_4_iint64_dp

        module function mean_1_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function mean_1_rsp_rsp
        module function mean_2_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_rsp_rsp
        module function mean_3_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_rsp_rsp
        module function mean_4_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_4_rsp_rsp
        module function mean_1_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_1_rdp_rdp
        module function mean_2_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_rdp_rdp
        module function mean_3_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_rdp_rdp
        module function mean_4_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_4_rdp_rdp
        module function mean_1_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function mean_1_csp_csp
        module function mean_2_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_csp_csp
        module function mean_3_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_csp_csp
        module function mean_4_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function mean_4_csp_csp
        module function mean_1_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function mean_1_cdp_cdp
        module function mean_2_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_cdp_cdp
        module function mean_3_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_cdp_cdp
        module function mean_4_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function mean_4_cdp_cdp

        module function mean_1_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_1_iint8_dp
        module function mean_2_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_iint8_dp
        module function mean_3_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_iint8_dp
        module function mean_4_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_4_iint8_dp
        module function mean_1_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_1_iint16_dp
        module function mean_2_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_iint16_dp
        module function mean_3_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_iint16_dp
        module function mean_4_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_4_iint16_dp
        module function mean_1_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_1_iint32_dp
        module function mean_2_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_iint32_dp
        module function mean_3_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_iint32_dp
        module function mean_4_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_4_iint32_dp
        module function mean_1_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function mean_1_iint64_dp
        module function mean_2_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_2_iint64_dp
        module function mean_3_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_3_iint64_dp
        module function mean_4_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_4_iint64_dp

        module function mean_mask_all_1_rsp_rsp(x, mask) result(res)
          real(sp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(sp) :: res
          end function mean_mask_all_1_rsp_rsp
        module function mean_mask_all_2_rsp_rsp(x, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(sp) :: res
          end function mean_mask_all_2_rsp_rsp
        module function mean_mask_all_3_rsp_rsp(x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(sp) :: res
          end function mean_mask_all_3_rsp_rsp
        module function mean_mask_all_4_rsp_rsp(x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(sp) :: res
          end function mean_mask_all_4_rsp_rsp
        module function mean_mask_all_1_rdp_rdp(x, mask) result(res)
          real(dp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
          end function mean_mask_all_1_rdp_rdp
        module function mean_mask_all_2_rdp_rdp(x, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
          end function mean_mask_all_2_rdp_rdp
        module function mean_mask_all_3_rdp_rdp(x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
          end function mean_mask_all_3_rdp_rdp
        module function mean_mask_all_4_rdp_rdp(x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
          end function mean_mask_all_4_rdp_rdp
        module function mean_mask_all_1_csp_csp(x, mask) result(res)
          complex(sp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          complex(sp) :: res
          end function mean_mask_all_1_csp_csp
        module function mean_mask_all_2_csp_csp(x, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          complex(sp) :: res
          end function mean_mask_all_2_csp_csp
        module function mean_mask_all_3_csp_csp(x, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          complex(sp) :: res
          end function mean_mask_all_3_csp_csp
        module function mean_mask_all_4_csp_csp(x, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          complex(sp) :: res
          end function mean_mask_all_4_csp_csp
        module function mean_mask_all_1_cdp_cdp(x, mask) result(res)
          complex(dp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          complex(dp) :: res
          end function mean_mask_all_1_cdp_cdp
        module function mean_mask_all_2_cdp_cdp(x, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          complex(dp) :: res
          end function mean_mask_all_2_cdp_cdp
        module function mean_mask_all_3_cdp_cdp(x, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          complex(dp) :: res
          end function mean_mask_all_3_cdp_cdp
        module function mean_mask_all_4_cdp_cdp(x, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          complex(dp) :: res
          end function mean_mask_all_4_cdp_cdp

        module function mean_mask_all_1_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_all_1_iint8_dp
        module function mean_mask_all_2_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function mean_mask_all_2_iint8_dp
        module function mean_mask_all_3_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function mean_mask_all_3_iint8_dp
        module function mean_mask_all_4_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function mean_mask_all_4_iint8_dp
        module function mean_mask_all_1_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_all_1_iint16_dp
        module function mean_mask_all_2_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function mean_mask_all_2_iint16_dp
        module function mean_mask_all_3_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function mean_mask_all_3_iint16_dp
        module function mean_mask_all_4_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function mean_mask_all_4_iint16_dp
        module function mean_mask_all_1_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_all_1_iint32_dp
        module function mean_mask_all_2_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function mean_mask_all_2_iint32_dp
        module function mean_mask_all_3_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function mean_mask_all_3_iint32_dp
        module function mean_mask_all_4_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function mean_mask_all_4_iint32_dp
        module function mean_mask_all_1_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_all_1_iint64_dp
        module function mean_mask_all_2_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function mean_mask_all_2_iint64_dp
        module function mean_mask_all_3_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function mean_mask_all_3_iint64_dp
        module function mean_mask_all_4_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function mean_mask_all_4_iint64_dp

        module function mean_mask_1_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(sp) :: res
        end function mean_mask_1_rsp_rsp
        module function mean_mask_2_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_rsp_rsp
        module function mean_mask_3_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_rsp_rsp
        module function mean_mask_4_rsp_rsp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_mask_4_rsp_rsp
        module function mean_mask_1_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_1_rdp_rdp
        module function mean_mask_2_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_rdp_rdp
        module function mean_mask_3_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_rdp_rdp
        module function mean_mask_4_rdp_rdp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_mask_4_rdp_rdp
        module function mean_mask_1_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          complex(sp) :: res
        end function mean_mask_1_csp_csp
        module function mean_mask_2_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_csp_csp
        module function mean_mask_3_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_csp_csp
        module function mean_mask_4_csp_csp(x, dim, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function mean_mask_4_csp_csp
        module function mean_mask_1_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          complex(dp) :: res
        end function mean_mask_1_cdp_cdp
        module function mean_mask_2_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_cdp_cdp
        module function mean_mask_3_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_cdp_cdp
        module function mean_mask_4_cdp_cdp(x, dim, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function mean_mask_4_cdp_cdp

        module function mean_mask_1_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_1_iint8_dp
        module function mean_mask_2_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_iint8_dp
        module function mean_mask_3_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_iint8_dp
        module function mean_mask_4_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_mask_4_iint8_dp
        module function mean_mask_1_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_1_iint16_dp
        module function mean_mask_2_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_iint16_dp
        module function mean_mask_3_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_iint16_dp
        module function mean_mask_4_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_mask_4_iint16_dp
        module function mean_mask_1_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_1_iint32_dp
        module function mean_mask_2_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_iint32_dp
        module function mean_mask_3_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_iint32_dp
        module function mean_mask_4_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_mask_4_iint32_dp
        module function mean_mask_1_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function mean_mask_1_iint64_dp
        module function mean_mask_2_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function mean_mask_2_iint64_dp
        module function mean_mask_3_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function mean_mask_3_iint64_dp
        module function mean_mask_4_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function mean_mask_4_iint64_dp

  end interface mean


  interface median
    !! version: experimental
    !!
    !! Median of array elements
    !! ([Specification](../page/specs/stdlib_stats.html#median-median-of-array-elements))
        module function median_all_1_iint8_dp (x, mask) result(res)
          integer(int8), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_1_iint8_dp
        module function median_all_2_iint8_dp (x, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_2_iint8_dp
        module function median_all_3_iint8_dp (x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_3_iint8_dp
        module function median_all_4_iint8_dp (x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_4_iint8_dp
        module function median_all_1_iint16_dp (x, mask) result(res)
          integer(int16), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_1_iint16_dp
        module function median_all_2_iint16_dp (x, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_2_iint16_dp
        module function median_all_3_iint16_dp (x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_3_iint16_dp
        module function median_all_4_iint16_dp (x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_4_iint16_dp
        module function median_all_1_iint32_dp (x, mask) result(res)
          integer(int32), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_1_iint32_dp
        module function median_all_2_iint32_dp (x, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_2_iint32_dp
        module function median_all_3_iint32_dp (x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_3_iint32_dp
        module function median_all_4_iint32_dp (x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_4_iint32_dp
        module function median_all_1_iint64_dp (x, mask) result(res)
          integer(int64), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_1_iint64_dp
        module function median_all_2_iint64_dp (x, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_2_iint64_dp
        module function median_all_3_iint64_dp (x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_3_iint64_dp
        module function median_all_4_iint64_dp (x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_4_iint64_dp
        module function median_all_1_rsp_sp (x, mask) result(res)
          real(sp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function median_all_1_rsp_sp
        module function median_all_2_rsp_sp (x, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function median_all_2_rsp_sp
        module function median_all_3_rsp_sp (x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function median_all_3_rsp_sp
        module function median_all_4_rsp_sp (x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function median_all_4_rsp_sp
        module function median_all_1_rdp_dp (x, mask) result(res)
          real(dp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_1_rdp_dp
        module function median_all_2_rdp_dp (x, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_2_rdp_dp
        module function median_all_3_rdp_dp (x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_3_rdp_dp
        module function median_all_4_rdp_dp (x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_all_4_rdp_dp
  
        module function median_1_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_1_iint8_dp
        module function median_2_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_2_iint8_dp
        module function median_3_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_3_iint8_dp
        module function median_4_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_4_iint8_dp
        module function median_1_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_1_iint16_dp
        module function median_2_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_2_iint16_dp
        module function median_3_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_3_iint16_dp
        module function median_4_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_4_iint16_dp
        module function median_1_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_1_iint32_dp
        module function median_2_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_2_iint32_dp
        module function median_3_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_3_iint32_dp
        module function median_4_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_4_iint32_dp
        module function median_1_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_1_iint64_dp
        module function median_2_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_2_iint64_dp
        module function median_3_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_3_iint64_dp
        module function median_4_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_4_iint64_dp
        module function median_1_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function median_1_rsp_sp
        module function median_2_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_2_rsp_sp
        module function median_3_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_3_rsp_sp
        module function median_4_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_4_rsp_sp
        module function median_1_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function median_1_rdp_dp
        module function median_2_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_2_rdp_dp
        module function median_3_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_3_rdp_dp
        module function median_4_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_4_rdp_dp
  
        module function median_all_mask_1_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_all_mask_1_iint8_dp
        module function median_all_mask_2_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function median_all_mask_2_iint8_dp
        module function median_all_mask_3_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function median_all_mask_3_iint8_dp
        module function median_all_mask_4_iint8_dp(x, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function median_all_mask_4_iint8_dp
        module function median_all_mask_1_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_all_mask_1_iint16_dp
        module function median_all_mask_2_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function median_all_mask_2_iint16_dp
        module function median_all_mask_3_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function median_all_mask_3_iint16_dp
        module function median_all_mask_4_iint16_dp(x, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function median_all_mask_4_iint16_dp
        module function median_all_mask_1_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_all_mask_1_iint32_dp
        module function median_all_mask_2_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function median_all_mask_2_iint32_dp
        module function median_all_mask_3_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function median_all_mask_3_iint32_dp
        module function median_all_mask_4_iint32_dp(x, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function median_all_mask_4_iint32_dp
        module function median_all_mask_1_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_all_mask_1_iint64_dp
        module function median_all_mask_2_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function median_all_mask_2_iint64_dp
        module function median_all_mask_3_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function median_all_mask_3_iint64_dp
        module function median_all_mask_4_iint64_dp(x, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function median_all_mask_4_iint64_dp
        module function median_all_mask_1_rsp_sp(x, mask) result(res)
          real(sp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(sp) :: res
        end function median_all_mask_1_rsp_sp
        module function median_all_mask_2_rsp_sp(x, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(sp) :: res
        end function median_all_mask_2_rsp_sp
        module function median_all_mask_3_rsp_sp(x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(sp) :: res
        end function median_all_mask_3_rsp_sp
        module function median_all_mask_4_rsp_sp(x, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(sp) :: res
        end function median_all_mask_4_rsp_sp
        module function median_all_mask_1_rdp_dp(x, mask) result(res)
          real(dp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_all_mask_1_rdp_dp
        module function median_all_mask_2_rdp_dp(x, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function median_all_mask_2_rdp_dp
        module function median_all_mask_3_rdp_dp(x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function median_all_mask_3_rdp_dp
        module function median_all_mask_4_rdp_dp(x, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function median_all_mask_4_rdp_dp
  
        module function  median_mask_1_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_mask_1_iint8_dp
        module function  median_mask_2_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_mask_2_iint8_dp
        module function  median_mask_3_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_mask_3_iint8_dp
        module function  median_mask_4_iint8_dp(x, dim, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_mask_4_iint8_dp
        module function  median_mask_1_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_mask_1_iint16_dp
        module function  median_mask_2_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_mask_2_iint16_dp
        module function  median_mask_3_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_mask_3_iint16_dp
        module function  median_mask_4_iint16_dp(x, dim, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_mask_4_iint16_dp
        module function  median_mask_1_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_mask_1_iint32_dp
        module function  median_mask_2_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_mask_2_iint32_dp
        module function  median_mask_3_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_mask_3_iint32_dp
        module function  median_mask_4_iint32_dp(x, dim, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_mask_4_iint32_dp
        module function  median_mask_1_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_mask_1_iint64_dp
        module function  median_mask_2_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_mask_2_iint64_dp
        module function  median_mask_3_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_mask_3_iint64_dp
        module function  median_mask_4_iint64_dp(x, dim, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_mask_4_iint64_dp
        module function  median_mask_1_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(sp) :: res
        end function median_mask_1_rsp_sp
        module function  median_mask_2_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_mask_2_rsp_sp
        module function  median_mask_3_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_mask_3_rsp_sp
        module function  median_mask_4_rsp_sp(x, dim, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_mask_4_rsp_sp
        module function  median_mask_1_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function median_mask_1_rdp_dp
        module function  median_mask_2_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function median_mask_2_rdp_dp
        module function  median_mask_3_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function median_mask_3_rdp_dp
        module function  median_mask_4_rdp_dp(x, dim, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function median_mask_4_rdp_dp

  end interface


  interface var
    !! version: experimental
    !!
    !! Variance of array elements
    !! ([Specification](../page/specs/stdlib_stats.html#var-variance-of-array-elements))

        module function var_all_1_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_1_rsp_rsp
        module function var_all_2_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_2_rsp_rsp
        module function var_all_3_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_3_rsp_rsp
        module function var_all_4_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_4_rsp_rsp
        module function var_all_1_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_1_rdp_rdp
        module function var_all_2_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_2_rdp_rdp
        module function var_all_3_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_3_rdp_rdp
        module function var_all_4_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_4_rdp_rdp
        module function var_all_1_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_1_csp_csp
        module function var_all_2_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_2_csp_csp
        module function var_all_3_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_3_csp_csp
        module function var_all_4_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_all_4_csp_csp
        module function var_all_1_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_1_cdp_cdp
        module function var_all_2_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_2_cdp_cdp
        module function var_all_3_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_3_cdp_cdp
        module function var_all_4_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_4_cdp_cdp

        module function var_all_1_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_1_iint8_dp
        module function var_all_2_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_2_iint8_dp
        module function var_all_3_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_3_iint8_dp
        module function var_all_4_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_4_iint8_dp
        module function var_all_1_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_1_iint16_dp
        module function var_all_2_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_2_iint16_dp
        module function var_all_3_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_3_iint16_dp
        module function var_all_4_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_4_iint16_dp
        module function var_all_1_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_1_iint32_dp
        module function var_all_2_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_2_iint32_dp
        module function var_all_3_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_3_iint32_dp
        module function var_all_4_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_4_iint32_dp
        module function var_all_1_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_1_iint64_dp
        module function var_all_2_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_2_iint64_dp
        module function var_all_3_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_3_iint64_dp
        module function var_all_4_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_all_4_iint64_dp

        module function var_1_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_1_rsp_rsp
        module function var_2_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_rsp_rsp
        module function var_3_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_rsp_rsp
        module function var_4_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_rsp_rsp
        module function var_1_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_1_rdp_rdp
        module function var_2_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_rdp_rdp
        module function var_3_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_rdp_rdp
        module function var_4_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_rdp_rdp
        module function var_1_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_1_csp_csp
        module function var_2_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_csp_csp
        module function var_3_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_csp_csp
        module function var_4_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_csp_csp
        module function var_1_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_1_cdp_cdp
        module function var_2_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_cdp_cdp
        module function var_3_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_cdp_cdp
        module function var_4_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_cdp_cdp

        module function var_1_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_1_iint8_dp
        module function var_2_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_iint8_dp
        module function var_3_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_iint8_dp
        module function var_4_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_iint8_dp
        module function var_1_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_1_iint16_dp
        module function var_2_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_iint16_dp
        module function var_3_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_iint16_dp
        module function var_4_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_iint16_dp
        module function var_1_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_1_iint32_dp
        module function var_2_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_iint32_dp
        module function var_3_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_iint32_dp
        module function var_4_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_iint32_dp
        module function var_1_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_1_iint64_dp
        module function var_2_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_2_iint64_dp
        module function var_3_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_3_iint64_dp
        module function var_4_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in), optional :: mask
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_4_iint64_dp

        module function var_mask_all_1_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_1_rsp_rsp
        module function var_mask_all_2_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_2_rsp_rsp
        module function var_mask_all_3_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_3_rsp_rsp
        module function var_mask_all_4_rsp_rsp(x, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_4_rsp_rsp
        module function var_mask_all_1_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_1_rdp_rdp
        module function var_mask_all_2_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_2_rdp_rdp
        module function var_mask_all_3_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_3_rdp_rdp
        module function var_mask_all_4_rdp_rdp(x, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_4_rdp_rdp
        module function var_mask_all_1_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_1_csp_csp
        module function var_mask_all_2_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_2_csp_csp
        module function var_mask_all_3_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_3_csp_csp
        module function var_mask_all_4_csp_csp(x, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_all_4_csp_csp
        module function var_mask_all_1_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_1_cdp_cdp
        module function var_mask_all_2_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_2_cdp_cdp
        module function var_mask_all_3_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_3_cdp_cdp
        module function var_mask_all_4_cdp_cdp(x, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_4_cdp_cdp

        module function var_mask_all_1_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_1_iint8_dp
        module function var_mask_all_2_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_2_iint8_dp
        module function var_mask_all_3_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_3_iint8_dp
        module function var_mask_all_4_iint8_dp(x, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_4_iint8_dp
        module function var_mask_all_1_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_1_iint16_dp
        module function var_mask_all_2_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_2_iint16_dp
        module function var_mask_all_3_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_3_iint16_dp
        module function var_mask_all_4_iint16_dp(x, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_4_iint16_dp
        module function var_mask_all_1_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_1_iint32_dp
        module function var_mask_all_2_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_2_iint32_dp
        module function var_mask_all_3_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_3_iint32_dp
        module function var_mask_all_4_iint32_dp(x, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_4_iint32_dp
        module function var_mask_all_1_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:)
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_1_iint64_dp
        module function var_mask_all_2_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:)
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_2_iint64_dp
        module function var_mask_all_3_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_3_iint64_dp
        module function var_mask_all_4_iint64_dp(x, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_all_4_iint64_dp

        module function var_mask_1_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_1_rsp_rsp
        module function var_mask_2_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_rsp_rsp
        module function var_mask_3_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_rsp_rsp
        module function var_mask_4_rsp_rsp(x, dim, mask, corrected) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_rsp_rsp
        module function var_mask_1_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_1_rdp_rdp
        module function var_mask_2_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_rdp_rdp
        module function var_mask_3_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_rdp_rdp
        module function var_mask_4_rdp_rdp(x, dim, mask, corrected) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_rdp_rdp
        module function var_mask_1_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(sp) :: res
        end function var_mask_1_csp_csp
        module function var_mask_2_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_csp_csp
        module function var_mask_3_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_csp_csp
        module function var_mask_4_csp_csp(x, dim, mask, corrected) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_csp_csp
        module function var_mask_1_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_1_cdp_cdp
        module function var_mask_2_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_cdp_cdp
        module function var_mask_3_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_cdp_cdp
        module function var_mask_4_cdp_cdp(x, dim, mask, corrected) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_cdp_cdp

        module function var_mask_1_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_1_iint8_dp
        module function var_mask_2_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_iint8_dp
        module function var_mask_3_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_iint8_dp
        module function var_mask_4_iint8_dp(x, dim, mask, corrected) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_iint8_dp
        module function var_mask_1_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_1_iint16_dp
        module function var_mask_2_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_iint16_dp
        module function var_mask_3_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_iint16_dp
        module function var_mask_4_iint16_dp(x, dim, mask, corrected) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_iint16_dp
        module function var_mask_1_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_1_iint32_dp
        module function var_mask_2_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_iint32_dp
        module function var_mask_3_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_iint32_dp
        module function var_mask_4_iint32_dp(x, dim, mask, corrected) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_iint32_dp
        module function var_mask_1_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:)
          logical, intent(in), optional :: corrected
          real(dp) :: res
        end function var_mask_1_iint64_dp
        module function var_mask_2_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function var_mask_2_iint64_dp
        module function var_mask_3_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function var_mask_3_iint64_dp
        module function var_mask_4_iint64_dp(x, dim, mask, corrected) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: dim
          logical, intent(in) :: mask(:,:,:,:)
          logical, intent(in), optional :: corrected
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function var_mask_4_iint64_dp

  end interface var


  interface moment
    !! version: experimental
    !!
    !! Central moment of array elements
    !! ([Specification](../page/specs/stdlib_stats.html#moment-central-moments-of-array-elements))
        module function moment_all_1_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function moment_all_1_rsp_rsp
        module function moment_all_2_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function moment_all_2_rsp_rsp
        module function moment_all_3_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function moment_all_3_rsp_rsp
        module function moment_all_4_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function moment_all_4_rsp_rsp
        module function moment_all_1_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_1_rdp_rdp
        module function moment_all_2_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_2_rdp_rdp
        module function moment_all_3_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_3_rdp_rdp
        module function moment_all_4_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_4_rdp_rdp
        module function moment_all_1_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function moment_all_1_csp_csp
        module function moment_all_2_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function moment_all_2_csp_csp
        module function moment_all_3_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function moment_all_3_csp_csp
        module function moment_all_4_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function moment_all_4_csp_csp
        module function moment_all_1_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function moment_all_1_cdp_cdp
        module function moment_all_2_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function moment_all_2_cdp_cdp
        module function moment_all_3_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function moment_all_3_cdp_cdp
        module function moment_all_4_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function moment_all_4_cdp_cdp

        module function moment_all_1_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_1_iint8_dp
        module function moment_all_2_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_2_iint8_dp
        module function moment_all_3_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_3_iint8_dp
        module function moment_all_4_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_4_iint8_dp
        module function moment_all_1_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_1_iint16_dp
        module function moment_all_2_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_2_iint16_dp
        module function moment_all_3_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_3_iint16_dp
        module function moment_all_4_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_4_iint16_dp
        module function moment_all_1_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_1_iint32_dp
        module function moment_all_2_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_2_iint32_dp
        module function moment_all_3_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_3_iint32_dp
        module function moment_all_4_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_4_iint32_dp
        module function moment_all_1_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_1_iint64_dp
        module function moment_all_2_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_2_iint64_dp
        module function moment_all_3_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_3_iint64_dp
        module function moment_all_4_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_all_4_iint64_dp

        module function moment_scalar_2_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in) :: center
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_rsp_rsp
        module function moment_scalar_3_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in) :: center
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_rsp_rsp
        module function moment_scalar_4_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in) :: center
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_scalar_4_rsp_rsp
        module function moment_scalar_2_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_rdp_rdp
        module function moment_scalar_3_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_rdp_rdp
        module function moment_scalar_4_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_scalar_4_rdp_rdp
        module function moment_scalar_2_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in) :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_csp_csp
        module function moment_scalar_3_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in) :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_csp_csp
        module function moment_scalar_4_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in) :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_scalar_4_csp_csp
        module function moment_scalar_2_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in) :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_cdp_cdp
        module function moment_scalar_3_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in) :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_cdp_cdp
        module function moment_scalar_4_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in) :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_scalar_4_cdp_cdp

        module function moment_1_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(sp) :: res
        end function moment_1_rsp_rsp
        module function moment_2_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_rsp_rsp
        module function moment_3_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_rsp_rsp
        module function moment_4_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_4_rsp_rsp
        module function moment_1_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_1_rdp_rdp
        module function moment_2_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_rdp_rdp
        module function moment_3_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_rdp_rdp
        module function moment_4_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_4_rdp_rdp
        module function moment_1_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(sp) :: res
        end function moment_1_csp_csp
        module function moment_2_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_csp_csp
        module function moment_3_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_csp_csp
        module function moment_4_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_4_csp_csp
        module function moment_1_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center
          logical, intent(in), optional :: mask
          complex(dp) :: res
        end function moment_1_cdp_cdp
        module function moment_2_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_cdp_cdp
        module function moment_3_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_cdp_cdp
        module function moment_4_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_4_cdp_cdp

        module function moment_scalar_2_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_iint8_dp
        module function moment_scalar_3_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_iint8_dp
        module function moment_scalar_4_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_scalar_4_iint8_dp
        module function moment_scalar_2_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_iint16_dp
        module function moment_scalar_3_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_iint16_dp
        module function moment_scalar_4_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_scalar_4_iint16_dp
        module function moment_scalar_2_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_iint32_dp
        module function moment_scalar_3_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_iint32_dp
        module function moment_scalar_4_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_scalar_4_iint32_dp
        module function moment_scalar_2_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_scalar_2_iint64_dp
        module function moment_scalar_3_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_scalar_3_iint64_dp
        module function moment_scalar_4_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in) :: center
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_scalar_4_iint64_dp

        module function moment_1_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_1_iint8_dp
        module function moment_2_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_iint8_dp
        module function moment_3_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_iint8_dp
        module function moment_4_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_4_iint8_dp
        module function moment_1_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_1_iint16_dp
        module function moment_2_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_iint16_dp
        module function moment_3_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_iint16_dp
        module function moment_4_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_4_iint16_dp
        module function moment_1_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_1_iint32_dp
        module function moment_2_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_iint32_dp
        module function moment_3_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_iint32_dp
        module function moment_4_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_4_iint32_dp
        module function moment_1_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center
          logical, intent(in), optional :: mask
          real(dp) :: res
        end function moment_1_iint64_dp
        module function moment_2_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_2_iint64_dp
        module function moment_3_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_3_iint64_dp
        module function moment_4_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in), optional :: mask
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_4_iint64_dp

        module function moment_mask_all_1_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(sp) :: res
        end function moment_mask_all_1_rsp_rsp
        module function moment_mask_all_2_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          real(sp) :: res
        end function moment_mask_all_2_rsp_rsp
        module function moment_mask_all_3_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          real(sp) :: res
        end function moment_mask_all_3_rsp_rsp
        module function moment_mask_all_4_rsp_rsp(x, order, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(sp) :: res
        end function moment_mask_all_4_rsp_rsp
        module function moment_mask_all_1_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_all_1_rdp_rdp
        module function moment_mask_all_2_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function moment_mask_all_2_rdp_rdp
        module function moment_mask_all_3_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function moment_mask_all_3_rdp_rdp
        module function moment_mask_all_4_rdp_rdp(x, order, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function moment_mask_all_4_rdp_rdp
        module function moment_mask_all_1_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          complex(sp) :: res
        end function moment_mask_all_1_csp_csp
        module function moment_mask_all_2_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          complex(sp) :: res
        end function moment_mask_all_2_csp_csp
        module function moment_mask_all_3_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          complex(sp) :: res
        end function moment_mask_all_3_csp_csp
        module function moment_mask_all_4_csp_csp(x, order, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          complex(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          complex(sp) :: res
        end function moment_mask_all_4_csp_csp
        module function moment_mask_all_1_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          complex(dp) :: res
        end function moment_mask_all_1_cdp_cdp
        module function moment_mask_all_2_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          complex(dp) :: res
        end function moment_mask_all_2_cdp_cdp
        module function moment_mask_all_3_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          complex(dp) :: res
        end function moment_mask_all_3_cdp_cdp
        module function moment_mask_all_4_cdp_cdp(x, order, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          complex(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          complex(dp) :: res
        end function moment_mask_all_4_cdp_cdp

        module function moment_mask_all_1_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_all_1_iint8_dp
        module function moment_mask_all_2_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function moment_mask_all_2_iint8_dp
        module function moment_mask_all_3_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function moment_mask_all_3_iint8_dp
        module function moment_mask_all_4_iint8_dp(x, order, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function moment_mask_all_4_iint8_dp
        module function moment_mask_all_1_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_all_1_iint16_dp
        module function moment_mask_all_2_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function moment_mask_all_2_iint16_dp
        module function moment_mask_all_3_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function moment_mask_all_3_iint16_dp
        module function moment_mask_all_4_iint16_dp(x, order, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function moment_mask_all_4_iint16_dp
        module function moment_mask_all_1_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_all_1_iint32_dp
        module function moment_mask_all_2_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function moment_mask_all_2_iint32_dp
        module function moment_mask_all_3_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function moment_mask_all_3_iint32_dp
        module function moment_mask_all_4_iint32_dp(x, order, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function moment_mask_all_4_iint32_dp
        module function moment_mask_all_1_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_all_1_iint64_dp
        module function moment_mask_all_2_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res
        end function moment_mask_all_2_iint64_dp
        module function moment_mask_all_3_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res
        end function moment_mask_all_3_iint64_dp
        module function moment_mask_all_4_iint64_dp(x, order, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          real(dp),intent(in), optional :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res
        end function moment_mask_all_4_iint64_dp

        module function moment_mask_scalar_2_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_rsp_rsp
        module function moment_mask_scalar_3_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_rsp_rsp
        module function moment_mask_scalar_4_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_rsp_rsp
        module function moment_mask_scalar_2_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_rdp_rdp
        module function moment_mask_scalar_3_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_rdp_rdp
        module function moment_mask_scalar_4_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_rdp_rdp
        module function moment_mask_scalar_2_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_csp_csp
        module function moment_mask_scalar_3_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_csp_csp
        module function moment_mask_scalar_4_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_csp_csp
        module function moment_mask_scalar_2_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_cdp_cdp
        module function moment_mask_scalar_3_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_cdp_cdp
        module function moment_mask_scalar_4_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_cdp_cdp

        module function moment_mask_1_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(sp) :: res
        end function moment_mask_1_rsp_rsp
        module function moment_mask_2_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_rsp_rsp
        module function moment_mask_3_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_rsp_rsp
        module function moment_mask_4_rsp_rsp(x, order, dim, center, mask) result(res)
          real(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_4_rsp_rsp
        module function moment_mask_1_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_1_rdp_rdp
        module function moment_mask_2_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_rdp_rdp
        module function moment_mask_3_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_rdp_rdp
        module function moment_mask_4_rdp_rdp(x, order, dim, center, mask) result(res)
          real(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_4_rdp_rdp
        module function moment_mask_1_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          complex(sp) :: res
        end function moment_mask_1_csp_csp
        module function moment_mask_2_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_csp_csp
        module function moment_mask_3_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_csp_csp
        module function moment_mask_4_csp_csp(x, order, dim, center, mask) result(res)
          complex(sp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_mask_4_csp_csp
        module function moment_mask_1_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          complex(dp) :: res
        end function moment_mask_1_cdp_cdp
        module function moment_mask_2_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_cdp_cdp
        module function moment_mask_3_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_cdp_cdp
        module function moment_mask_4_cdp_cdp(x, order, dim, center, mask) result(res)
          complex(dp), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x,&
              & 3), size(x, 4), mask=3<dim))
        end function moment_mask_4_cdp_cdp

        module function moment_mask_scalar_2_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_iint8_dp
        module function moment_mask_scalar_3_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_iint8_dp
        module function moment_mask_scalar_4_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_iint8_dp
        module function moment_mask_scalar_2_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_iint16_dp
        module function moment_mask_scalar_3_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_iint16_dp
        module function moment_mask_scalar_4_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_iint16_dp
        module function moment_mask_scalar_2_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_iint32_dp
        module function moment_mask_scalar_3_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_iint32_dp
        module function moment_mask_scalar_4_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_iint32_dp
        module function moment_mask_scalar_2_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_scalar_2_iint64_dp
        module function moment_mask_scalar_3_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_scalar_3_iint64_dp
        module function moment_mask_scalar_4_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in) :: center
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_scalar_4_iint64_dp

        module function moment_mask_1_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_1_iint8_dp
        module function moment_mask_2_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_iint8_dp
        module function moment_mask_3_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_iint8_dp
        module function moment_mask_4_iint8_dp(x, order, dim, center, mask) result(res)
          integer(int8), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_4_iint8_dp
        module function moment_mask_1_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_1_iint16_dp
        module function moment_mask_2_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_iint16_dp
        module function moment_mask_3_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_iint16_dp
        module function moment_mask_4_iint16_dp(x, order, dim, center, mask) result(res)
          integer(int16), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_4_iint16_dp
        module function moment_mask_1_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_1_iint32_dp
        module function moment_mask_2_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_iint32_dp
        module function moment_mask_3_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_iint32_dp
        module function moment_mask_4_iint32_dp(x, order, dim, center, mask) result(res)
          integer(int32), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_4_iint32_dp
        module function moment_mask_1_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center
          logical, intent(in) :: mask(:)
          real(dp) :: res
        end function moment_mask_1_iint64_dp
        module function moment_mask_2_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
          logical, intent(in) :: mask(:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))
        end function moment_mask_2_iint64_dp
        module function moment_mask_3_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim))
          logical, intent(in) :: mask(:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        end function moment_mask_3_iint64_dp
        module function moment_mask_4_iint64_dp(x, order, dim, center, mask) result(res)
          integer(int64), intent(in) :: x(:,:,:,:)
          integer, intent(in) :: order
          integer, intent(in) :: dim
          real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
              & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
          logical, intent(in) :: mask(:,:,:,:)
          real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
              & size(x, 4), mask=3<dim))
        end function moment_mask_4_iint64_dp
  end interface moment

end module stdlib_stats
