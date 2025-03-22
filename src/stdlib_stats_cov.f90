submodule (stdlib_stats) stdlib_stats_cov

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

    module function cov_1_rsp_rsp(x, dim, mask, corrected) result(res)
      real(sp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(sp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._sp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_rsp_rsp
    module function cov_1_rdp_rdp(x, dim, mask, corrected) result(res)
      real(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_rdp_rdp
    module function cov_1_csp_csp(x, dim, mask, corrected) result(res)
      complex(sp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(sp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._sp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_csp_csp
    module function cov_1_cdp_cdp(x, dim, mask, corrected) result(res)
      complex(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_cdp_cdp


    module function cov_1_iint8_dp(x, dim, mask, corrected) result(res)
      integer(int8), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_iint8_dp
    module function cov_1_iint16_dp(x, dim, mask, corrected) result(res)
      integer(int16), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_iint16_dp
    module function cov_1_iint32_dp(x, dim, mask, corrected) result(res)
      integer(int32), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_iint32_dp
    module function cov_1_iint64_dp(x, dim, mask, corrected) result(res)
      integer(int64), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      res = var(x, dim, corrected = optval(corrected, .true.))

    end function cov_1_iint64_dp


    module function cov_mask_1_rsp_rsp(x, dim, mask, corrected) result(res)
      real(sp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(sp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_rsp_rsp
    module function cov_mask_1_rdp_rdp(x, dim, mask, corrected) result(res)
      real(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(dp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_rdp_rdp
    module function cov_mask_1_csp_csp(x, dim, mask, corrected) result(res)
      complex(sp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(sp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_csp_csp
    module function cov_mask_1_cdp_cdp(x, dim, mask, corrected) result(res)
      complex(dp), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(dp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_cdp_cdp


    module function cov_mask_1_iint8_dp(x, dim, mask, corrected) result(res)
      integer(int8), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(dp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_iint8_dp
    module function cov_mask_1_iint16_dp(x, dim, mask, corrected) result(res)
      integer(int16), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(dp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_iint16_dp
    module function cov_mask_1_iint32_dp(x, dim, mask, corrected) result(res)
      integer(int32), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(dp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_iint32_dp
    module function cov_mask_1_iint64_dp(x, dim, mask, corrected) result(res)
      integer(int64), intent(in) :: x(:)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:)
      logical, intent(in), optional :: corrected
      real(dp) :: res

      res = var(x, dim, mask, corrected = optval(corrected, .true.))

    end function cov_mask_1_iint64_dp


    module function cov_2_rsp_rsp(x, dim, mask, corrected) result(res)
      real(sp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      real(sp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(sp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._sp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = x(i, :) - mean_
          end do
            res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = x(:, i) - mean_
          end do
            res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_rsp_rsp
    module function cov_2_rdp_rdp(x, dim, mask, corrected) result(res)
      real(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = x(i, :) - mean_
          end do
            res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = x(:, i) - mean_
          end do
            res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_rdp_rdp
    module function cov_2_csp_csp(x, dim, mask, corrected) result(res)
      complex(sp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      complex(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      complex(sp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      complex(sp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._sp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = x(i, :) - mean_
          end do
            res = matmul( transpose(conjg(center)), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = x(:, i) - mean_
          end do
            res = matmul( center, transpose(conjg(center)))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_csp_csp
    module function cov_2_cdp_cdp(x, dim, mask, corrected) result(res)
      complex(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      complex(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      complex(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = x(i, :) - mean_
          end do
            res = matmul( transpose(conjg(center)), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = x(:, i) - mean_
          end do
            res = matmul( center, transpose(conjg(center)))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_cdp_cdp


    module function cov_2_iint8_dp(x, dim, mask, corrected) result(res)
      integer(int8), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                      , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = real(x(i, :), dp) - mean_
          end do
          res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = real(x(:, i), dp) - mean_
          end do
          res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_iint8_dp
    module function cov_2_iint16_dp(x, dim, mask, corrected) result(res)
      integer(int16), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                      , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = real(x(i, :), dp) - mean_
          end do
          res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = real(x(:, i), dp) - mean_
          end do
          res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_iint16_dp
    module function cov_2_iint32_dp(x, dim, mask, corrected) result(res)
      integer(int32), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                      , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = real(x(i, :), dp) - mean_
          end do
          res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = real(x(:, i), dp) - mean_
          end do
          res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_iint32_dp
    module function cov_2_iint64_dp(x, dim, mask, corrected) result(res)
      integer(int64), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in), optional :: mask
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                      , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i
      real(dp) :: mean_(merge(size(x, 1), size(x, 2), mask = 1<dim))
      real(dp) :: center(size(x, 1),size(x, 2))

      if (.not.optval(mask, .true.)) then
        res = ieee_value(1._dp, ieee_quiet_nan)
        return
      end if

      mean_ = mean(x, dim)
      select case(dim)
        case(1)
          do i = 1, size(x, 1)
            center(i, :) = real(x(i, :), dp) - mean_
          end do
          res = matmul( transpose(center), center)
        case(2)
          do i = 1, size(x, 2)
            center(:, i) = real(x(:, i), dp) - mean_
          end do
          res = matmul( center, transpose(center))
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select
      res = res / (size(x, dim) - merge(1, 0, optval(corrected, .true.)))

    end function cov_2_iint64_dp


    module function cov_mask_2_rsp_rsp(x, dim, mask, corrected) result(res)
      real(sp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      real(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      real(sp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(sp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(:, i) .and. mask(:, j)
             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
                0._sp,&
                mask_)
             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
                0._sp,&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(i, :) .and. mask(j, :)
             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
                0._sp,&
                mask_)
             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
                0._sp,&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_rsp_rsp
    module function cov_mask_2_rdp_rdp(x, dim, mask, corrected) result(res)
      real(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(:, i) .and. mask(:, j)
             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
                0._dp,&
                mask_)
             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
                0._dp,&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(i, :) .and. mask(j, :)
             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
                0._dp,&
                mask_)
             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
                0._dp,&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_rdp_rdp
    module function cov_mask_2_csp_csp(x, dim, mask, corrected) result(res)
      complex(sp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      complex(sp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      complex(sp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      complex(sp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(:, i) .and. mask(:, j)
             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
                cmplx(0,0,kind=sp),&
                mask_)
             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
                cmplx(0,0,kind=sp),&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(i, :) .and. mask(j, :)
             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
                cmplx(0,0,kind=sp),&
                mask_)
             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
                cmplx(0,0,kind=sp),&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_csp_csp
    module function cov_mask_2_cdp_cdp(x, dim, mask, corrected) result(res)
      complex(dp), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      complex(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      complex(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      complex(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(:, i) .and. mask(:, j)
             centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)
             centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
             mask_ = mask(i, :) .and. mask(j, :)
             centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)
             centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),&
                cmplx(0,0,kind=dp),&
                mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_cdp_cdp


    module function cov_mask_2_iint8_dp(x, dim, mask, corrected) result(res)
      integer(int8), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(:, i) .and. mask(:, j)
              centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(i, :) .and. mask(j, :)
              centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_iint8_dp
    module function cov_mask_2_iint16_dp(x, dim, mask, corrected) result(res)
      integer(int16), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(:, i) .and. mask(:, j)
              centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(i, :) .and. mask(j, :)
              centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_iint16_dp
    module function cov_mask_2_iint32_dp(x, dim, mask, corrected) result(res)
      integer(int32), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(:, i) .and. mask(:, j)
              centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(i, :) .and. mask(j, :)
              centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_iint32_dp
    module function cov_mask_2_iint64_dp(x, dim, mask, corrected) result(res)
      integer(int64), intent(in) :: x(:, :)
      integer, intent(in) :: dim
      logical, intent(in) :: mask(:,:)
      logical, intent(in), optional :: corrected
      real(dp) :: res(merge(size(x, 1), size(x, 2), mask = 1<dim)&
                          , merge(size(x, 1), size(x, 2), mask = 1<dim))

      integer :: i, j, n
      real(dp) :: centeri_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      real(dp) :: centerj_(merge(size(x, 2), size(x, 1), mask = 1<dim))
      logical :: mask_(merge(size(x, 2), size(x, 1), mask = 1<dim))

      select case(dim)
        case(1)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(:, i) .and. mask(:, j)
              centeri_ = merge( x(:, i) - mean(x(:, i), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(:, j) - mean(x(:, j), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centerj_, centeri_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case(2)
          do i = 1, size(res, 2)
            do j = 1, size(res, 1)
              mask_ = mask(i, :) .and. mask(j, :)
              centeri_ = merge( x(i, :) - mean(x(i, :), mask = mask_),0._dp, mask_)
              centerj_ = merge( x(j, :) - mean(x(j, :), mask = mask_),0._dp, mask_)

              n = count(mask_)
              res(j, i) = dot_product( centeri_, centerj_)&
                           / (n - merge(1, 0,&
                            optval(corrected, .true.) .and. n > 0))
            end do
          end do
        case default
          call error_stop("ERROR (cov): wrong dimension")
      end select

    end function cov_mask_2_iint64_dp


end submodule
