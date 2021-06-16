submodule (stdlib_stats) stdlib_stats_moment

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function moment_1_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(sp) :: res

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_rsp_rsp
      module function moment_2_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_rsp_rsp
      module function moment_3_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_rsp_rsp
      module function moment_5_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_rsp_rsp
      module function moment_6_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_rsp_rsp
      module function moment_7_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_rsp_rsp
      module function moment_8_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_rsp_rsp
      module function moment_9_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_rsp_rsp
      module function moment_10_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_rsp_rsp
      module function moment_11_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_rsp_rsp
      module function moment_12_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_rsp_rsp
      module function moment_13_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_rsp_rsp
      module function moment_14_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_rsp_rsp
      module function moment_15_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_rsp_rsp
      module function moment_1_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_rdp_rdp
      module function moment_2_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_rdp_rdp
      module function moment_3_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_rdp_rdp
      module function moment_5_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_rdp_rdp
      module function moment_6_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_rdp_rdp
      module function moment_7_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_rdp_rdp
      module function moment_8_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_rdp_rdp
      module function moment_9_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_rdp_rdp
      module function moment_10_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_rdp_rdp
      module function moment_11_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_rdp_rdp
      module function moment_12_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_rdp_rdp
      module function moment_13_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_rdp_rdp
      module function moment_14_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_rdp_rdp
      module function moment_15_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_rdp_rdp
      module function moment_1_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(qp) :: res

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_rqp_rqp
      module function moment_2_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_rqp_rqp
      module function moment_3_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_3_rqp_rqp
      module function moment_4_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_rqp_rqp
      module function moment_5_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_rqp_rqp
      module function moment_6_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_rqp_rqp
      module function moment_7_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_rqp_rqp
      module function moment_8_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_rqp_rqp
      module function moment_9_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_rqp_rqp
      module function moment_10_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_rqp_rqp
      module function moment_11_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_rqp_rqp
      module function moment_12_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_rqp_rqp
      module function moment_13_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_rqp_rqp
      module function moment_14_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_rqp_rqp
      module function moment_15_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_rqp_rqp
      module function moment_1_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(sp) :: res

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_csp_csp
      module function moment_2_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_csp_csp
      module function moment_3_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_3_csp_csp
      module function moment_4_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_csp_csp
      module function moment_5_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_csp_csp
      module function moment_6_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_csp_csp
      module function moment_7_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_csp_csp
      module function moment_8_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_csp_csp
      module function moment_9_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_csp_csp
      module function moment_10_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_csp_csp
      module function moment_11_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_csp_csp
      module function moment_12_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_csp_csp
      module function moment_13_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_csp_csp
      module function moment_14_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_csp_csp
      module function moment_15_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._sp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_csp_csp
      module function moment_1_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(dp) :: res

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_cdp_cdp
      module function moment_2_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_cdp_cdp
      module function moment_3_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_3_cdp_cdp
      module function moment_4_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_cdp_cdp
      module function moment_5_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_cdp_cdp
      module function moment_6_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_cdp_cdp
      module function moment_7_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_cdp_cdp
      module function moment_8_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_cdp_cdp
      module function moment_9_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_cdp_cdp
      module function moment_10_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_cdp_cdp
      module function moment_11_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_cdp_cdp
      module function moment_12_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_cdp_cdp
      module function moment_13_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_cdp_cdp
      module function moment_14_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_cdp_cdp
      module function moment_15_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_cdp_cdp
      module function moment_1_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center
        logical, intent(in), optional :: mask
        complex(qp) :: res

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_cqp_cqp
      module function moment_2_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_cqp_cqp
      module function moment_3_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_3_cqp_cqp
      module function moment_4_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_cqp_cqp
      module function moment_5_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_cqp_cqp
      module function moment_6_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_cqp_cqp
      module function moment_7_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_cqp_cqp
      module function moment_8_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_cqp_cqp
      module function moment_9_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_cqp_cqp
      module function moment_10_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_cqp_cqp
      module function moment_11_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_cqp_cqp
      module function moment_12_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_cqp_cqp
      module function moment_13_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_cqp_cqp
      module function moment_14_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_cqp_cqp
      module function moment_15_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._qp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_cqp_cqp


      module function moment_1_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_iint8_dp
      module function moment_2_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_iint8_dp
      module function moment_3_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_iint8_dp
      module function moment_5_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_iint8_dp
      module function moment_6_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_iint8_dp
      module function moment_7_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_iint8_dp
      module function moment_8_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_iint8_dp
      module function moment_9_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_iint8_dp
      module function moment_10_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_iint8_dp
      module function moment_11_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_iint8_dp
      module function moment_12_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_iint8_dp
      module function moment_13_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_iint8_dp
      module function moment_14_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_iint8_dp
      module function moment_15_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_iint8_dp
      module function moment_1_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_iint16_dp
      module function moment_2_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_iint16_dp
      module function moment_3_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_iint16_dp
      module function moment_5_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_iint16_dp
      module function moment_6_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_iint16_dp
      module function moment_7_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_iint16_dp
      module function moment_8_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_iint16_dp
      module function moment_9_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_iint16_dp
      module function moment_10_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_iint16_dp
      module function moment_11_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_iint16_dp
      module function moment_12_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_iint16_dp
      module function moment_13_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_iint16_dp
      module function moment_14_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_iint16_dp
      module function moment_15_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_iint16_dp
      module function moment_1_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_iint32_dp
      module function moment_2_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_iint32_dp
      module function moment_3_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_iint32_dp
      module function moment_5_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_iint32_dp
      module function moment_6_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_iint32_dp
      module function moment_7_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_iint32_dp
      module function moment_8_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_iint32_dp
      module function moment_9_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_iint32_dp
      module function moment_10_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_iint32_dp
      module function moment_11_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_iint32_dp
      module function moment_12_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_iint32_dp
      module function moment_13_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_iint32_dp
      module function moment_14_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_iint32_dp
      module function moment_15_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_iint32_dp
      module function moment_1_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center
        logical, intent(in), optional :: mask
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_1_iint64_dp
      module function moment_2_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_2_iint64_dp
      module function moment_3_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_4_iint64_dp
      module function moment_5_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_5_iint64_dp
      module function moment_6_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_6_iint64_dp
      module function moment_7_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_7_iint64_dp
      module function moment_8_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_8_iint64_dp
      module function moment_9_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_9_iint64_dp
      module function moment_10_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_10_iint64_dp
      module function moment_11_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_11_iint64_dp
      module function moment_12_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_12_iint64_dp
      module function moment_13_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_13_iint64_dp
      module function moment_14_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_14_iint64_dp
      module function moment_15_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp),intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim),&
            & merge(size(x, 8), size(x, 9), mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x,&
            & 11), mask=10<dim), merge(size(x, 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim),&
            & merge(size(x, 13), size(x, 14), mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))
        logical, intent(in), optional :: mask
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim), merge(size(x, 7), size(x, 8), mask=7<dim), merge(size(x, 8), size(x, 9),&
            & mask=8<dim), merge(size(x, 9), size(x, 10), mask=9<dim), merge(size(x, 10), size(x, 11), mask=10<dim), merge(size(x,&
            & 11), size(x, 12), mask=11<dim), merge(size(x, 12), size(x, 13), mask=12<dim), merge(size(x, 13), size(x, 14),&
            & mask=13<dim), merge(size(x, 14), size(x, 15), mask=14<dim))

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (.not.optval(mask, .true.)) then
          res = ieee_value(1._dp, ieee_quiet_nan)
          return
        end if

        n = real(size(x, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 1))
              do i = 1, size(x, 1)
                res = res + (real(x(i, :, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 2))
              do i = 1, size(x, 2)
                res = res + (real(x(:, i, :, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 3))
              do i = 1, size(x, 3)
                res = res + (real(x(:, :, i, :, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 4))
              do i = 1, size(x, 4)
                res = res + (real(x(:, :, :, i, :, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 5))
              do i = 1, size(x, 5)
                res = res + (real(x(:, :, :, :, i, :, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 6))
              do i = 1, size(x, 6)
                res = res + (real(x(:, :, :, :, :, i, :, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 7))
              do i = 1, size(x, 7)
                res = res + (real(x(:, :, :, :, :, :, i, :, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(8)
            if (present(center)) then
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 8))
              do i = 1, size(x, 8)
                res = res + (real(x(:, :, :, :, :, :, :, i, :, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(9)
            if (present(center)) then
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 9))
              do i = 1, size(x, 9)
                res = res + (real(x(:, :, :, :, :, :, :, :, i, :, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(10)
            if (present(center)) then
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 10))
              do i = 1, size(x, 10)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, i, :, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(11)
            if (present(center)) then
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 11))
              do i = 1, size(x, 11)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, i, :, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(12)
            if (present(center)) then
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 12))
              do i = 1, size(x, 12)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, i, :, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(13)
            if (present(center)) then
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 13))
              do i = 1, size(x, 13)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, i, :, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(14)
            if (present(center)) then
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 14))
              do i = 1, size(x, 14)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, i, :), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case(15)
            if (present(center)) then
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) -&
                  center)**order
              end do
            else
              allocate(mean_, source = mean(x, 15))
              do i = 1, size(x, 15)
                res = res + (real(x(:, :, :, :, :, :, :, :, :, :, :, :, :, :, i), dp) - mean_)**order
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_15_iint64_dp

end submodule
