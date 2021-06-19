submodule (stdlib_stats) stdlib_stats_moment_mask

  use, intrinsic:: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  implicit none

contains

      module function moment_mask_1_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(sp) :: res

        integer :: i
        real(sp) :: n
        real(sp), allocatable :: mean_

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i) -&
                  center)**order,&
                    0._sp,&
                    mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i) - mean_)**order,&
                    0._sp,&
                    mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_rsp_rsp
      module function moment_mask_2_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(sp), allocatable :: mean_(:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) -&
                  center)**order,&
                    0._sp,&
                    mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) - mean_)**order,&
                    0._sp,&
                    mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) -&
                  center)**order,&
                    0._sp,&
                    mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) - mean_)**order,&
                    0._sp,&
                    mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_rsp_rsp
      module function moment_mask_3_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(sp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) - mean_)**order,&
                    0._sp,&
                    mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) - mean_)**order,&
                    0._sp,&
                    mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) - mean_)**order,&
                    0._sp,&
                    mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(sp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_rsp_rsp
      module function moment_mask_5_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        real(sp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_rsp_rsp
      module function moment_mask_6_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        real(sp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_rsp_rsp
      module function moment_mask_7_rsp_rsp(x, order, dim, center, mask) result(res)
        real(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        real(sp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) -&
                  center)**order,&
                    0._sp,&
                    mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) - mean_)**order,&
                    0._sp,&
                    mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_rsp_rsp
      module function moment_mask_1_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i) -&
                  center)**order,&
                    0._dp,&
                    mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i) - mean_)**order,&
                    0._dp,&
                    mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_rdp_rdp
      module function moment_mask_2_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp), allocatable :: mean_(:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) -&
                  center)**order,&
                    0._dp,&
                    mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) - mean_)**order,&
                    0._dp,&
                    mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) -&
                  center)**order,&
                    0._dp,&
                    mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) - mean_)**order,&
                    0._dp,&
                    mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_rdp_rdp
      module function moment_mask_3_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) - mean_)**order,&
                    0._dp,&
                    mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) - mean_)**order,&
                    0._dp,&
                    mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) - mean_)**order,&
                    0._dp,&
                    mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_rdp_rdp
      module function moment_mask_5_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        real(dp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_rdp_rdp
      module function moment_mask_6_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_rdp_rdp
      module function moment_mask_7_rdp_rdp(x, order, dim, center, mask) result(res)
        real(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) -&
                  center)**order,&
                    0._dp,&
                    mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) - mean_)**order,&
                    0._dp,&
                    mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_rdp_rdp
      module function moment_mask_1_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(qp) :: res

        integer :: i
        real(qp) :: n
        real(qp), allocatable :: mean_

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i) -&
                  center)**order,&
                    0._qp,&
                    mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i) - mean_)**order,&
                    0._qp,&
                    mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_rqp_rqp
      module function moment_mask_2_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(qp), allocatable :: mean_(:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) -&
                  center)**order,&
                    0._qp,&
                    mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) - mean_)**order,&
                    0._qp,&
                    mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) -&
                  center)**order,&
                    0._qp,&
                    mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) - mean_)**order,&
                    0._qp,&
                    mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_rqp_rqp
      module function moment_mask_3_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(qp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) - mean_)**order,&
                    0._qp,&
                    mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) - mean_)**order,&
                    0._qp,&
                    mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) - mean_)**order,&
                    0._qp,&
                    mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_3_rqp_rqp
      module function moment_mask_4_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in) :: mask(:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(qp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_rqp_rqp
      module function moment_mask_5_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        real(qp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_rqp_rqp
      module function moment_mask_6_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        real(qp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_rqp_rqp
      module function moment_mask_7_rqp_rqp(x, order, dim, center, mask) result(res)
        real(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        real(qp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) -&
                  center)**order,&
                    0._qp,&
                    mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) - mean_)**order,&
                    0._qp,&
                    mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_rqp_rqp
      module function moment_mask_1_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        complex(sp) :: res

        integer :: i
        real(sp) :: n
        complex(sp), allocatable :: mean_

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_csp_csp
      module function moment_mask_2_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        complex(sp), allocatable :: mean_(:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_csp_csp
      module function moment_mask_3_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        complex(sp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_3_csp_csp
      module function moment_mask_4_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in) :: mask(:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        complex(sp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_csp_csp
      module function moment_mask_5_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        complex(sp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_csp_csp
      module function moment_mask_6_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        complex(sp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_csp_csp
      module function moment_mask_7_csp_csp(x, order, dim, center, mask) result(res)
        complex(sp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(sp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(sp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(sp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        complex(sp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), sp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=sp),&
                    mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_csp_csp
      module function moment_mask_1_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        complex(dp) :: res

        integer :: i
        real(dp) :: n
        complex(dp), allocatable :: mean_

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_cdp_cdp
      module function moment_mask_2_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        complex(dp), allocatable :: mean_(:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_cdp_cdp
      module function moment_mask_3_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        complex(dp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_3_cdp_cdp
      module function moment_mask_4_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in) :: mask(:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        complex(dp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_cdp_cdp
      module function moment_mask_5_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        complex(dp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_cdp_cdp
      module function moment_mask_6_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        complex(dp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_cdp_cdp
      module function moment_mask_7_cdp_cdp(x, order, dim, center, mask) result(res)
        complex(dp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        complex(dp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=dp),&
                    mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_cdp_cdp
      module function moment_mask_1_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        complex(qp) :: res

        integer :: i
        real(qp) :: n
        complex(qp), allocatable :: mean_

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_cqp_cqp
      module function moment_mask_2_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        complex(qp), allocatable :: mean_(:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_cqp_cqp
      module function moment_mask_3_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        complex(qp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_3_cqp_cqp
      module function moment_mask_4_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim))
        logical, intent(in) :: mask(:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        complex(qp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_cqp_cqp
      module function moment_mask_5_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        complex(qp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_cqp_cqp
      module function moment_mask_6_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        complex(qp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_cqp_cqp
      module function moment_mask_7_cqp_cqp(x, order, dim, center, mask) result(res)
        complex(qp), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        complex(qp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        complex(qp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(qp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        complex(qp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), qp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge( (x(i, :, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge( (x(:, i, :, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge( (x(:, :, i, :, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge( (x(:, :, :, i, :, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge( (x(:, :, :, :, i, :, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge( (x(:, :, :, :, :, i, :) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) -&
                  center)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge( (x(:, :, :, :, :, :, i) - mean_)**order,&
                    cmplx(0,0,kind=qp),&
                    mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_cqp_cqp


      module function moment_mask_1_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) -&
                                    center)**order,&
                                    0._dp, mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_iint8_dp
      module function moment_mask_2_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp), allocatable :: mean_(:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_iint8_dp
      module function moment_mask_3_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_iint8_dp
      module function moment_mask_5_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        real(dp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_iint8_dp
      module function moment_mask_6_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_iint8_dp
      module function moment_mask_7_iint8_dp(x, order, dim, center, mask) result(res)
        integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_iint8_dp
      module function moment_mask_1_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) -&
                                    center)**order,&
                                    0._dp, mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_iint16_dp
      module function moment_mask_2_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp), allocatable :: mean_(:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_iint16_dp
      module function moment_mask_3_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_iint16_dp
      module function moment_mask_5_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        real(dp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_iint16_dp
      module function moment_mask_6_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_iint16_dp
      module function moment_mask_7_iint16_dp(x, order, dim, center, mask) result(res)
        integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_iint16_dp
      module function moment_mask_1_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) -&
                                    center)**order,&
                                    0._dp, mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_iint32_dp
      module function moment_mask_2_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp), allocatable :: mean_(:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_iint32_dp
      module function moment_mask_3_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_iint32_dp
      module function moment_mask_5_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        real(dp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_iint32_dp
      module function moment_mask_6_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_iint32_dp
      module function moment_mask_7_iint32_dp(x, order, dim, center, mask) result(res)
        integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_iint32_dp
      module function moment_mask_1_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center
        logical, intent(in) :: mask(:)
        real(dp) :: res

        integer :: i
        real(dp) :: n
        real(dp), allocatable :: mean_

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) -&
                                    center)**order,&
                                    0._dp, mask(i))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_1_iint64_dp
      module function moment_mask_2_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim))
        logical, intent(in) :: mask(:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim))
        real(dp), allocatable :: mean_(:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_2_iint64_dp
      module function moment_mask_3_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim))
        logical, intent(in) :: mask(:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim))
        real(dp), allocatable :: mean_(:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

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

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim))
        real(dp), allocatable :: mean_(:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_4_iint64_dp
      module function moment_mask_5_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        logical, intent(in) :: mask(:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim))
        real(dp), allocatable :: mean_(:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_5_iint64_dp
      module function moment_mask_6_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_6_iint64_dp
      module function moment_mask_7_iint64_dp(x, order, dim, center, mask) result(res)
        integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
        integer, intent(in) :: order
        integer, intent(in) :: dim
        real(dp), intent(in), optional :: center(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3),&
            & mask=2<dim), merge(size(x, 3), size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5),&
            & size(x, 6), mask=5<dim), merge(size(x, 6), size(x, 7), mask=6<dim))
        logical, intent(in) :: mask(:,:,:,:,:,:,:)
        real(dp) :: res(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))

        integer :: i
        real(dp) :: n(merge(size(x, 1), size(x, 2), mask=1<dim), merge(size(x, 2), size(x, 3), mask=2<dim), merge(size(x, 3),&
            & size(x, 4), mask=3<dim), merge(size(x, 4), size(x, 5), mask=4<dim), merge(size(x, 5), size(x, 6), mask=5<dim),&
            & merge(size(x, 6), size(x, 7), mask=6<dim))
        real(dp), allocatable :: mean_(:,:,:,:,:,:)

        n = real(count(mask, dim), dp)

        res = 0
        select case(dim)
          case(1)
            if (present(center)) then
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 1, mask))
              do i = 1, size(x, 1)
                res = res + merge((real(x(i, :, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(i, :, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(2)
            if (present(center)) then
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 2, mask))
              do i = 1, size(x, 2)
                res = res + merge((real(x(:, i, :, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, i, :, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(3)
            if (present(center)) then
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 3, mask))
              do i = 1, size(x, 3)
                res = res + merge((real(x(:, :, i, :, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, i, :, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(4)
            if (present(center)) then
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 4, mask))
              do i = 1, size(x, 4)
                res = res + merge((real(x(:, :, :, i, :, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, i, :, :, :))
              end do
              deallocate(mean_)
            end if
          case(5)
            if (present(center)) then
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
            else
              allocate(mean_, source = mean(x, 5, mask))
              do i = 1, size(x, 5)
                res = res + merge((real(x(:, :, :, :, i, :, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, i, :, :))
              end do
              deallocate(mean_)
            end if
          case(6)
            if (present(center)) then
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
            else
              allocate(mean_, source = mean(x, 6, mask))
              do i = 1, size(x, 6)
                res = res + merge((real(x(:, :, :, :, :, i, :), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, i, :))
              end do
              deallocate(mean_)
            end if
          case(7)
            if (present(center)) then
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) -&
                                    center)**order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
            else
              allocate(mean_, source = mean(x, 7, mask))
              do i = 1, size(x, 7)
                res = res + merge((real(x(:, :, :, :, :, :, i), dp) - mean_)&
                                    **order,&
                                    0._dp, mask(:, :, :, :, :, :, i))
              end do
              deallocate(mean_)
            end if
          case default
            call error_stop("ERROR (moment): wrong dimension")
        end select
        res = res / n

      end function moment_mask_7_iint64_dp

end submodule
