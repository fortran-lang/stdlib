!> Inspired by original code (MIT license) written in 2016 by Keurfon Luu (keurfonluu@outlook.com)
!> https://github.com/keurfonluu/Forlab

submodule (stdlib_math) stdlib_math_diff

    implicit none

contains

    !> `diff` computes differences of adjacent elements of an array.
    
    pure module function diff_1_sp(x, n, prepend, append) result(y)
        real(sp), intent(in) :: x(:)
        integer, intent(in), optional :: n
        real(sp), intent(in), optional :: prepend(:), append(:)
        real(sp), allocatable :: y(:)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend) 
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0))
            return
        end if

        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x-1)
            return
        end if

        block
        real(sp) :: work(size_work)
        if (size_prepend > 0) work(:size_prepend) = prepend
        work(size_prepend+1:size_prepend+size_x) = x
        if (size_append > 0) work(size_prepend+size_x+1:) = append
        
        do i = 1, n_
            work(1:size_work-i) = work(2:size_work-i+1) - work(1:size_work-i)
        end do

        y = work(1:size_work-n_)
        end block

    end function diff_1_sp

    pure module function diff_2_sp(x, n, dim, prepend, append) result(y)
        real(sp), intent(in) :: x(:, :)
        integer, intent(in), optional :: n, dim
        real(sp), intent(in), optional :: prepend(:, :), append(:, :)
        real(sp), allocatable :: y(:, :)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, dim_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(dim)) then
            if (dim == 1 .or. dim == 2) then
                dim_ = dim
            else
                dim_ = 1
            end if
        else
            dim_ = 1
        end if
        
        if (present(prepend)) size_prepend = size(prepend, dim_)
        if (present(append)) size_append = size(append, dim_)
        size_x = size(x, dim_)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0, 0))
            return
        end if
        
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            if (dim_ == 1) then
                y = x(2:, :) - x(1:size_x-1, :)
            elseif (dim_ == 2) then
                y = x(:, 2:) - x(:, 1:size_x-1)
            end if
            return
        end if
        
        if (dim_ == 1) then
            block
            real(sp) :: work(size_work, size(x, 2))
            if (size_prepend > 0) work(1:size_prepend, :) = prepend
            work(size_prepend+1:size_x+size_prepend, :) = x
            if (size_append > 0) work(size_x+size_prepend+1:, :) = append
            do i = 1, n_
                work(1:size_work-i, :) = work(2:size_work-i+1, :) - work(1:size_work-i, :)
            end do

            y = work(1:size_work-n_, :)
            end block
            
        elseif (dim_ == 2) then
            block
            real(sp) :: work(size(x, 1), size_work)
            if (size_prepend > 0) work(:, 1:size_prepend) = prepend
            work(:, size_prepend+1:size_x+size_prepend) = x
            if (size_append > 0) work(:, size_x+size_prepend+1:) = append
            do i = 1, n_
                work(:, 1:size_work-i) = work(:, 2:size_work-i+1) - work(:, 1:size_work-i)
            end do
            
            y = work(:, 1:size_work-n_)
            end block
            
        end if

    end function diff_2_sp
    pure module function diff_1_dp(x, n, prepend, append) result(y)
        real(dp), intent(in) :: x(:)
        integer, intent(in), optional :: n
        real(dp), intent(in), optional :: prepend(:), append(:)
        real(dp), allocatable :: y(:)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend) 
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0))
            return
        end if

        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x-1)
            return
        end if

        block
        real(dp) :: work(size_work)
        if (size_prepend > 0) work(:size_prepend) = prepend
        work(size_prepend+1:size_prepend+size_x) = x
        if (size_append > 0) work(size_prepend+size_x+1:) = append
        
        do i = 1, n_
            work(1:size_work-i) = work(2:size_work-i+1) - work(1:size_work-i)
        end do

        y = work(1:size_work-n_)
        end block

    end function diff_1_dp

    pure module function diff_2_dp(x, n, dim, prepend, append) result(y)
        real(dp), intent(in) :: x(:, :)
        integer, intent(in), optional :: n, dim
        real(dp), intent(in), optional :: prepend(:, :), append(:, :)
        real(dp), allocatable :: y(:, :)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, dim_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(dim)) then
            if (dim == 1 .or. dim == 2) then
                dim_ = dim
            else
                dim_ = 1
            end if
        else
            dim_ = 1
        end if
        
        if (present(prepend)) size_prepend = size(prepend, dim_)
        if (present(append)) size_append = size(append, dim_)
        size_x = size(x, dim_)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0, 0))
            return
        end if
        
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            if (dim_ == 1) then
                y = x(2:, :) - x(1:size_x-1, :)
            elseif (dim_ == 2) then
                y = x(:, 2:) - x(:, 1:size_x-1)
            end if
            return
        end if
        
        if (dim_ == 1) then
            block
            real(dp) :: work(size_work, size(x, 2))
            if (size_prepend > 0) work(1:size_prepend, :) = prepend
            work(size_prepend+1:size_x+size_prepend, :) = x
            if (size_append > 0) work(size_x+size_prepend+1:, :) = append
            do i = 1, n_
                work(1:size_work-i, :) = work(2:size_work-i+1, :) - work(1:size_work-i, :)
            end do

            y = work(1:size_work-n_, :)
            end block
            
        elseif (dim_ == 2) then
            block
            real(dp) :: work(size(x, 1), size_work)
            if (size_prepend > 0) work(:, 1:size_prepend) = prepend
            work(:, size_prepend+1:size_x+size_prepend) = x
            if (size_append > 0) work(:, size_x+size_prepend+1:) = append
            do i = 1, n_
                work(:, 1:size_work-i) = work(:, 2:size_work-i+1) - work(:, 1:size_work-i)
            end do
            
            y = work(:, 1:size_work-n_)
            end block
            
        end if

    end function diff_2_dp
    pure module function diff_1_int8(x, n, prepend, append) result(y)
        integer(int8), intent(in) :: x(:)
        integer, intent(in), optional :: n
        integer(int8), intent(in), optional :: prepend(:), append(:)
        integer(int8), allocatable :: y(:)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend) 
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0))
            return
        end if

        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x-1)
            return
        end if

        block
        integer(int8) :: work(size_work)
        if (size_prepend > 0) work(:size_prepend) = prepend
        work(size_prepend+1:size_prepend+size_x) = x
        if (size_append > 0) work(size_prepend+size_x+1:) = append
        
        do i = 1, n_
            work(1:size_work-i) = work(2:size_work-i+1) - work(1:size_work-i)
        end do

        y = work(1:size_work-n_)
        end block

    end function diff_1_int8

    pure module function diff_2_int8(x, n, dim, prepend, append) result(y)
        integer(int8), intent(in) :: x(:, :)
        integer, intent(in), optional :: n, dim
        integer(int8), intent(in), optional :: prepend(:, :), append(:, :)
        integer(int8), allocatable :: y(:, :)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, dim_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(dim)) then
            if (dim == 1 .or. dim == 2) then
                dim_ = dim
            else
                dim_ = 1
            end if
        else
            dim_ = 1
        end if
        
        if (present(prepend)) size_prepend = size(prepend, dim_)
        if (present(append)) size_append = size(append, dim_)
        size_x = size(x, dim_)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0, 0))
            return
        end if
        
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            if (dim_ == 1) then
                y = x(2:, :) - x(1:size_x-1, :)
            elseif (dim_ == 2) then
                y = x(:, 2:) - x(:, 1:size_x-1)
            end if
            return
        end if
        
        if (dim_ == 1) then
            block
            integer(int8) :: work(size_work, size(x, 2))
            if (size_prepend > 0) work(1:size_prepend, :) = prepend
            work(size_prepend+1:size_x+size_prepend, :) = x
            if (size_append > 0) work(size_x+size_prepend+1:, :) = append
            do i = 1, n_
                work(1:size_work-i, :) = work(2:size_work-i+1, :) - work(1:size_work-i, :)
            end do

            y = work(1:size_work-n_, :)
            end block
            
        elseif (dim_ == 2) then
            block
            integer(int8) :: work(size(x, 1), size_work)
            if (size_prepend > 0) work(:, 1:size_prepend) = prepend
            work(:, size_prepend+1:size_x+size_prepend) = x
            if (size_append > 0) work(:, size_x+size_prepend+1:) = append
            do i = 1, n_
                work(:, 1:size_work-i) = work(:, 2:size_work-i+1) - work(:, 1:size_work-i)
            end do
            
            y = work(:, 1:size_work-n_)
            end block
            
        end if

    end function diff_2_int8
    pure module function diff_1_int16(x, n, prepend, append) result(y)
        integer(int16), intent(in) :: x(:)
        integer, intent(in), optional :: n
        integer(int16), intent(in), optional :: prepend(:), append(:)
        integer(int16), allocatable :: y(:)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend) 
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0))
            return
        end if

        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x-1)
            return
        end if

        block
        integer(int16) :: work(size_work)
        if (size_prepend > 0) work(:size_prepend) = prepend
        work(size_prepend+1:size_prepend+size_x) = x
        if (size_append > 0) work(size_prepend+size_x+1:) = append
        
        do i = 1, n_
            work(1:size_work-i) = work(2:size_work-i+1) - work(1:size_work-i)
        end do

        y = work(1:size_work-n_)
        end block

    end function diff_1_int16

    pure module function diff_2_int16(x, n, dim, prepend, append) result(y)
        integer(int16), intent(in) :: x(:, :)
        integer, intent(in), optional :: n, dim
        integer(int16), intent(in), optional :: prepend(:, :), append(:, :)
        integer(int16), allocatable :: y(:, :)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, dim_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(dim)) then
            if (dim == 1 .or. dim == 2) then
                dim_ = dim
            else
                dim_ = 1
            end if
        else
            dim_ = 1
        end if
        
        if (present(prepend)) size_prepend = size(prepend, dim_)
        if (present(append)) size_append = size(append, dim_)
        size_x = size(x, dim_)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0, 0))
            return
        end if
        
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            if (dim_ == 1) then
                y = x(2:, :) - x(1:size_x-1, :)
            elseif (dim_ == 2) then
                y = x(:, 2:) - x(:, 1:size_x-1)
            end if
            return
        end if
        
        if (dim_ == 1) then
            block
            integer(int16) :: work(size_work, size(x, 2))
            if (size_prepend > 0) work(1:size_prepend, :) = prepend
            work(size_prepend+1:size_x+size_prepend, :) = x
            if (size_append > 0) work(size_x+size_prepend+1:, :) = append
            do i = 1, n_
                work(1:size_work-i, :) = work(2:size_work-i+1, :) - work(1:size_work-i, :)
            end do

            y = work(1:size_work-n_, :)
            end block
            
        elseif (dim_ == 2) then
            block
            integer(int16) :: work(size(x, 1), size_work)
            if (size_prepend > 0) work(:, 1:size_prepend) = prepend
            work(:, size_prepend+1:size_x+size_prepend) = x
            if (size_append > 0) work(:, size_x+size_prepend+1:) = append
            do i = 1, n_
                work(:, 1:size_work-i) = work(:, 2:size_work-i+1) - work(:, 1:size_work-i)
            end do
            
            y = work(:, 1:size_work-n_)
            end block
            
        end if

    end function diff_2_int16
    pure module function diff_1_int32(x, n, prepend, append) result(y)
        integer(int32), intent(in) :: x(:)
        integer, intent(in), optional :: n
        integer(int32), intent(in), optional :: prepend(:), append(:)
        integer(int32), allocatable :: y(:)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend) 
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0))
            return
        end if

        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x-1)
            return
        end if

        block
        integer(int32) :: work(size_work)
        if (size_prepend > 0) work(:size_prepend) = prepend
        work(size_prepend+1:size_prepend+size_x) = x
        if (size_append > 0) work(size_prepend+size_x+1:) = append
        
        do i = 1, n_
            work(1:size_work-i) = work(2:size_work-i+1) - work(1:size_work-i)
        end do

        y = work(1:size_work-n_)
        end block

    end function diff_1_int32

    pure module function diff_2_int32(x, n, dim, prepend, append) result(y)
        integer(int32), intent(in) :: x(:, :)
        integer, intent(in), optional :: n, dim
        integer(int32), intent(in), optional :: prepend(:, :), append(:, :)
        integer(int32), allocatable :: y(:, :)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, dim_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(dim)) then
            if (dim == 1 .or. dim == 2) then
                dim_ = dim
            else
                dim_ = 1
            end if
        else
            dim_ = 1
        end if
        
        if (present(prepend)) size_prepend = size(prepend, dim_)
        if (present(append)) size_append = size(append, dim_)
        size_x = size(x, dim_)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0, 0))
            return
        end if
        
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            if (dim_ == 1) then
                y = x(2:, :) - x(1:size_x-1, :)
            elseif (dim_ == 2) then
                y = x(:, 2:) - x(:, 1:size_x-1)
            end if
            return
        end if
        
        if (dim_ == 1) then
            block
            integer(int32) :: work(size_work, size(x, 2))
            if (size_prepend > 0) work(1:size_prepend, :) = prepend
            work(size_prepend+1:size_x+size_prepend, :) = x
            if (size_append > 0) work(size_x+size_prepend+1:, :) = append
            do i = 1, n_
                work(1:size_work-i, :) = work(2:size_work-i+1, :) - work(1:size_work-i, :)
            end do

            y = work(1:size_work-n_, :)
            end block
            
        elseif (dim_ == 2) then
            block
            integer(int32) :: work(size(x, 1), size_work)
            if (size_prepend > 0) work(:, 1:size_prepend) = prepend
            work(:, size_prepend+1:size_x+size_prepend) = x
            if (size_append > 0) work(:, size_x+size_prepend+1:) = append
            do i = 1, n_
                work(:, 1:size_work-i) = work(:, 2:size_work-i+1) - work(:, 1:size_work-i)
            end do
            
            y = work(:, 1:size_work-n_)
            end block
            
        end if

    end function diff_2_int32
    pure module function diff_1_int64(x, n, prepend, append) result(y)
        integer(int64), intent(in) :: x(:)
        integer, intent(in), optional :: n
        integer(int64), intent(in), optional :: prepend(:), append(:)
        integer(int64), allocatable :: y(:)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend) 
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0))
            return
        end if

        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x-1)
            return
        end if

        block
        integer(int64) :: work(size_work)
        if (size_prepend > 0) work(:size_prepend) = prepend
        work(size_prepend+1:size_prepend+size_x) = x
        if (size_append > 0) work(size_prepend+size_x+1:) = append
        
        do i = 1, n_
            work(1:size_work-i) = work(2:size_work-i+1) - work(1:size_work-i)
        end do

        y = work(1:size_work-n_)
        end block

    end function diff_1_int64

    pure module function diff_2_int64(x, n, dim, prepend, append) result(y)
        integer(int64), intent(in) :: x(:, :)
        integer, intent(in), optional :: n, dim
        integer(int64), intent(in), optional :: prepend(:, :), append(:, :)
        integer(int64), allocatable :: y(:, :)
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, dim_, i

        n_ = optval(n, 1)
        if (n_ <= 0) then
            y = x
            return
        end if
        
        size_prepend = 0
        size_append = 0
        if (present(dim)) then
            if (dim == 1 .or. dim == 2) then
                dim_ = dim
            else
                dim_ = 1
            end if
        else
            dim_ = 1
        end if
        
        if (present(prepend)) size_prepend = size(prepend, dim_)
        if (present(append)) size_append = size(append, dim_)
        size_x = size(x, dim_)
        size_work = size_x + size_prepend + size_append
        
        if (size_work <= n_) then
            allocate(y(0, 0))
            return
        end if
        
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            if (dim_ == 1) then
                y = x(2:, :) - x(1:size_x-1, :)
            elseif (dim_ == 2) then
                y = x(:, 2:) - x(:, 1:size_x-1)
            end if
            return
        end if
        
        if (dim_ == 1) then
            block
            integer(int64) :: work(size_work, size(x, 2))
            if (size_prepend > 0) work(1:size_prepend, :) = prepend
            work(size_prepend+1:size_x+size_prepend, :) = x
            if (size_append > 0) work(size_x+size_prepend+1:, :) = append
            do i = 1, n_
                work(1:size_work-i, :) = work(2:size_work-i+1, :) - work(1:size_work-i, :)
            end do

            y = work(1:size_work-n_, :)
            end block
            
        elseif (dim_ == 2) then
            block
            integer(int64) :: work(size(x, 1), size_work)
            if (size_prepend > 0) work(:, 1:size_prepend) = prepend
            work(:, size_prepend+1:size_x+size_prepend) = x
            if (size_append > 0) work(:, size_x+size_prepend+1:) = append
            do i = 1, n_
                work(:, 1:size_work-i) = work(:, 2:size_work-i+1) - work(:, 1:size_work-i)
            end do
            
            y = work(:, 1:size_work-n_)
            end block
            
        end if

    end function diff_2_int64

end submodule stdlib_math_diff