submodule(stdlib_math) stdlib_math_arange

contains

    !> `arange` creates a vector of the `real(sp)` type 
    !>  with evenly spaced values within a given interval.
    pure module function arange_r_sp(start, end, step) result(result)

        real(sp), intent(in) :: start
        real(sp), intent(in), optional :: end, step
        real(sp), allocatable :: result(:)
        
        real(sp) :: start_, end_, step_
        integer :: i

        start_ = merge(start, 1.0_sp, present(end))
        end_   = optval(end, start)
        step_  = optval(step, 1.0_sp)
        step_  = sign(merge(step_, 1.0_sp, step_ /= 0.0_sp), end_ - start_)

        allocate(result(floor((end_ - start_)/step_) + 1))

        result = [(start_ + (i - 1)*step_, i=1, size(result), 1)]

    end function arange_r_sp
    !> `arange` creates a vector of the `real(dp)` type 
    !>  with evenly spaced values within a given interval.
    pure module function arange_r_dp(start, end, step) result(result)

        real(dp), intent(in) :: start
        real(dp), intent(in), optional :: end, step
        real(dp), allocatable :: result(:)
        
        real(dp) :: start_, end_, step_
        integer :: i

        start_ = merge(start, 1.0_dp, present(end))
        end_   = optval(end, start)
        step_  = optval(step, 1.0_dp)
        step_  = sign(merge(step_, 1.0_dp, step_ /= 0.0_dp), end_ - start_)

        allocate(result(floor((end_ - start_)/step_) + 1))

        result = [(start_ + (i - 1)*step_, i=1, size(result), 1)]

    end function arange_r_dp

    !> `arange` creates a vector of the `integer(int8)` type 
    !>  with evenly spaced values within a given interval.
    pure module function arange_i_int8(start, end, step) result(result)

        integer(int8), intent(in) :: start
        integer(int8), intent(in), optional :: end, step
        integer(int8), allocatable :: result(:)
        
        integer(int8) :: start_, end_, step_
        integer(int8) :: i

        start_ = merge(start, 1_int8, present(end))
        end_   = optval(end, start)
        step_  = optval(step, 1_int8)
        step_  = sign(merge(step_, 1_int8, step_ /= 0_int8), end_ - start_)

        allocate(result((end_ - start_)/step_ + 1_int8))

        result = [(i, i=start_, end_, step_)]

    end function arange_i_int8
    !> `arange` creates a vector of the `integer(int16)` type 
    !>  with evenly spaced values within a given interval.
    pure module function arange_i_int16(start, end, step) result(result)

        integer(int16), intent(in) :: start
        integer(int16), intent(in), optional :: end, step
        integer(int16), allocatable :: result(:)
        
        integer(int16) :: start_, end_, step_
        integer(int16) :: i

        start_ = merge(start, 1_int16, present(end))
        end_   = optval(end, start)
        step_  = optval(step, 1_int16)
        step_  = sign(merge(step_, 1_int16, step_ /= 0_int16), end_ - start_)

        allocate(result((end_ - start_)/step_ + 1_int16))

        result = [(i, i=start_, end_, step_)]

    end function arange_i_int16
    !> `arange` creates a vector of the `integer(int32)` type 
    !>  with evenly spaced values within a given interval.
    pure module function arange_i_int32(start, end, step) result(result)

        integer(int32), intent(in) :: start
        integer(int32), intent(in), optional :: end, step
        integer(int32), allocatable :: result(:)
        
        integer(int32) :: start_, end_, step_
        integer(int32) :: i

        start_ = merge(start, 1_int32, present(end))
        end_   = optval(end, start)
        step_  = optval(step, 1_int32)
        step_  = sign(merge(step_, 1_int32, step_ /= 0_int32), end_ - start_)

        allocate(result((end_ - start_)/step_ + 1_int32))

        result = [(i, i=start_, end_, step_)]

    end function arange_i_int32
    !> `arange` creates a vector of the `integer(int64)` type 
    !>  with evenly spaced values within a given interval.
    pure module function arange_i_int64(start, end, step) result(result)

        integer(int64), intent(in) :: start
        integer(int64), intent(in), optional :: end, step
        integer(int64), allocatable :: result(:)
        
        integer(int64) :: start_, end_, step_
        integer(int64) :: i

        start_ = merge(start, 1_int64, present(end))
        end_   = optval(end, start)
        step_  = optval(step, 1_int64)
        step_  = sign(merge(step_, 1_int64, step_ /= 0_int64), end_ - start_)

        allocate(result((end_ - start_)/step_ + 1_int64))

        result = [(i, i=start_, end_, step_)]

    end function arange_i_int64

end submodule stdlib_math_arange
