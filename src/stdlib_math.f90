module stdlib_math
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, xdp, qp
    use stdlib_optval, only: optval
    use stdlib_bitsets, only: bitset_64, bitset_large

    implicit none
    private
    public :: clip, swap, gcd, linspace, logspace
    public :: EULERS_NUMBER_SP, EULERS_NUMBER_DP
    public :: DEFAULT_LINSPACE_LENGTH, DEFAULT_LOGSPACE_BASE, DEFAULT_LOGSPACE_LENGTH
    public :: stdlib_meshgrid_ij, stdlib_meshgrid_xy
    public :: arange, arg, argd, argpi, deg2rad, rad2deg, is_close, all_close, diff, meshgrid

    integer, parameter :: DEFAULT_LINSPACE_LENGTH = 100
    integer, parameter :: DEFAULT_LOGSPACE_LENGTH = 50
    integer, parameter :: DEFAULT_LOGSPACE_BASE = 10

    ! Useful constants for lnspace
    real(sp), parameter :: EULERS_NUMBER_SP = exp(1.0_sp)
    real(dp), parameter :: EULERS_NUMBER_DP = exp(1.0_dp)

    !> Useful constants `PI` for `argd/argpi`
    real(kind=sp), parameter :: PI_sp = acos(-1.0_sp)
    real(kind=dp), parameter :: PI_dp = acos(-1.0_dp)

    !> Values for optional argument `indexing` of `meshgrid`
    integer, parameter :: stdlib_meshgrid_xy = 0, stdlib_meshgrid_ij = 1

    interface clip
        module procedure clip_int8
        module procedure clip_int16
        module procedure clip_int32
        module procedure clip_int64
        module procedure clip_sp
        module procedure clip_dp
    end interface clip

    !> Swap the values of the lhs and rhs arguments
    !> ([Specification](../page/specs/stdlib_math.html#swap_subroutine))
    !>
    !> Version: experimental
    interface swap
      module procedure :: swap_int8
      module procedure :: swap_int16
      module procedure :: swap_int32
      module procedure :: swap_int64
      module procedure :: swap_sp
      module procedure :: swap_dp
      module procedure :: swap_bitset_64
      module procedure :: swap_bitset_large
      module procedure :: swap_csp
      module procedure :: swap_cdp
      module procedure :: swap_bool
      module procedure :: swap_str
      module procedure :: swap_stt
    end interface

    !> Returns the greatest common divisor of two integers
    !> ([Specification](../page/specs/stdlib_math.html#gcd))
    !>
    !> Version: experimental
    interface gcd
        module procedure gcd_int8
        module procedure gcd_int16
        module procedure gcd_int32
        module procedure gcd_int64
    end interface gcd

    interface linspace
    !! Version: Experimental
    !!
    !! Create rank 1 array of linearly spaced elements
    !! If the number of elements is not specified, create an array with size 100. If n is a negative value,
    !! return an array with size 0. If n = 1, return an array whose only element is end
    !!([Specification](../page/specs/stdlib_math.html#linspace-create-a-linearly-spaced-rank-one-array))
      pure module function linspace_default_1_rsp_rsp(start, end) result(res)
        real(sp), intent(in) :: start
        real(sp), intent(in) :: end

        real(sp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_rsp_rsp
      pure module function linspace_default_1_rdp_rdp(start, end) result(res)
        real(dp), intent(in) :: start
        real(dp), intent(in) :: end

        real(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_rdp_rdp
      pure module function linspace_default_1_csp_csp(start, end) result(res)
        complex(sp), intent(in) :: start
        complex(sp), intent(in) :: end

        complex(sp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_csp_csp
      pure module function linspace_default_1_cdp_cdp(start, end) result(res)
        complex(dp), intent(in) :: start
        complex(dp), intent(in) :: end

        complex(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_cdp_cdp

      pure module function linspace_n_1_rsp_rsp(start, end, n) result(res)
        real(sp), intent(in) :: start
        real(sp), intent(in) :: end
        integer, intent(in) :: n

        real(sp) :: res(max(n, 0))
      end function linspace_n_1_rsp_rsp
      pure module function linspace_n_1_rdp_rdp(start, end, n) result(res)
        real(dp), intent(in) :: start
        real(dp), intent(in) :: end
        integer, intent(in) :: n

        real(dp) :: res(max(n, 0))
      end function linspace_n_1_rdp_rdp
      pure module function linspace_n_1_csp_csp(start, end, n) result(res)
        complex(sp), intent(in) :: start
        complex(sp), intent(in) :: end
        integer, intent(in) :: n

        complex(sp) :: res(max(n, 0))
      end function linspace_n_1_csp_csp
      pure module function linspace_n_1_cdp_cdp(start, end, n) result(res)
        complex(dp), intent(in) :: start
        complex(dp), intent(in) :: end
        integer, intent(in) :: n

        complex(dp) :: res(max(n, 0))
      end function linspace_n_1_cdp_cdp


    ! Add support for integer linspace
    !!
    !! When dealing with integers as the `start` and `end` parameters, the return type is always a `real(dp)`.
      pure module function linspace_default_1_iint8_iint8(start, end) result(res)
        integer(int8), intent(in) :: start
        integer(int8), intent(in) :: end

        real(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_iint8_iint8
      pure module function linspace_default_1_iint16_iint16(start, end) result(res)
        integer(int16), intent(in) :: start
        integer(int16), intent(in) :: end

        real(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_iint16_iint16
      pure module function linspace_default_1_iint32_iint32(start, end) result(res)
        integer(int32), intent(in) :: start
        integer(int32), intent(in) :: end

        real(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_iint32_iint32
      pure module function linspace_default_1_iint64_iint64(start, end) result(res)
        integer(int64), intent(in) :: start
        integer(int64), intent(in) :: end

        real(dp) :: res(DEFAULT_LINSPACE_LENGTH)
      end function linspace_default_1_iint64_iint64

      pure module function linspace_n_1_iint8_iint8(start, end, n) result(res)
        integer(int8), intent(in) :: start
        integer(int8), intent(in) :: end
        integer, intent(in) :: n

        real(dp) :: res(max(n, 0))
      end function linspace_n_1_iint8_iint8
      pure module function linspace_n_1_iint16_iint16(start, end, n) result(res)
        integer(int16), intent(in) :: start
        integer(int16), intent(in) :: end
        integer, intent(in) :: n

        real(dp) :: res(max(n, 0))
      end function linspace_n_1_iint16_iint16
      pure module function linspace_n_1_iint32_iint32(start, end, n) result(res)
        integer(int32), intent(in) :: start
        integer(int32), intent(in) :: end
        integer, intent(in) :: n

        real(dp) :: res(max(n, 0))
      end function linspace_n_1_iint32_iint32
      pure module function linspace_n_1_iint64_iint64(start, end, n) result(res)
        integer(int64), intent(in) :: start
        integer(int64), intent(in) :: end
        integer, intent(in) :: n

        real(dp) :: res(max(n, 0))
      end function linspace_n_1_iint64_iint64

  end interface

  interface logspace
  !! Version: Experimental
  !!
  !! Create rank 1 array of logarithmically spaced elements from base**start to base**end.
  !! If the number of elements is not specified, create an array with size 50. If n is a negative value,
  !! return an array with size 0. If n = 1, return an array whose only element is base**end. If no base
  !! is specified, logspace will default to using a base of 10
  !!
  !!([Specification](../page/specs/stdlib_math.html#logspace-create-a-logarithmically-spaced-rank-one-array))
    pure module function logspace_1_rsp_default(start, end) result(res)

      real(sp), intent(in) :: start
      real(sp), intent(in) :: end

      real(sp) :: res(DEFAULT_LOGSPACE_LENGTH)

    end function logspace_1_rsp_default
    pure module function logspace_1_rdp_default(start, end) result(res)

      real(dp), intent(in) :: start
      real(dp), intent(in) :: end

      real(dp) :: res(DEFAULT_LOGSPACE_LENGTH)

    end function logspace_1_rdp_default
    pure module function logspace_1_csp_default(start, end) result(res)

      complex(sp), intent(in) :: start
      complex(sp), intent(in) :: end

      complex(sp) :: res(DEFAULT_LOGSPACE_LENGTH)

    end function logspace_1_csp_default
    pure module function logspace_1_cdp_default(start, end) result(res)

      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end

      complex(dp) :: res(DEFAULT_LOGSPACE_LENGTH)

    end function logspace_1_cdp_default
    pure module function logspace_1_iint32_default(start, end) result(res)

      integer, intent(in) :: start
      integer, intent(in) :: end

      real(dp) :: res(DEFAULT_LOGSPACE_LENGTH)

  end function logspace_1_iint32_default

    pure module function logspace_1_rsp_n(start, end, n) result(res)
      real(sp), intent(in) :: start
      real(sp), intent(in) :: end
      integer, intent(in) :: n

      real(sp) :: res(max(n, 0))
    end function logspace_1_rsp_n
    pure module function logspace_1_rdp_n(start, end, n) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n

      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n
    pure module function logspace_1_csp_n(start, end, n) result(res)
      complex(sp), intent(in) :: start
      complex(sp), intent(in) :: end
      integer, intent(in) :: n

      complex(sp) :: res(max(n, 0))
    end function logspace_1_csp_n
    pure module function logspace_1_cdp_n(start, end, n) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n

      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n
    pure module function logspace_1_iint32_n(start, end, n) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n

      real(dp) :: res(n)
    end function logspace_1_iint32_n

    ! Generate logarithmically spaced sequence from sp base to the powers
    ! of sp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_rsp_n_rbase(start, end, n, base) result(res)
      real(sp), intent(in) :: start
      real(sp), intent(in) :: end
      integer, intent(in) :: n
      real(sp), intent(in) :: base
      ! real(sp) endpoints + real(sp) base = real(sp) result
      real(sp) :: res(max(n, 0))
    end function logspace_1_rsp_n_rbase

    pure module function logspace_1_rsp_n_cbase(start, end, n, base) result(res)
      real(sp), intent(in) :: start
      real(sp), intent(in) :: end
      integer, intent(in) :: n
      complex(sp), intent(in) :: base
      ! real(sp) endpoints + complex(sp) base = complex(sp) result
      real(sp) :: res(max(n, 0))
    end function logspace_1_rsp_n_cbase

    pure module function logspace_1_rsp_n_ibase(start, end, n, base) result(res)
      real(sp), intent(in) :: start
      real(sp), intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! real(sp) endpoints + integer base = real(sp) result
      real(sp) :: res(max(n, 0))
    end function logspace_1_rsp_n_ibase
    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_rdp_n_rbase(start, end, n, base) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n
      real(dp), intent(in) :: base
      ! real(dp) endpoints + real(dp) base = real(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n_rbase

    pure module function logspace_1_rdp_n_cbase(start, end, n, base) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n
      complex(dp), intent(in) :: base
      ! real(dp) endpoints + complex(dp) base = complex(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n_cbase

    pure module function logspace_1_rdp_n_ibase(start, end, n, base) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! real(dp) endpoints + integer base = real(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_rdp_n_ibase
    ! Generate logarithmically spaced sequence from sp base to the powers
    ! of sp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_csp_n_rbase(start, end, n, base) result(res)
      complex(sp), intent(in) :: start
      complex(sp), intent(in) :: end
      integer, intent(in) :: n
      real(sp), intent(in) :: base
      ! complex(sp) endpoints + real(sp) base = complex(sp) result
      complex(sp) :: res(max(n, 0))
    end function logspace_1_csp_n_rbase

    pure module function logspace_1_csp_n_cbase(start, end, n, base) result(res)
      complex(sp), intent(in) :: start
      complex(sp), intent(in) :: end
      integer, intent(in) :: n
      complex(sp), intent(in) :: base
      ! complex(sp) endpoints + complex(sp) base = complex(sp) result
      complex(sp) :: res(max(n, 0))
    end function logspace_1_csp_n_cbase

    pure module function logspace_1_csp_n_ibase(start, end, n, base) result(res)
      complex(sp), intent(in) :: start
      complex(sp), intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! complex(sp) endpoints + integer base = complex(sp) result
      complex(sp) :: res(max(n, 0))
    end function logspace_1_csp_n_ibase
    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_cdp_n_rbase(start, end, n, base) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n
      real(dp), intent(in) :: base
      ! complex(dp) endpoints + real(dp) base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n_rbase

    pure module function logspace_1_cdp_n_cbase(start, end, n, base) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n
      complex(dp), intent(in) :: base
      ! complex(dp) endpoints + complex(dp) base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n_cbase

    pure module function logspace_1_cdp_n_ibase(start, end, n, base) result(res)
      complex(dp), intent(in) :: start
      complex(dp), intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! complex(dp) endpoints + integer base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_cdp_n_ibase
    ! Generate logarithmically spaced sequence from dp base to the powers
    ! of dp start and end. [base^start, ... , base^end]
    ! Different combinations of parameter types will lead to different result types.
    ! Those combinations are indicated in the body of each function.
    pure module function logspace_1_iint32_n_rspbase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      real(sp), intent(in) :: base
      ! integer endpoints + real(sp) base = real(sp) result
      real(sp) :: res(max(n, 0))
    end function logspace_1_iint32_n_rspbase

    pure module function logspace_1_iint32_n_cspbase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      complex(sp), intent(in) :: base
      ! integer endpoints + complex(sp) base = complex(sp) result
      complex(sp) :: res(max(n, 0))
    end function logspace_1_iint32_n_cspbase
    pure module function logspace_1_iint32_n_rdpbase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      real(dp), intent(in) :: base
      ! integer endpoints + real(dp) base = real(dp) result
      real(dp) :: res(max(n, 0))
    end function logspace_1_iint32_n_rdpbase

    pure module function logspace_1_iint32_n_cdpbase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      complex(dp), intent(in) :: base
      ! integer endpoints + complex(dp) base = complex(dp) result
      complex(dp) :: res(max(n, 0))
    end function logspace_1_iint32_n_cdpbase

    pure module function logspace_1_iint32_n_ibase(start, end, n, base) result(res)
      integer, intent(in) :: start
      integer, intent(in) :: end
      integer, intent(in) :: n
      integer, intent(in) :: base
      ! integer endpoints + integer base = integer result
      integer :: res(max(n, 0))
    end function logspace_1_iint32_n_ibase


  end interface

    !> Version: experimental
    !>
    !> `arange` creates a one-dimensional `array` of the `integer/real` type 
    !>  with fixed-spaced values of given spacing, within a given interval.
    !> ([Specification](../page/specs/stdlib_math.html#arange-function))
    interface arange
        pure module function arange_r_sp(start, end, step) result(result)
            real(sp), intent(in) :: start
            real(sp), intent(in), optional :: end, step
            real(sp), allocatable :: result(:)
        end function arange_r_sp
        pure module function arange_r_dp(start, end, step) result(result)
            real(dp), intent(in) :: start
            real(dp), intent(in), optional :: end, step
            real(dp), allocatable :: result(:)
        end function arange_r_dp
        pure module function arange_i_int8(start, end, step) result(result)
            integer(int8), intent(in) :: start
            integer(int8), intent(in), optional :: end, step
            integer(int8), allocatable :: result(:)
        end function arange_i_int8
        pure module function arange_i_int16(start, end, step) result(result)
            integer(int16), intent(in) :: start
            integer(int16), intent(in), optional :: end, step
            integer(int16), allocatable :: result(:)
        end function arange_i_int16
        pure module function arange_i_int32(start, end, step) result(result)
            integer(int32), intent(in) :: start
            integer(int32), intent(in), optional :: end, step
            integer(int32), allocatable :: result(:)
        end function arange_i_int32
        pure module function arange_i_int64(start, end, step) result(result)
            integer(int64), intent(in) :: start
            integer(int64), intent(in), optional :: end, step
            integer(int64), allocatable :: result(:)
        end function arange_i_int64
    end interface arange

    !> Version: experimental
    !>
    !> `arg` computes the phase angle in the interval (-π,π].
    !> ([Specification](../page/specs/stdlib_math.html#arg-function))
    interface arg
        procedure :: arg_sp
        procedure :: arg_dp
    end interface arg

    !> Version: experimental
    !>
    !> `argd` computes the phase angle of degree version in the interval (-180.0,180.0].
    !> ([Specification](../page/specs/stdlib_math.html#argd-function))
    interface argd
        procedure :: argd_sp
        procedure :: argd_dp
    end interface argd

    !> Version: experimental
    !>
    !> `argpi` computes the phase angle of circular version in the interval (-1.0,1.0].
    !> ([Specification](../page/specs/stdlib_math.html#argpi-function))
    interface argpi
        procedure :: argpi_sp
        procedure :: argpi_dp
    end interface argpi

    !> Version: experimental
    !> 
    !> `deg2rad` converts phase angles from degrees to radians.
    !> ([Specification](../page/specs/stdlib_math.html#deg2rad-function))
    interface deg2rad
        procedure :: deg2rad_sp
        procedure :: deg2rad_dp
    end interface deg2rad
    
    !> Version: experimental
    !> 
    !> `rad2deg` converts phase angles from radians to degrees.
    !> ([Specification](../page/specs/stdlib_math.html#rad2deg-function))
    interface rad2deg
        procedure :: rad2deg_sp
        procedure :: rad2deg_dp
    end interface rad2deg
    
    !> Returns a boolean scalar/array where two scalar/arrays are element-wise equal within a tolerance.
    !> ([Specification](../page/specs/stdlib_math.html#is_close-function))
    interface is_close
        elemental module logical function is_close_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a, b
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_rsp
        elemental module logical function is_close_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a, b
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_rdp
        elemental module logical function is_close_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a, b
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_csp
        elemental module logical function is_close_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a, b
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function is_close_cdp
    end interface is_close

    !> Version: experimental
    !>
    !> Returns a boolean scalar where two arrays are element-wise equal within a tolerance.
    !> ([Specification](../page/specs/stdlib_math.html#all_close-function))
    interface all_close
        logical pure module function all_close_1_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:), b(:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_rsp
        logical pure module function all_close_2_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:,:), b(:,:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_rsp
        logical pure module function all_close_3_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:,:,:), b(:,:,:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_rsp
        logical pure module function all_close_4_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(sp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_rsp
        logical pure module function all_close_1_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:), b(:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_rdp
        logical pure module function all_close_2_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:,:), b(:,:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_rdp
        logical pure module function all_close_3_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:,:,:), b(:,:,:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_rdp
        logical pure module function all_close_4_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(dp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_rdp
        logical pure module function all_close_1_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:), b(:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_csp
        logical pure module function all_close_2_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:,:), b(:,:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_csp
        logical pure module function all_close_3_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:,:,:), b(:,:,:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_csp
        logical pure module function all_close_4_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(sp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
            real(sp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_csp
        logical pure module function all_close_1_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:), b(:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_1_cdp
        logical pure module function all_close_2_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:,:), b(:,:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_2_cdp
        logical pure module function all_close_3_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:,:,:), b(:,:,:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_3_cdp
        logical pure module function all_close_4_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)
            complex(dp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
            real(dp), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
        end function all_close_4_cdp
    end interface all_close
    
    !> Version: experimental
    !>
    !> Computes differences between adjacent elements of an array.
    !> ([Specification](../page/specs/stdlib_math.html#diff-function))
    interface diff
        pure module function diff_1_sp(x, n, prepend, append) result(y)
            real(sp), intent(in) :: x(:)
            integer, intent(in), optional :: n
            real(sp), intent(in), optional :: prepend(:), append(:)
            real(sp), allocatable :: y(:)
        end function diff_1_sp
        pure module function diff_2_sp(X, n, dim, prepend, append) result(y)
            real(sp), intent(in) :: x(:, :)
            integer, intent(in), optional :: n, dim
            real(sp), intent(in), optional :: prepend(:, :), append(:, :)
            real(sp), allocatable :: y(:, :)
        end function diff_2_sp
        pure module function diff_1_dp(x, n, prepend, append) result(y)
            real(dp), intent(in) :: x(:)
            integer, intent(in), optional :: n
            real(dp), intent(in), optional :: prepend(:), append(:)
            real(dp), allocatable :: y(:)
        end function diff_1_dp
        pure module function diff_2_dp(X, n, dim, prepend, append) result(y)
            real(dp), intent(in) :: x(:, :)
            integer, intent(in), optional :: n, dim
            real(dp), intent(in), optional :: prepend(:, :), append(:, :)
            real(dp), allocatable :: y(:, :)
        end function diff_2_dp
        pure module function diff_1_int8(x, n, prepend, append) result(y)
            integer(int8), intent(in) :: x(:)
            integer, intent(in), optional :: n
            integer(int8), intent(in), optional :: prepend(:), append(:)
            integer(int8), allocatable :: y(:)
        end function diff_1_int8
        pure module function diff_2_int8(X, n, dim, prepend, append) result(y)
            integer(int8), intent(in) :: x(:, :)
            integer, intent(in), optional :: n, dim
            integer(int8), intent(in), optional :: prepend(:, :), append(:, :)
            integer(int8), allocatable :: y(:, :)
        end function diff_2_int8
        pure module function diff_1_int16(x, n, prepend, append) result(y)
            integer(int16), intent(in) :: x(:)
            integer, intent(in), optional :: n
            integer(int16), intent(in), optional :: prepend(:), append(:)
            integer(int16), allocatable :: y(:)
        end function diff_1_int16
        pure module function diff_2_int16(X, n, dim, prepend, append) result(y)
            integer(int16), intent(in) :: x(:, :)
            integer, intent(in), optional :: n, dim
            integer(int16), intent(in), optional :: prepend(:, :), append(:, :)
            integer(int16), allocatable :: y(:, :)
        end function diff_2_int16
        pure module function diff_1_int32(x, n, prepend, append) result(y)
            integer(int32), intent(in) :: x(:)
            integer, intent(in), optional :: n
            integer(int32), intent(in), optional :: prepend(:), append(:)
            integer(int32), allocatable :: y(:)
        end function diff_1_int32
        pure module function diff_2_int32(X, n, dim, prepend, append) result(y)
            integer(int32), intent(in) :: x(:, :)
            integer, intent(in), optional :: n, dim
            integer(int32), intent(in), optional :: prepend(:, :), append(:, :)
            integer(int32), allocatable :: y(:, :)
        end function diff_2_int32
        pure module function diff_1_int64(x, n, prepend, append) result(y)
            integer(int64), intent(in) :: x(:)
            integer, intent(in), optional :: n
            integer(int64), intent(in), optional :: prepend(:), append(:)
            integer(int64), allocatable :: y(:)
        end function diff_1_int64
        pure module function diff_2_int64(X, n, dim, prepend, append) result(y)
            integer(int64), intent(in) :: x(:, :)
            integer, intent(in), optional :: n, dim
            integer(int64), intent(in), optional :: prepend(:, :), append(:, :)
            integer(int64), allocatable :: y(:, :)
        end function diff_2_int64
    end interface diff


    !> Version: experimental
    !>
    !> Computes a list of coordinate matrices from coordinate vectors.
    !> ([Specification](../page/specs/stdlib_math.html#meshgrid))
    interface meshgrid
        module subroutine meshgrid_1_iint8_iint8(&
                x1,  &
                xm1,  &
                indexing &
        )
            integer(int8), intent(in) :: x1(:)
            integer(int8), intent(out) :: xm1 (:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_1_iint8_iint8
        module subroutine meshgrid_2_iint8_iint8(&
                x1, x2,  &
                xm1, xm2,  &
                indexing &
        )
            integer(int8), intent(in) :: x1(:)
            integer(int8), intent(out) :: xm1 (:,:)
            integer(int8), intent(in) :: x2(:)
            integer(int8), intent(out) :: xm2 (:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_2_iint8_iint8
        module subroutine meshgrid_3_iint8_iint8(&
                x1, x2, x3,  &
                xm1, xm2, xm3,  &
                indexing &
        )
            integer(int8), intent(in) :: x1(:)
            integer(int8), intent(out) :: xm1 (:,:,:)
            integer(int8), intent(in) :: x2(:)
            integer(int8), intent(out) :: xm2 (:,:,:)
            integer(int8), intent(in) :: x3(:)
            integer(int8), intent(out) :: xm3 (:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_3_iint8_iint8
        module subroutine meshgrid_4_iint8_iint8(&
                x1, x2, x3, x4,  &
                xm1, xm2, xm3, xm4,  &
                indexing &
        )
            integer(int8), intent(in) :: x1(:)
            integer(int8), intent(out) :: xm1 (:,:,:,:)
            integer(int8), intent(in) :: x2(:)
            integer(int8), intent(out) :: xm2 (:,:,:,:)
            integer(int8), intent(in) :: x3(:)
            integer(int8), intent(out) :: xm3 (:,:,:,:)
            integer(int8), intent(in) :: x4(:)
            integer(int8), intent(out) :: xm4 (:,:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_4_iint8_iint8
        module subroutine meshgrid_1_iint16_iint16(&
                x1,  &
                xm1,  &
                indexing &
        )
            integer(int16), intent(in) :: x1(:)
            integer(int16), intent(out) :: xm1 (:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_1_iint16_iint16
        module subroutine meshgrid_2_iint16_iint16(&
                x1, x2,  &
                xm1, xm2,  &
                indexing &
        )
            integer(int16), intent(in) :: x1(:)
            integer(int16), intent(out) :: xm1 (:,:)
            integer(int16), intent(in) :: x2(:)
            integer(int16), intent(out) :: xm2 (:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_2_iint16_iint16
        module subroutine meshgrid_3_iint16_iint16(&
                x1, x2, x3,  &
                xm1, xm2, xm3,  &
                indexing &
        )
            integer(int16), intent(in) :: x1(:)
            integer(int16), intent(out) :: xm1 (:,:,:)
            integer(int16), intent(in) :: x2(:)
            integer(int16), intent(out) :: xm2 (:,:,:)
            integer(int16), intent(in) :: x3(:)
            integer(int16), intent(out) :: xm3 (:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_3_iint16_iint16
        module subroutine meshgrid_4_iint16_iint16(&
                x1, x2, x3, x4,  &
                xm1, xm2, xm3, xm4,  &
                indexing &
        )
            integer(int16), intent(in) :: x1(:)
            integer(int16), intent(out) :: xm1 (:,:,:,:)
            integer(int16), intent(in) :: x2(:)
            integer(int16), intent(out) :: xm2 (:,:,:,:)
            integer(int16), intent(in) :: x3(:)
            integer(int16), intent(out) :: xm3 (:,:,:,:)
            integer(int16), intent(in) :: x4(:)
            integer(int16), intent(out) :: xm4 (:,:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_4_iint16_iint16
        module subroutine meshgrid_1_iint32_iint32(&
                x1,  &
                xm1,  &
                indexing &
        )
            integer(int32), intent(in) :: x1(:)
            integer(int32), intent(out) :: xm1 (:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_1_iint32_iint32
        module subroutine meshgrid_2_iint32_iint32(&
                x1, x2,  &
                xm1, xm2,  &
                indexing &
        )
            integer(int32), intent(in) :: x1(:)
            integer(int32), intent(out) :: xm1 (:,:)
            integer(int32), intent(in) :: x2(:)
            integer(int32), intent(out) :: xm2 (:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_2_iint32_iint32
        module subroutine meshgrid_3_iint32_iint32(&
                x1, x2, x3,  &
                xm1, xm2, xm3,  &
                indexing &
        )
            integer(int32), intent(in) :: x1(:)
            integer(int32), intent(out) :: xm1 (:,:,:)
            integer(int32), intent(in) :: x2(:)
            integer(int32), intent(out) :: xm2 (:,:,:)
            integer(int32), intent(in) :: x3(:)
            integer(int32), intent(out) :: xm3 (:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_3_iint32_iint32
        module subroutine meshgrid_4_iint32_iint32(&
                x1, x2, x3, x4,  &
                xm1, xm2, xm3, xm4,  &
                indexing &
        )
            integer(int32), intent(in) :: x1(:)
            integer(int32), intent(out) :: xm1 (:,:,:,:)
            integer(int32), intent(in) :: x2(:)
            integer(int32), intent(out) :: xm2 (:,:,:,:)
            integer(int32), intent(in) :: x3(:)
            integer(int32), intent(out) :: xm3 (:,:,:,:)
            integer(int32), intent(in) :: x4(:)
            integer(int32), intent(out) :: xm4 (:,:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_4_iint32_iint32
        module subroutine meshgrid_1_iint64_iint64(&
                x1,  &
                xm1,  &
                indexing &
        )
            integer(int64), intent(in) :: x1(:)
            integer(int64), intent(out) :: xm1 (:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_1_iint64_iint64
        module subroutine meshgrid_2_iint64_iint64(&
                x1, x2,  &
                xm1, xm2,  &
                indexing &
        )
            integer(int64), intent(in) :: x1(:)
            integer(int64), intent(out) :: xm1 (:,:)
            integer(int64), intent(in) :: x2(:)
            integer(int64), intent(out) :: xm2 (:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_2_iint64_iint64
        module subroutine meshgrid_3_iint64_iint64(&
                x1, x2, x3,  &
                xm1, xm2, xm3,  &
                indexing &
        )
            integer(int64), intent(in) :: x1(:)
            integer(int64), intent(out) :: xm1 (:,:,:)
            integer(int64), intent(in) :: x2(:)
            integer(int64), intent(out) :: xm2 (:,:,:)
            integer(int64), intent(in) :: x3(:)
            integer(int64), intent(out) :: xm3 (:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_3_iint64_iint64
        module subroutine meshgrid_4_iint64_iint64(&
                x1, x2, x3, x4,  &
                xm1, xm2, xm3, xm4,  &
                indexing &
        )
            integer(int64), intent(in) :: x1(:)
            integer(int64), intent(out) :: xm1 (:,:,:,:)
            integer(int64), intent(in) :: x2(:)
            integer(int64), intent(out) :: xm2 (:,:,:,:)
            integer(int64), intent(in) :: x3(:)
            integer(int64), intent(out) :: xm3 (:,:,:,:)
            integer(int64), intent(in) :: x4(:)
            integer(int64), intent(out) :: xm4 (:,:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_4_iint64_iint64
        module subroutine meshgrid_1_rsp_rsp(&
                x1,  &
                xm1,  &
                indexing &
        )
            real(sp), intent(in) :: x1(:)
            real(sp), intent(out) :: xm1 (:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_1_rsp_rsp
        module subroutine meshgrid_2_rsp_rsp(&
                x1, x2,  &
                xm1, xm2,  &
                indexing &
        )
            real(sp), intent(in) :: x1(:)
            real(sp), intent(out) :: xm1 (:,:)
            real(sp), intent(in) :: x2(:)
            real(sp), intent(out) :: xm2 (:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_2_rsp_rsp
        module subroutine meshgrid_3_rsp_rsp(&
                x1, x2, x3,  &
                xm1, xm2, xm3,  &
                indexing &
        )
            real(sp), intent(in) :: x1(:)
            real(sp), intent(out) :: xm1 (:,:,:)
            real(sp), intent(in) :: x2(:)
            real(sp), intent(out) :: xm2 (:,:,:)
            real(sp), intent(in) :: x3(:)
            real(sp), intent(out) :: xm3 (:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_3_rsp_rsp
        module subroutine meshgrid_4_rsp_rsp(&
                x1, x2, x3, x4,  &
                xm1, xm2, xm3, xm4,  &
                indexing &
        )
            real(sp), intent(in) :: x1(:)
            real(sp), intent(out) :: xm1 (:,:,:,:)
            real(sp), intent(in) :: x2(:)
            real(sp), intent(out) :: xm2 (:,:,:,:)
            real(sp), intent(in) :: x3(:)
            real(sp), intent(out) :: xm3 (:,:,:,:)
            real(sp), intent(in) :: x4(:)
            real(sp), intent(out) :: xm4 (:,:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_4_rsp_rsp
        module subroutine meshgrid_1_rdp_rdp(&
                x1,  &
                xm1,  &
                indexing &
        )
            real(dp), intent(in) :: x1(:)
            real(dp), intent(out) :: xm1 (:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_1_rdp_rdp
        module subroutine meshgrid_2_rdp_rdp(&
                x1, x2,  &
                xm1, xm2,  &
                indexing &
        )
            real(dp), intent(in) :: x1(:)
            real(dp), intent(out) :: xm1 (:,:)
            real(dp), intent(in) :: x2(:)
            real(dp), intent(out) :: xm2 (:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_2_rdp_rdp
        module subroutine meshgrid_3_rdp_rdp(&
                x1, x2, x3,  &
                xm1, xm2, xm3,  &
                indexing &
        )
            real(dp), intent(in) :: x1(:)
            real(dp), intent(out) :: xm1 (:,:,:)
            real(dp), intent(in) :: x2(:)
            real(dp), intent(out) :: xm2 (:,:,:)
            real(dp), intent(in) :: x3(:)
            real(dp), intent(out) :: xm3 (:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_3_rdp_rdp
        module subroutine meshgrid_4_rdp_rdp(&
                x1, x2, x3, x4,  &
                xm1, xm2, xm3, xm4,  &
                indexing &
        )
            real(dp), intent(in) :: x1(:)
            real(dp), intent(out) :: xm1 (:,:,:,:)
            real(dp), intent(in) :: x2(:)
            real(dp), intent(out) :: xm2 (:,:,:,:)
            real(dp), intent(in) :: x3(:)
            real(dp), intent(out) :: xm3 (:,:,:,:)
            real(dp), intent(in) :: x4(:)
            real(dp), intent(out) :: xm4 (:,:,:,:)
            integer, intent(in), optional :: indexing
        end subroutine meshgrid_4_rdp_rdp
    end interface meshgrid
contains

    elemental function clip_int8(x, xmin, xmax) result(res)
        integer(int8), intent(in) :: x
        integer(int8), intent(in) :: xmin
        integer(int8), intent(in) :: xmax
        integer(int8) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int8

    elemental function clip_int16(x, xmin, xmax) result(res)
        integer(int16), intent(in) :: x
        integer(int16), intent(in) :: xmin
        integer(int16), intent(in) :: xmax
        integer(int16) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int16

    elemental function clip_int32(x, xmin, xmax) result(res)
        integer(int32), intent(in) :: x
        integer(int32), intent(in) :: xmin
        integer(int32), intent(in) :: xmax
        integer(int32) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int32

    elemental function clip_int64(x, xmin, xmax) result(res)
        integer(int64), intent(in) :: x
        integer(int64), intent(in) :: xmin
        integer(int64), intent(in) :: xmax
        integer(int64) :: res

        res = max(min(x, xmax), xmin)
    end function clip_int64

    elemental function clip_sp(x, xmin, xmax) result(res)
        real(sp), intent(in) :: x
        real(sp), intent(in) :: xmin
        real(sp), intent(in) :: xmax
        real(sp) :: res

        res = max(min(x, xmax), xmin)
    end function clip_sp

    elemental function clip_dp(x, xmin, xmax) result(res)
        real(dp), intent(in) :: x
        real(dp), intent(in) :: xmin
        real(dp), intent(in) :: xmax
        real(dp) :: res

        res = max(min(x, xmax), xmin)
    end function clip_dp


    elemental function arg_sp(z) result(result) 
        complex(sp), intent(in) :: z
        real(sp) :: result

        result = merge(0.0_sp, atan2(z%im, z%re), z == (0.0_sp, 0.0_sp))

    end function arg_sp

    elemental function argd_sp(z) result(result) 
        complex(sp), intent(in) :: z
        real(sp) :: result

        result = merge(0.0_sp, atan2(z%im, z%re)*180.0_sp/PI_sp, &
                 z == (0.0_sp, 0.0_sp))

    end function argd_sp

    elemental function argpi_sp(z) result(result) 
        complex(sp), intent(in) :: z
        real(sp) :: result

        result = merge(0.0_sp, atan2(z%im, z%re)/PI_sp, &
                 z == (0.0_sp, 0.0_sp))
                 

    end function argpi_sp
    elemental function arg_dp(z) result(result) 
        complex(dp), intent(in) :: z
        real(dp) :: result

        result = merge(0.0_dp, atan2(z%im, z%re), z == (0.0_dp, 0.0_dp))

    end function arg_dp

    elemental function argd_dp(z) result(result) 
        complex(dp), intent(in) :: z
        real(dp) :: result

        result = merge(0.0_dp, atan2(z%im, z%re)*180.0_dp/PI_dp, &
                 z == (0.0_dp, 0.0_dp))

    end function argd_dp

    elemental function argpi_dp(z) result(result) 
        complex(dp), intent(in) :: z
        real(dp) :: result

        result = merge(0.0_dp, atan2(z%im, z%re)/PI_dp, &
                 z == (0.0_dp, 0.0_dp))
                 

    end function argpi_dp

    elemental function deg2rad_sp(theta) result(result)
        real(sp), intent(in) :: theta
        real(sp) :: result
        result = theta * PI_sp / 180.0_sp
        
    end function deg2rad_sp
    
    elemental function rad2deg_sp(theta) result(result)
        real(sp), intent(in) :: theta
        real(sp) :: result
        result = theta * 180.0_sp / PI_sp
        
    end function rad2deg_sp
    elemental function deg2rad_dp(theta) result(result)
        real(dp), intent(in) :: theta
        real(dp) :: result
        result = theta * PI_dp / 180.0_dp
        
    end function deg2rad_dp
    
    elemental function rad2deg_dp(theta) result(result)
        real(dp), intent(in) :: theta
        real(dp) :: result
        result = theta * 180.0_dp / PI_dp
        
    end function rad2deg_dp

    !> Returns the greatest common divisor of two integers of kind int8
    !> using the Euclidean algorithm.
    elemental function gcd_int8(a, b) result(res)
        integer(int8), intent(in) :: a
        integer(int8), intent(in) :: b
        integer(int8) :: res

        integer(int8) :: rem, tmp

        rem = min(abs(a), abs(b))
        res = max(abs(a), abs(b))
        do while (rem /= 0_int8)
          tmp = rem
          rem = mod(res, rem)
          res = tmp
        end do
    end function gcd_int8

    !> Returns the greatest common divisor of two integers of kind int16
    !> using the Euclidean algorithm.
    elemental function gcd_int16(a, b) result(res)
        integer(int16), intent(in) :: a
        integer(int16), intent(in) :: b
        integer(int16) :: res

        integer(int16) :: rem, tmp

        rem = min(abs(a), abs(b))
        res = max(abs(a), abs(b))
        do while (rem /= 0_int16)
          tmp = rem
          rem = mod(res, rem)
          res = tmp
        end do
    end function gcd_int16

    !> Returns the greatest common divisor of two integers of kind int32
    !> using the Euclidean algorithm.
    elemental function gcd_int32(a, b) result(res)
        integer(int32), intent(in) :: a
        integer(int32), intent(in) :: b
        integer(int32) :: res

        integer(int32) :: rem, tmp

        rem = min(abs(a), abs(b))
        res = max(abs(a), abs(b))
        do while (rem /= 0_int32)
          tmp = rem
          rem = mod(res, rem)
          res = tmp
        end do
    end function gcd_int32

    !> Returns the greatest common divisor of two integers of kind int64
    !> using the Euclidean algorithm.
    elemental function gcd_int64(a, b) result(res)
        integer(int64), intent(in) :: a
        integer(int64), intent(in) :: b
        integer(int64) :: res

        integer(int64) :: rem, tmp

        rem = min(abs(a), abs(b))
        res = max(abs(a), abs(b))
        do while (rem /= 0_int64)
          tmp = rem
          rem = mod(res, rem)
          res = tmp
        end do
    end function gcd_int64


    elemental subroutine swap_int8(lhs, rhs)
        integer(int8), intent(inout) :: lhs, rhs
        integer(int8) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_int16(lhs, rhs)
        integer(int16), intent(inout) :: lhs, rhs
        integer(int16) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_int32(lhs, rhs)
        integer(int32), intent(inout) :: lhs, rhs
        integer(int32) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_int64(lhs, rhs)
        integer(int64), intent(inout) :: lhs, rhs
        integer(int64) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_sp(lhs, rhs)
        real(sp), intent(inout) :: lhs, rhs
        real(sp) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_dp(lhs, rhs)
        real(dp), intent(inout) :: lhs, rhs
        real(dp) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_bitset_64(lhs, rhs)
        type(bitset_64), intent(inout) :: lhs, rhs
        type(bitset_64) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_bitset_large(lhs, rhs)
        type(bitset_large), intent(inout) :: lhs, rhs
        type(bitset_large) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine


    elemental subroutine swap_csp(lhs, rhs)
        complex(sp), intent(inout) :: lhs, rhs
        complex(sp) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_cdp(lhs, rhs)
        complex(dp), intent(inout) :: lhs, rhs
        complex(dp) :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine


    elemental subroutine swap_bool(lhs, rhs)
        logical, intent(inout) :: lhs, rhs
        logical :: temp
        temp = lhs; lhs = rhs; rhs = temp
    end subroutine

    elemental subroutine swap_str(lhs,rhs)
        character(*), intent(inout) :: lhs, rhs
        character(len=max(len(lhs), len(rhs))) :: temp
        temp = lhs ; lhs = rhs ; rhs = temp
    end subroutine

    elemental subroutine swap_stt(lhs,rhs)
        use stdlib_string_type, only: string_type
        type(string_type), intent(inout) :: lhs, rhs
        type(string_type) :: temp
        temp = lhs ; lhs = rhs ; rhs = temp
    end subroutine
    
end module stdlib_math
