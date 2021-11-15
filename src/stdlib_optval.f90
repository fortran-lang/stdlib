

module stdlib_optval
  !!
  !! Provides a generic function `optval`, which can be used to
  !! conveniently implement fallback values for optional arguments
  !! to subprograms
  !! ([Specification](../page/specs/stdlib_optval.html))
  !!
  !! If `x` is an `optional` parameter of a
  !! subprogram, then the expression `optval(x, default)` inside that
  !! subprogram evaluates to `x` if it is present, otherwise `default`.
  !!
  !! It is an error to call `optval` with a single actual argument.
  !!
  use stdlib_kinds, only: sp, dp, xdp, qp, int8, int16, int32, int64
  implicit none


  private
  public :: optval


  interface optval
    !! version: experimental
    !!
    !! Fallback value for optional arguments
    !! ([Specification](../page/specs/stdlib_optval.html#description))
      module procedure optval_rsp
      module procedure optval_rdp
      module procedure optval_iint8
      module procedure optval_iint16
      module procedure optval_iint32
      module procedure optval_iint64
      module procedure optval_csp
      module procedure optval_cdp
      module procedure optval_ll1
    module procedure optval_character
     ! TODO: differentiate ascii & ucs char kinds
  end interface optval


contains

    pure elemental function optval_rsp(x, default) result(y)
    real(sp), intent(in), optional :: x
    real(sp), intent(in) :: default
    real(sp) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_rsp
    pure elemental function optval_rdp(x, default) result(y)
    real(dp), intent(in), optional :: x
    real(dp), intent(in) :: default
    real(dp) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_rdp
    pure elemental function optval_iint8(x, default) result(y)
    integer(int8), intent(in), optional :: x
    integer(int8), intent(in) :: default
    integer(int8) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_iint8
    pure elemental function optval_iint16(x, default) result(y)
    integer(int16), intent(in), optional :: x
    integer(int16), intent(in) :: default
    integer(int16) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_iint16
    pure elemental function optval_iint32(x, default) result(y)
    integer(int32), intent(in), optional :: x
    integer(int32), intent(in) :: default
    integer(int32) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_iint32
    pure elemental function optval_iint64(x, default) result(y)
    integer(int64), intent(in), optional :: x
    integer(int64), intent(in) :: default
    integer(int64) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_iint64
    pure elemental function optval_csp(x, default) result(y)
    complex(sp), intent(in), optional :: x
    complex(sp), intent(in) :: default
    complex(sp) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_csp
    pure elemental function optval_cdp(x, default) result(y)
    complex(dp), intent(in), optional :: x
    complex(dp), intent(in) :: default
    complex(dp) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_cdp
    pure elemental function optval_ll1(x, default) result(y)
    logical, intent(in), optional :: x
    logical, intent(in) :: default
    logical :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_ll1

  ! Cannot be made elemental
  pure function optval_character(x, default) result(y)
    character(len=*), intent(in), optional :: x
    character(len=*), intent(in) :: default
    character(len=:), allocatable :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_character

end module stdlib_optval
