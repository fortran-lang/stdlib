module stdlib_experimental_optval
  !!
  !! Provides a generic function `optval`, which can be used to
  !! conveniently implement fallback values for optional arguments
  !! to subprograms. If `x` is an `optional` parameter of a
  !! subprogram, then the expression `optval(x, default)` inside that
  !! subprogram evaluates to `x` if it is present, otherwise `default`.
  !!
  !! It is an error to call `optval` with a single actual argument.
  !!
  use iso_fortran_env, only: sp => real32, dp => real64, int8, int16, int32, int64
#ifdef REAL128
  use iso_fortran_env, only: qp => real128
#endif


  implicit none


  private
  public :: optval


  interface optval
     module procedure optval_sp
     module procedure optval_dp
#ifdef REAL128
     module procedure optval_qp
#endif
     module procedure optval_int8
     module procedure optval_int16
     module procedure optval_int32
     module procedure optval_int64
     module procedure optval_logical
     module procedure optval_character
     ! TODO: complex kinds
     ! TODO: differentiate ascii & ucs char kinds
  end interface optval


contains


  pure function optval_sp(x, default) result(y)
    real(sp), intent(in), optional :: x
    real(sp), intent(in) :: default
    real(sp) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_sp


  pure function optval_dp(x, default) result(y)
    real(dp), intent(in), optional :: x
    real(dp), intent(in) :: default
    real(dp) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_dp

#ifdef REAL128
  pure function optval_qp(x, default) result(y)
    real(qp), intent(in), optional :: x
    real(qp), intent(in) :: default
    real(qp) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_qp
#endif

  pure function optval_int8(x, default) result(y)
    integer(int8), intent(in), optional :: x
    integer(int8), intent(in) :: default
    integer(int8) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_int8


  pure function optval_int16(x, default) result(y)
    integer(int16), intent(in), optional :: x
    integer(int16), intent(in) :: default
    integer(int16) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_int16


  pure function optval_int32(x, default) result(y)
    integer(int32), intent(in), optional :: x
    integer(int32), intent(in) :: default
    integer(int32) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_int32


  pure function optval_int64(x, default) result(y)
    integer(int64), intent(in), optional :: x
    integer(int64), intent(in) :: default
    integer(int64) :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_int64


  pure function optval_logical(x, default) result(y)
    logical, intent(in), optional :: x
    logical, intent(in) :: default
    logical :: y

    if (present(x)) then
       y = x
    else
       y = default
    end if
  end function optval_logical


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

end module stdlib_experimental_optval
