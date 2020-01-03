module default_m
  !!
  !! Provides a generic function `default`, which can be used to
  !! conveniently implement fallback values for optional arguments
  !! to subprograms. If `x` is an `optional` parameter of a
  !! subprogram, then the expression `default(x, y)` inside that
  !! subprogram evaluates to `x` if it is present, otherwise `y`.
  !!
  !! It is an error to call `default` with a single actual argument.
  !!
  !! For additional clarity, `default` be called with keyword argument
  !! for the fallback value, e.g., `default(x, to=1.0)`.
  !!
  use iso_fortran_env, only: sp => real32, dp => real64, qp => real128, int16, int32, int64
  implicit none


  private
  public :: default
  

  interface default
     module procedure default_sp
     module procedure default_dp
     module procedure default_qp
     module procedure default_int16
     module procedure default_int32
     module procedure default_int64
     module procedure default_logical
     module procedure default_character
     ! TODO: complex kinds
     ! TODO: differentiate ascii & ucs char kinds
  end interface default


contains

  
  function default_sp(x, to) result(y)
    real(sp), intent(in), optional :: x
    real(sp), intent(in) :: to
    real(sp) :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_sp

  
  function default_dp(x, to) result(y)
    real(dp), intent(in), optional :: x
    real(dp), intent(in) :: to
    real(dp) :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_dp

  
  function default_qp(x, to) result(y)
    real(qp), intent(in), optional :: x
    real(qp), intent(in) :: to
    real(qp) :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_qp

  
  function default_int16(x, to) result(y)
    integer(int16), intent(in), optional :: x
    integer(int16), intent(in) :: to
    integer(int16) :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_int16

  
  function default_int32(x, to) result(y)
    integer(int32), intent(in), optional :: x
    integer(int32), intent(in) :: to
    integer(int32) :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_int32

  
  function default_int64(x, to) result(y)
    integer(int64), intent(in), optional :: x
    integer(int64), intent(in) :: to
    integer(int64) :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_int64

  
  function default_logical(x, to) result(y)
    logical, intent(in), optional :: x
    logical, intent(in) :: to
    logical :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_logical


  function default_character(x, to) result(y)
    character(len=*), intent(in), optional :: x
    character(len=*), intent(in) :: to
    character(len=:), allocatable :: y

    if (present(x)) then
       y = x
    else
       y = to
    end if
  end function default_character  
  
end module stdlib_experimental_default
