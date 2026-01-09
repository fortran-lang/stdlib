module stdlib_io_aux
  implicit none(type, external)
  private
  public :: FMT_INT, FMT_REAL_SP, FMT_REAL_DP, FMT_REAL_XDP, FMT_REAL_QP
  public :: FMT_COMPLEX_SP, FMT_COMPLEX_DP, FMT_COMPLEX_XDP, FMT_COMPLEX_QP

  !> Version: experimental
  !>
  !> Format strings with edit descriptors for each type and kind
  !> ([Specification](../page/specs/stdlib_io.html))
  character(*), parameter :: &
    !> Format string for integers
    FMT_INT = '(i0)', &
    !> Format string for single precision real numbers
    FMT_REAL_SP = '(es15.8e2)', &
    !> Format string for souble precision real numbers
    FMT_REAL_DP = '(es24.16e3)', &
    !> Format string for extended double precision real numbers
    FMT_REAL_XDP = '(es26.18e3)', &
    !> Format string for quadruple precision real numbers
    FMT_REAL_QP = '(es44.35e4)', &
    !> Format string for single precision complex numbers
    FMT_COMPLEX_SP = '(es15.08e2,1x,es15.08e2)', &
    !> Format string for double precision complex numbers
    FMT_COMPLEX_DP = '(es24.16e3,1x,es24.16e3)', &
    !> Format string for extended double precision complex numbers
    FMT_COMPLEX_XDP = '(es26.18e3,1x,es26.18e3)', &
    !> Format string for quadruple precision complex numbers
    FMT_COMPLEX_QP = '(es44.35e4,1x,es44.35e4)'

end module stdlib_io_aux
