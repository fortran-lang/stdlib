!> Version: experimental
!>
!> The specification of this module is available [here](../page/specs/stdlib_kinds.html).
module stdlib_kinds
  use iso_fortran_env, only: int8, int16, int32, int64
  use iso_c_binding, only: c_bool, c_char
  implicit none
  private
  public :: sp, dp, xdp, qp, int8, int16, int32, int64, lk, c_bool, c_char

  !> Single precision real numbers
  integer, parameter :: sp = selected_real_kind(6)

  !> Double precision real numbers
  integer, parameter :: dp = selected_real_kind(15)

  !> Extended double precision real numbers
  integer, parameter :: xdp = -1

  !> Quadruple precision real numbers
  integer, parameter :: qp = -1

  !> Default logical kind parameter
  integer, parameter :: lk = kind(.true.)

end module stdlib_kinds
