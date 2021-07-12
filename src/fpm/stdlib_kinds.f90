module stdlib_kinds
!! version: experimental
use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
use iso_fortran_env, only: int8, int16, int32, int64
use iso_c_binding, only: c_bool
! If we decide later to use iso_c_binding instead of iso_fortran_env:
!use iso_c_binding, only: sp=>c_float, dp=>c_double, qp=>c_float128
!use iso_c_binding, only: int8=>c_int8_t, int16=>c_int16_t, int32=>c_int32_t, int64=>c_int64_t
implicit none
private
public sp, dp, qp, int8, int16, int32, int64, lk, c_bool

integer, parameter :: lk = kind(.true.)
end module stdlib_kinds
