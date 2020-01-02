module stdlib_experimental_kinds
! Instead of iso_fortran_env, we use iso_c_binding, to be compatible with C
!use iso_fortran_env, only: sp=>real32, dp=>real64, qp=>real128
!use iso_fortran_env, only: int32, int64, int128
use iso_c_binding, only: sp=>c_float, dp=>c_double, qp=>c_float128
use iso_c_binding, only: int8=>c_int8_t, int16=>c_int16_t, int32=>c_int32_t, int64=>c_int64_t
implicit none
private
public sp, dp, qp, int32, int64, int128
end module
