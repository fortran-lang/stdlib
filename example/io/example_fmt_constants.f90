program example_fmt_constants
  use stdlib_kinds, only: int32, int64, sp, dp
  use stdlib_io, only: FMT_INT, FMT_REAL_SP, FMT_REAL_DP, FMT_COMPLEX_SP, FMT_COMPLEX_DP
  implicit none

  integer(kind=int32) :: i32
  integer(kind=int64) :: i64
  real(kind=sp)       :: r32
  real(kind=dp)       :: r64
  complex(kind=sp)    :: c32
  complex(kind=dp)    :: c64

  i32 = 100_int32
  i64 = 100_int64
  r32 = 100.0_sp
  r64 = 100.0_dp
  c32 = cmplx(100.0_sp, kind=sp)
  c64 = cmplx(100.0_dp, kind=dp)

  print "(2("//FMT_INT//",1x))", i32, i64 ! outputs: 100 100
  print FMT_REAL_SP, r32                  ! outputs: 1.00000000E+02
  print FMT_REAL_DP, r64                  ! outputs: 1.0000000000000000E+002
  print FMT_COMPLEX_SP, c32               ! outputs: 1.00000000E+02  0.00000000E+00
  print FMT_COMPLEX_DP, c64               ! outputs: 1.0000000000000000E+002  0.0000000000000000E+000

end program example_fmt_constants
