program example_clip_integer
  use stdlib_math, only: clip
  use stdlib_kinds, only: int32
  implicit none
  integer(int32) :: x
  integer(int32) :: xmin
  integer(int32) :: xmax
  integer(int32) :: clipped_value

  xmin = -5_int32
  xmax = 5_int32
  x = 12_int32

  clipped_value = clip(x, xmin, xmax)
! clipped_value <- 5
end program example_clip_integer
