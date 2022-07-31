program example_savenpy
  use stdlib_io_npy, only: save_npy
  implicit none
  real :: x(3, 2) = 1
  call save_npy('example.npy', x)
end program example_savenpy
