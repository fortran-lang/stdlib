program demo_loadnpy
  use stdlib_io_npy, only: load_npy
  implicit none
  real, allocatable :: x(:, :)
  call load_npy('example.npy', x)
end program demo_loadnpy
