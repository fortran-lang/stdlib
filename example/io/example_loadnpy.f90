program example_loadnpy
   use stdlib_io_np, only: load_npy
   implicit none
   real, allocatable :: x(:, :)
   call load_npy('example.npy', x)
 end program example_loadnpy