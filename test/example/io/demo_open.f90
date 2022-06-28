program demo_open
  use stdlib_io, only: open
  implicit none
  integer :: u
  u = open ('example.dat', 'wt')
  write (u, '(a)') 'This is an example for open'
  close (u)
end program demo_open
