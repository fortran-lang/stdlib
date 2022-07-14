program example_diff

  use stdlib_math, only: diff
  implicit none

  integer :: i(7) = [1, 1, 2, 3, 5, 8, 13]
  real    :: x(6) = [0, 5, 15, 30, 50, 75]
  integer :: A(3, 3) = reshape([1, 7, 17, 3, 11, 19, 5, 13, 23], [3, 3])
  integer :: Y(3, 2)

  print *, diff(i)        ! [0, 1, 1, 2, 3, 5]
  print *, diff(x, 2)     ! [5.0, 5.0, 5.0, 5.0]

  Y = diff(A, n=1, dim=2)
  print *, Y(1, :)        ! [2, 2]
  print *, Y(2, :)        ! [4, 2]
  print *, Y(3, :)        ! [2, 4]

  print *, diff(i, prepend=[0]) ! [1, 0, 1, 1, 2, 3, 5]
  print *, diff(i, append=[21]) ! [0, 1, 1, 2, 3, 5, 8]

end program example_diff
