program example_math_arange
  use stdlib_math, only: arange
  implicit none

  print *, arange(3)                 ! [1,2,3]
  print *, arange(-1)                ! [1,0,-1]
  print *, arange(0, 2)               ! [0,1,2]
  print *, arange(1, -1)              ! [1,0,-1]
  print *, arange(0, 2, 2)           ! [0,2]

  print *, arange(3.0)               ! [1.0,2.0,3.0]
  print *, arange(0.0, 5.0)           ! [0.0,1.0,2.0,3.0,4.0,5.0]
  print *, arange(0.0, 6.0, 2.5)       ! [0.0,2.5,5.0]

  print *, (1.0, 1.0)*arange(3)       ! [(1.0,1.0),(2.0,2.0),[3.0,3.0]]

  print *, arange(0.0, 2.0, -2.0)      ! [0.0,2.0].     Not recommended: `step` argument is negative!
  print *, arange(0.0, 2.0, 0.0)       ! [0.0,1.0,2.0]. Not recommended: `step` argument is zero!

end program example_math_arange
