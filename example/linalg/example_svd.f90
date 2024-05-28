! Singular Value Decomposition 
program example_svd
  use stdlib_linalg_constants, only: dp
  use stdlib_linalg, only: svd
  implicit none

  real(dp), allocatable :: A(:,:),s(:),u(:,:),vt(:,:)
  character(*), parameter :: fmt = "(a,*(1x,f12.8))"

  ! We want to find the singular value decomposition of matrix: 
  ! 
  !    A = [ 3  2  2]
  !        [ 2  3 -2]  
  !   
  A = transpose(reshape([ 3, 2, 2, &
                          2, 3,-2], [3,2]))

  ! Prepare arrays
  allocate(s(2),u(2,2),vt(3,3))

  ! Get singular value decomposition
  call svd(A,s,u,vt)

  ! Singular values: [5, 3]
  print fmt, '    '
  print fmt, 'S = ',s
  print fmt, '    '
  
  ! Left vectors (may be flipped): 
  !     [Ã2/2  Ã2/2]
  ! U = [Ã2/2 -Ã2/2]
  !
  print fmt, '    '
  print fmt, 'U = ',u(1,:)
  print fmt, '    ',u(2,:)
  

  ! Right vectors (may be flipped): 
  !     [Ã2/2    Ã2/2      0]
  ! V = [1/Ã18 -1/Ã18  4/Ã18]
  !     [ 2/3    -2/3   -1/3]
  !
  print fmt, '    '
  print fmt, '    ',vt(1,:)
  print fmt, 'VT= ',vt(2,:)
  print fmt, '    ',vt(3,:)
  print fmt, '    '
  

end program example_svd
