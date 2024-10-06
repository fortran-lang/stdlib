! Vector norm: demonstrate usage of the function interface
program example_norm
  use stdlib_linalg, only: norm, linalg_state_type
  implicit none
  
  real :: a(3,3),na
  integer :: j
  type(linalg_state_type) :: err
    
  ! a = [   -3.00000000   0.00000000   3.00000000
  !         -2.00000000   1.00000000   4.00000000
  !         -1.00000000   2.00000000   5.00000000   ]  
  a = reshape([(j-4,j=1,9)], [3,3])
  
  print "(' a = [   ',3(g0,3x),2(/9x,3(g0,3x)),']')", transpose(a)
    
  ! Norm with integer input
  print *, 'Euclidean norm      = ',norm(a, 2)        ! 8.30662346
  
  ! Norm with character input
  print *, 'Euclidean norm      = ',norm(a, '2')      ! 8.30662346
  
  ! Euclidean norm of row arrays, a(i,:)
  print *, 'Rows norms          = ',norm(a, 2, dim=2) ! 4.24264050       4.58257580       5.47722578  
  
  ! Euclidean norm of columns arrays, a(:,i)
  print *, 'Columns norms       = ',norm(a, 2, dim=1) ! 3.74165750       2.23606801       7.07106781 
  
  ! Infinity norms
  print *, 'maxval(||a||)       = ',norm(a, 'inf')          ! 5.00000000
  print *, 'maxval(||a(i,:)||)  = ',norm(a, 'inf', dim=2)   ! 3.00000000       4.00000000       5.00000000
  print *, 'minval(||a||)       = ',norm(a, '-inf')         ! 0.00000000
  print *, 'minval(||a(:,i)||)  = ',norm(a, '-inf', dim=1)  ! 1.00000000       0.00000000       3.00000000 
  
  ! Catch Error: 
  ! [norm] returned Value Error: dimension 4 is out of rank for shape(a)= [3, 3]
  print *, 'invalid: ',norm(a,'inf', dim=4, err=err)
  print *, 'error =  ',err%print()  

end program example_norm
