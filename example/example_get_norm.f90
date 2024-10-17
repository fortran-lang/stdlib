! Vector norm: demonstrate usage of the function interface
program example_get_norm
  use stdlib_linalg, only: get_norm, linalg_state_type
  implicit none
  
  real :: a(3,3), nrm, nrmd(3)
  integer :: j
  type(linalg_state_type) :: err
    
  ! a = [   -3.00000000   0.00000000   3.00000000
  !         -2.00000000   1.00000000   4.00000000
  !         -1.00000000   2.00000000   5.00000000   ]  
  a = reshape([(j-4,j=1,9)], [3,3])
  
  print "(' a = [   ',3(g0,3x),2(/9x,3(g0,3x)),']')", transpose(a)
    
  ! Norm with integer input
  call get_norm(a, nrm, 2)
  print *, 'Euclidean norm      = ',nrm   ! 8.30662346
  
  ! Norm with character input
  call get_norm(a, nrm, '2')
  print *, 'Euclidean norm      = ',nrm   ! 8.30662346
  
  ! Euclidean norm of row arrays, a(i,:)
  call get_norm(a, nrmd, 2, dim=2)
  print *, 'Rows norms          = ',nrmd  ! 4.24264050       4.58257580       5.47722578  
  
  ! Euclidean norm of columns arrays, a(:,i)
  call get_norm(a, nrmd, 2, dim=1)
  print *, 'Columns norms       = ',nrmd  ! 3.74165750       2.23606801       7.07106781 
  
  ! Infinity norms
  call get_norm(a, nrm, 'inf')
  print *, 'maxval(||a||)       = ',nrm   ! 5.00000000
  
  call get_norm(a, nrmd, 'inf', dim=2)
  print *, 'maxval(||a(i,:)||)  = ',nrmd  ! 3.00000000       4.00000000       5.00000000
  
  call get_norm(a, nrm, '-inf')
  print *, 'minval(||a||)       = ',nrm   ! 0.00000000
  
  call get_norm(a, nrmd, '-inf', dim=1)
  print *, 'minval(||a(:,i)||)  = ',nrmd  ! 1.00000000       0.00000000       3.00000000 
  
  ! Catch Error: 
  ! [norm] returned Value Error: dimension 4 is out of rank for shape(a)= [3, 3]
  call get_norm(a, nrmd, 'inf', dim=4, err=err)
  print *, 'invalid: ',err%print()  

end program example_get_norm
