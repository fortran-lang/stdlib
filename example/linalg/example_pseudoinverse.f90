! Matrix pseudo-inversion example: function, subroutine, and operator interfaces
program example_pseudoinverse
  use stdlib_linalg, only: pinv, pseudoinvert, operator(.pinv.), linalg_state_type
  implicit none

  real :: A(15,5), Am1(5,15)
  type(linalg_state_type) :: state
  
  ! Generate random matrix A (15x15)
  call random_number(A)

  ! Pseudo-inverse: Function interfcae
  Am1 = pinv(A, err=state)
  print *, 'Max error (function)  : ', maxval(abs(A-matmul(A, matmul(Am1,A))))
  
  ! User threshold
  Am1 = pinv(A, rtol=0.001, err=state)
  print *, 'Max error (rtol=0.001): ', maxval(abs(A-matmul(A, matmul(Am1,A))))  

  ! Pseudo-inverse: Subroutine interface
  call pseudoinvert(A, Am1, err=state)

  print *, 'Max error (subroutine): ', maxval(abs(A-matmul(A, matmul(Am1,A))))

  ! Operator interface
  Am1 = .pinv.A
  
  print *, 'Max error (operator)  : ', maxval(abs(A-matmul(A, matmul(Am1,A))))

end program example_pseudoinverse
