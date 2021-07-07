program test_io_disp
    
    use :: stdlib_io, only: disp
    implicit none
    real(8) :: r(2, 3)
    complex :: c(2, 3), c_3d(2, 100, 20)
    integer :: i(2, 3)
    logical :: l(10, 10)

    r = 1.; c = 1.; c_3d = 2.; i = 1; l = .true.
    r(1, 1) = -1.e-11
    r(1, 2) = -1.e10
    c(2, 2) = (-1.e10,-1.e10)
    c_3d(1,3,1) = (1000, 0.001)
    c_3d(1,3,2) = (1.e4, 100.)
    call disp('string', header='disp(string):')
    call disp('It is a note.')
    call disp()

    call disp(r, header='disp(r):')
    call disp(c, header='disp(c):')
    call disp(i, header='disp(i):')
    call disp(l, header='disp(l):', brief=.true.)

    call disp(c_3d(:,:,3), header='disp(c_3d, 3):', brief=.true.)
    call disp(c_3d(2,:,:), header='disp(c_3d, 2):', brief=.true.)

end program test_io_disp