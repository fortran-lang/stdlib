program test_io_disp
    use, non_intrinsic :: stdlib_io, only: disp
    implicit none
    real(8) :: r(2, 3)
    complex :: c(2, 3), c_3d(2, 3, 2)
    integer :: i(2, 3)
    logical :: l(2, 3)

    r = 1.; c = 1.; c_3d = 2.; i = 1; l = .true.
    r(1, 1) = -1.e-11
    r(1, 2) = -1.e10
    c(2, 2) = (-1.e10,-1.e10)
    c_3d(1,3,1) = (1000, 0.001)
    c_3d(1,3,2) = (1.e4, 100.)
        call disp('string', 'disp(string):')
        call disp('It is a note.')
        call disp()

        call disp(r, 'disp(r):')
        call disp(c, 'disp(c):')
        call disp(i, 'disp(i):')
        call disp(l, 'disp(l):')

        call disp(c_3d, 3, 'disp(c_3d, 3):')
        call disp(c_3d, 2, 'disp(c_3d, 2):')
end program test_io_disp