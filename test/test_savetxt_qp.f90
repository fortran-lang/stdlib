program test_savetxt_qp
use stdlib_kinds, only: qp
use stdlib_io, only: loadtxt, savetxt
use stdlib_error, only: check
implicit none

character(:), allocatable :: outpath

outpath = get_outpath() // "/tmp_qp.dat"

call test_rqp(outpath)
call test_cqp(outpath)

contains

    function get_outpath() result(outpath)
    integer :: ierr
    character(256) :: argv
    character(:), allocatable :: outpath

    call get_command_argument(1, argv, status=ierr)
    if (ierr==0) then
        outpath = trim(argv)
    else
        outpath = '.'
    endif
    end function get_outpath

    subroutine test_rqp(outpath)
    character(*), intent(in) :: outpath
    real(qp) :: d(3, 2), e(2, 3)
    real(qp), allocatable :: d2(:, :)
    d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    call savetxt(outpath, d)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [3, 2]))
    call check(all(abs(d-d2) < epsilon(1._qp)))

    e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call savetxt(outpath, e)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [2, 3]))
    call check(all(abs(e-d2) < epsilon(1._qp)))
    end subroutine test_rqp

    subroutine test_cqp(outpath)
    character(*), intent(in) :: outpath
    complex(qp) :: d(3, 2), e(2, 3)
    complex(qp), allocatable :: d2(:, :)
    d = reshape([1, 2, 3, 4, 5, 6], [3, 2])
    call savetxt(outpath, d)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [3, 2]))
    call check(all(abs(d-d2) < epsilon(1._qp)))

    e = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    call savetxt(outpath, e)
    call loadtxt(outpath, d2)
    call check(all(shape(d2) == [2, 3]))
    call check(all(abs(e-d2) < epsilon(1._qp)))
    end subroutine test_cqp

end program test_savetxt_qp
