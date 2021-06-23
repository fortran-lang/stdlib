program test_gauss_
    use stdlib_kinds, only: dp
    use stdlib_error, only: check
    use stdlib_quadrature , only: gauss_legendre, gauss_legendre_lobatto

    implicit none

    call test_gauss
    call test_gauss_lobatto

contains

    subroutine test_gauss
        integer :: i
        real(dp) :: analytic, numeric

        ! x**2 from -1 to 1
        analytic = 2.0_dp/3.0_dp
        do i=2,6
            block
                real(dp), dimension(i) :: x,w
                call gauss_legendre(x,w)
                numeric = sum(x**2 * w)
                !print *, i, numeric
                call check(abs(numeric-analytic) < 2*epsilon(analytic))
            end block
        end do

    end subroutine

    subroutine test_gauss_lobatto
    end subroutine

end program
