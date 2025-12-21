
submodule(stdlib_lapack_extended_base) stdlib_lapack_extended
    implicit none
contains
    pure module subroutine stdlib_glagtm_sp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
        character, intent(in) :: trans
        integer(ilp), intent(in) :: ldb, ldx, n, nrhs
        real(sp), intent(in) :: alpha, beta
        real(sp), intent(inout) :: b(ldb,*)
        real(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)

        ! Internal variables.
        integer(ilp) :: i, j
        real(sp) :: temp
        if(n == 0) then
            return
        endif
        if(beta == 0.0_sp) then
            b(1:n, 1:nrhs) = 0.0_sp
        else
            b(1:n, 1:nrhs) = beta * b(1:n, 1:nrhs)
        end if

        if(trans == 'N') then
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + du(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = dl(i - 1) * x(i - 1, j) + d(i) * x(i, j) + du(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = dl(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        else
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + dl(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = du(i - 1) * x(i - 1, j) + d(i) * x(i, j) + dl(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = du(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        end if
    end subroutine stdlib_glagtm_sp
    pure module subroutine stdlib_glagtm_dp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
        character, intent(in) :: trans
        integer(ilp), intent(in) :: ldb, ldx, n, nrhs
        real(dp), intent(in) :: alpha, beta
        real(dp), intent(inout) :: b(ldb,*)
        real(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)

        ! Internal variables.
        integer(ilp) :: i, j
        real(dp) :: temp
        if(n == 0) then
            return
        endif
        if(beta == 0.0_dp) then
            b(1:n, 1:nrhs) = 0.0_dp
        else
            b(1:n, 1:nrhs) = beta * b(1:n, 1:nrhs)
        end if

        if(trans == 'N') then
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + du(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = dl(i - 1) * x(i - 1, j) + d(i) * x(i, j) + du(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = dl(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        else
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + dl(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = du(i - 1) * x(i - 1, j) + d(i) * x(i, j) + dl(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = du(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        end if
    end subroutine stdlib_glagtm_dp
    pure module subroutine stdlib_glagtm_csp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
        character, intent(in) :: trans
        integer(ilp), intent(in) :: ldb, ldx, n, nrhs
        complex(sp), intent(in) :: alpha, beta
        complex(sp), intent(inout) :: b(ldb,*)
        complex(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)

        ! Internal variables.
        integer(ilp) :: i, j
        complex(sp) :: temp
        if(n == 0) then
            return
        endif
        if(beta == 0.0_sp) then
            b(1:n, 1:nrhs) = 0.0_sp
        else
            b(1:n, 1:nrhs) = beta * b(1:n, 1:nrhs)
        end if

        if(trans == 'N') then
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + du(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = dl(i - 1) * x(i - 1, j) + d(i) * x(i, j) + du(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = dl(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        else if(trans == 'C') then
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = conjg(d(1_ilp)) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = conjg(d(1_ilp)) * x(1_ilp, j) + conjg(dl(1_ilp)) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = conjg(du(i - 1)) * x(i - 1, j) + conjg(d(i)) * x(i, j) + conjg(dl(i)) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = conjg(du(n - 1)) * x(n - 1, j) + conjg(d(n)) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        else
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + dl(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = du(i - 1) * x(i - 1, j) + d(i) * x(i, j) + dl(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = du(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        end if
    end subroutine stdlib_glagtm_csp
    pure module subroutine stdlib_glagtm_cdp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
        character, intent(in) :: trans
        integer(ilp), intent(in) :: ldb, ldx, n, nrhs
        complex(dp), intent(in) :: alpha, beta
        complex(dp), intent(inout) :: b(ldb,*)
        complex(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)

        ! Internal variables.
        integer(ilp) :: i, j
        complex(dp) :: temp
        if(n == 0) then
            return
        endif
        if(beta == 0.0_dp) then
            b(1:n, 1:nrhs) = 0.0_dp
        else
            b(1:n, 1:nrhs) = beta * b(1:n, 1:nrhs)
        end if

        if(trans == 'N') then
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + du(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = dl(i - 1) * x(i - 1, j) + d(i) * x(i, j) + du(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = dl(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        else if(trans == 'C') then
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = conjg(d(1_ilp)) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = conjg(d(1_ilp)) * x(1_ilp, j) + conjg(dl(1_ilp)) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = conjg(du(i - 1)) * x(i - 1, j) + conjg(d(i)) * x(i, j) + conjg(dl(i)) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = conjg(du(n - 1)) * x(n - 1, j) + conjg(d(n)) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        else
            do j = 1, nrhs
                if(n == 1_ilp) then
                    temp = d(1_ilp) * x(1_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                else
                    temp = d(1_ilp) * x(1_ilp, j) + dl(1_ilp) * x(2_ilp, j)
                    b(1_ilp, j) = b(1_ilp, j) + alpha * temp
                    do i = 2, n - 1
                        temp = du(i - 1) * x(i - 1, j) + d(i) * x(i, j) + dl(i) * x(i + 1, j)
                        b(i, j) = b(i, j) + alpha * temp
                    end do
                    temp = du(n - 1) * x(n - 1, j) + d(n) * x(n, j)
                    b(n, j) = b(n, j) + alpha * temp
                end if
            end do
        end if
    end subroutine stdlib_glagtm_cdp

end submodule