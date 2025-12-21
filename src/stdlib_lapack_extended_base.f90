module stdlib_lapack_extended_base
    use stdlib_linalg_constants
    implicit none

    interface glagtm
        pure module subroutine stdlib_glagtm_sp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp), intent(in) :: ldb, ldx, n, nrhs
            real(sp), intent(in) :: alpha, beta
            real(sp), intent(inout) :: b(ldb,*)
            real(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_glagtm_sp
        pure module subroutine stdlib_glagtm_dp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp), intent(in) :: ldb, ldx, n, nrhs
            real(dp), intent(in) :: alpha, beta
            real(dp), intent(inout) :: b(ldb,*)
            real(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_glagtm_dp
        pure module subroutine stdlib_glagtm_csp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp), intent(in) :: ldb, ldx, n, nrhs
            complex(sp), intent(in) :: alpha, beta
            complex(sp), intent(inout) :: b(ldb,*)
            complex(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_glagtm_csp
        pure module subroutine stdlib_glagtm_cdp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp), intent(in) :: ldb, ldx, n, nrhs
            complex(dp), intent(in) :: alpha, beta
            complex(dp), intent(inout) :: b(ldb,*)
            complex(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_glagtm_cdp
        pure module subroutine stdlib_I64_glagtm_sp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
            real(sp), intent(in) :: alpha, beta
            real(sp), intent(inout) :: b(ldb,*)
            real(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_I64_glagtm_sp
        pure module subroutine stdlib_I64_glagtm_dp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
            real(dp), intent(in) :: alpha, beta
            real(dp), intent(inout) :: b(ldb,*)
            real(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_I64_glagtm_dp
        pure module subroutine stdlib_I64_glagtm_csp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
            complex(sp), intent(in) :: alpha, beta
            complex(sp), intent(inout) :: b(ldb,*)
            complex(sp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_I64_glagtm_csp
        pure module subroutine stdlib_I64_glagtm_cdp(trans, n, nrhs, alpha, dl, d, du, x, ldx, beta, b, ldb)
            character, intent(in) :: trans
            integer(ilp64), intent(in) :: ldb, ldx, n, nrhs
            complex(dp), intent(in) :: alpha, beta
            complex(dp), intent(inout) :: b(ldb,*)
            complex(dp), intent(in) :: d(*), dl(*), du(*), x(ldx,*)
        end subroutine stdlib_I64_glagtm_cdp
    end interface
end module