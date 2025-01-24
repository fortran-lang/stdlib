module stdlib_lapack_others
  use stdlib_linalg_constants
  use stdlib_linalg_lapack_aux
  use stdlib_linalg_blas
  use stdlib_lapack_solve
  implicit none

interface 
     real(sp) module function stdlib_sla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_sla_syrpvgrw

     real(dp) module function stdlib_dla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dla_syrpvgrw


     real(sp) module function stdlib_cla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
           integer(ilp), intent(in) :: ipiv(*)
     end function stdlib_cla_syrpvgrw

     real(dp) module function stdlib_zla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, info, lda, ldaf
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
           integer(ilp), intent(in) :: ipiv(*)
     end function stdlib_zla_syrpvgrw


     real(sp) module function stdlib_I64_sla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           integer(ilp64), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
     end function stdlib_I64_sla_syrpvgrw

     real(dp) module function stdlib_I64_dla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           integer(ilp64), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
     end function stdlib_I64_dla_syrpvgrw


     real(sp) module function stdlib_I64_cla_syrpvgrw( uplo, n, info, a, lda, af, ldaf, ipiv,work )
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           real(sp), intent(out) :: work(*)
           integer(ilp64), intent(in) :: ipiv(*)
     end function stdlib_I64_cla_syrpvgrw

     real(dp) module function stdlib_I64_zla_syrpvgrw( uplo, n, info, a, lda, af,ldaf, ipiv, work )
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, info, lda, ldaf
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           real(dp), intent(out) :: work(*)
           integer(ilp64), intent(in) :: ipiv(*)
     end function stdlib_I64_zla_syrpvgrw


end interface 


interface 
     pure real(sp) module function stdlib_sla_gerpvgrw( n, ncols, a, lda, af, ldaf )
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_sla_gerpvgrw

     pure real(dp) module function stdlib_dla_gerpvgrw( n, ncols, a, lda, af,ldaf )
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_dla_gerpvgrw


     pure real(sp) module function stdlib_cla_gerpvgrw( n, ncols, a, lda, af, ldaf )
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_cla_gerpvgrw

     pure real(dp) module function stdlib_zla_gerpvgrw( n, ncols, a, lda, af,ldaf )
           integer(ilp), intent(in) :: n, ncols, lda, ldaf
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_zla_gerpvgrw


     pure real(sp) module function stdlib_I64_sla_gerpvgrw( n, ncols, a, lda, af, ldaf )
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           real(sp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_I64_sla_gerpvgrw

     pure real(dp) module function stdlib_I64_dla_gerpvgrw( n, ncols, a, lda, af,ldaf )
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           real(dp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_I64_dla_gerpvgrw


     pure real(sp) module function stdlib_I64_cla_gerpvgrw( n, ncols, a, lda, af, ldaf )
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_I64_cla_gerpvgrw

     pure real(dp) module function stdlib_I64_zla_gerpvgrw( n, ncols, a, lda, af,ldaf )
           integer(ilp64), intent(in) :: n, ncols, lda, ldaf
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
     end function stdlib_I64_zla_gerpvgrw


end interface 


interface 
     real(sp) module function stdlib_cla_gbrcond_c( trans, n, kl, ku, ab, ldab, afb,ldafb, ipiv, c, &
               capply, info, work,rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp) :: kd, ke
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_cla_gbrcond_c

     real(dp) module function stdlib_zla_gbrcond_c( trans, n, kl, ku, ab,ldab, afb, ldafb, ipiv,c, &
               capply, info, work,rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp) :: kd, ke
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_zla_gbrcond_c


     real(sp) module function stdlib_I64_cla_gbrcond_c( trans, n, kl, ku, ab, ldab, afb,ldafb, ipiv, c, &
               capply, info, work,rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp64) :: kd, ke
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_I64_cla_gbrcond_c

     real(dp) module function stdlib_I64_zla_gbrcond_c( trans, n, kl, ku, ab,ldab, afb, ldafb, ipiv,c, &
               capply, info, work,rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, kl, ku, ldab, ldafb
           integer(ilp64) :: kd, ke
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: ab(ldab,*), afb(ldafb,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_I64_zla_gbrcond_c


end interface 


interface 
     real(sp) module function stdlib_cla_gercond_c( trans, n, a, lda, af, ldaf, ipiv, c,capply, info, &
               work, rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_cla_gercond_c

     real(dp) module function stdlib_zla_gercond_c( trans, n, a, lda, af,ldaf, ipiv, c, capply,info, &
               work, rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_zla_gercond_c


     real(sp) module function stdlib_I64_cla_gercond_c( trans, n, a, lda, af, ldaf, ipiv, c,capply, info, &
               work, rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_I64_cla_gercond_c

     real(dp) module function stdlib_I64_zla_gercond_c( trans, n, a, lda, af,ldaf, ipiv, c, capply,info, &
               work, rwork )
           character, intent(in) :: trans
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_I64_zla_gercond_c


end interface 


interface 
     real(sp) module function stdlib_cla_hercond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
               work, rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_cla_hercond_c

     real(dp) module function stdlib_zla_hercond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
                rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_zla_hercond_c


     real(sp) module function stdlib_I64_cla_hercond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
               work, rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_I64_cla_hercond_c

     real(dp) module function stdlib_I64_zla_hercond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
                rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_I64_zla_hercond_c


end interface 


interface 
     module subroutine stdlib_sla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_sla_syamv

     module subroutine stdlib_dla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n, uplo
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_dla_syamv


     module subroutine stdlib_cla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           integer(ilp), intent(in) :: uplo
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_cla_syamv

     module subroutine stdlib_zla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp), intent(in) :: incx, incy, lda, n
           integer(ilp), intent(in) :: uplo
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_zla_syamv


     module subroutine stdlib_I64_sla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n, uplo
           real(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_I64_sla_syamv

     module subroutine stdlib_I64_dla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n, uplo
           real(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_I64_dla_syamv


     module subroutine stdlib_I64_cla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(sp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n
           integer(ilp64), intent(in) :: uplo
           complex(sp), intent(in) :: a(lda,*), x(*)
           real(sp), intent(inout) :: y(*)
     end subroutine stdlib_I64_cla_syamv

     module subroutine stdlib_I64_zla_syamv( uplo, n, alpha, a, lda, x, incx, beta, y,incy )
           real(dp), intent(in) :: alpha, beta
           integer(ilp64), intent(in) :: incx, incy, lda, n
           integer(ilp64), intent(in) :: uplo
           complex(dp), intent(in) :: a(lda,*), x(*)
           real(dp), intent(inout) :: y(*)
     end subroutine stdlib_I64_zla_syamv


end interface 


interface 
     real(sp) module function stdlib_sla_syrcond( uplo, n, a, lda, af, ldaf, ipiv, cmode,c, info, work, &
               iwork )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(sp), intent(out) :: work(*)
     end function stdlib_sla_syrcond

     real(dp) module function stdlib_dla_syrcond( uplo, n, a, lda, af, ldaf,ipiv, cmode, c, info, work,&
               iwork )
           character, intent(in) :: uplo
           integer(ilp), intent(in) :: n, lda, ldaf, cmode
           integer(ilp), intent(out) :: info
           integer(ilp), intent(out) :: iwork(*)
           integer(ilp), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(dp), intent(out) :: work(*)
     end function stdlib_dla_syrcond


     real(sp) module function stdlib_I64_sla_syrcond( uplo, n, a, lda, af, ldaf, ipiv, cmode,c, info, work, &
               iwork )
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, lda, ldaf, cmode
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(out) :: iwork(*)
           integer(ilp64), intent(in) :: ipiv(*)
           real(sp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(sp), intent(out) :: work(*)
     end function stdlib_I64_sla_syrcond

     real(dp) module function stdlib_I64_dla_syrcond( uplo, n, a, lda, af, ldaf,ipiv, cmode, c, info, work,&
               iwork )
           character, intent(in) :: uplo
           integer(ilp64), intent(in) :: n, lda, ldaf, cmode
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(out) :: iwork(*)
           integer(ilp64), intent(in) :: ipiv(*)
           real(dp), intent(in) :: a(lda,*), af(ldaf,*), c(*)
           real(dp), intent(out) :: work(*)
     end function stdlib_I64_dla_syrcond


end interface 


interface 
     real(sp) module function stdlib_cla_syrcond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
               work, rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_cla_syrcond_c

     real(dp) module function stdlib_zla_syrcond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
                rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           integer(ilp), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_zla_syrcond_c


     real(sp) module function stdlib_I64_cla_syrcond_c( uplo, n, a, lda, af, ldaf, ipiv, c,capply, info, &
               work, rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_I64_cla_syrcond_c

     real(dp) module function stdlib_I64_zla_syrcond_c( uplo, n, a, lda, af,ldaf, ipiv, c, capply,info, work,&
                rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           integer(ilp64), intent(in) :: ipiv(*)
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_I64_zla_syrcond_c


end interface 


interface 
     real(sp) module function stdlib_cla_porcond_c( uplo, n, a, lda, af, ldaf, c, capply,info, work, &
               rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_cla_porcond_c

     real(dp) module function stdlib_zla_porcond_c( uplo, n, a, lda, af,ldaf, c, capply, info,work, &
               rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp), intent(in) :: n, lda, ldaf
           integer(ilp), intent(out) :: info
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_zla_porcond_c


     real(sp) module function stdlib_I64_cla_porcond_c( uplo, n, a, lda, af, ldaf, c, capply,info, work, &
               rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           complex(sp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(sp), intent(out) :: work(*)
           real(sp), intent(in) :: c(*)
           real(sp), intent(out) :: rwork(*)
     end function stdlib_I64_cla_porcond_c

     real(dp) module function stdlib_I64_zla_porcond_c( uplo, n, a, lda, af,ldaf, c, capply, info,work, &
               rwork )
           character, intent(in) :: uplo
           logical(lk), intent(in) :: capply
           integer(ilp64), intent(in) :: n, lda, ldaf
           integer(ilp64), intent(out) :: info
           complex(dp), intent(in) :: a(lda,*), af(ldaf,*)
           complex(dp), intent(out) :: work(*)
           real(dp), intent(in) :: c(*)
           real(dp), intent(out) :: rwork(*)
     end function stdlib_I64_zla_porcond_c


end interface 

end module stdlib_lapack_others
