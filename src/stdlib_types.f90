module stdlib_types
implicit none
private
public sp, dp, qp

integer, parameter :: sp=kind(0.), &             ! single precision
                      dp=kind(0.d0), &           ! double precision
                      qp=selected_real_kind(32)  ! quadruple precision

end module
