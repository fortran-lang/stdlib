program example_kabsch_umeyama
    use stdlib_linalg_constants, only: dp
    use stdlib_spatial, only: kabsch_umeyama
    implicit none

    integer, parameter :: d = 2, N = 7
    real(dp) :: P(d,N), Q(d,N), R(d, d), t(d), c, rmsd

    integer :: i

    ! 2x7 matrices.
    P(1,:) = [23.0_dp,  66.0_dp,  88.0_dp, 119.0_dp, 122.0_dp, 170.0_dp, 179.0_dp]
    P(2,:) = [178.0_dp, 173.0_dp, 187.0_dp, 202.0_dp, 229.0_dp, 232.0_dp, 199.0_dp]

    Q(1,:) = [232.0_dp, 208.0_dp, 181.0_dp, 155.0_dp, 142.0_dp, 121.0_dp, 139.0_dp]
    Q(2,:) = [ 38.0_dp,  32.0_dp,  31.0_dp,  45.0_dp,  33.0_dp,  59.0_dp,  69.0_dp]

    call kabsch_umeyama(P, Q, R, t, c, rmsd)

    print *
    print *, "Recovered rotation R:"
    do i = 1, d
        print *, R(i,:)
    end do

    print *
    print *, "Recovered scale c:", c

    print *
    print *, "Recovered translation t:"
    print *, t

    print *
    print *, "RMSD:", rmsd

    print *
    print *, "Recovered P:"
    do i = 1, N
        print*, c*matmul(R, Q(:,i)) + t
    end do
    
end program example_kabsch_umeyama