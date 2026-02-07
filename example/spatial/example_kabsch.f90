program example_kabsch
    use stdlib_linalg_constants, only: dp
    use stdlib_spatial, only: kabsch
    implicit none

    integer, parameter :: d = 2, N = 3
    real(dp) :: P(d, N), Q(d, N), R(d, d), t(d), c, rmsd

    integer :: i

    P(:,1) = [3.0_dp, -2.0_dp]
    P(:,2) = [7.0_dp, 4.0_dp]
    P(:,3) = [5.0_dp, 0.0_dp]

    Q(:,1) = [2.0_dp, 3.0_dp]
    Q(:,2) = [-1.0_dp, 5.0_dp]
    Q(:,3) = [1.0_dp, 4.0_dp]

    call kabsch(P, Q, R, t, c, rmsd)

    print *, ""
    print *, "Recovered rotation R:"
    do i = 1, d
        print *, R(i,:)
    end do

    print *, "Recovered scale c:", c

    print *, ""
    print *, "Recovered translation t:"
    print *, t

    print *, ""
    print *, "RMSD:", rmsd

end program example_kabsch