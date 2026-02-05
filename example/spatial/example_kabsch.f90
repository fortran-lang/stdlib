program example_kabsch
    use stdlib_linalg_constants, only: dp
    use stdlib_spatial
    use stdlib_math, only: all_close
    implicit none

    integer, parameter :: d = 3, N = 4
    real(dp) :: P(d, N), Q(d, N), Q_1(d, N)
    real(dp) :: R(d, d), t(d)
    real(dp) :: c, rmsd
    logical :: scale

    integer :: i

    ! Reference point set P (columns are points)
Q(:,1) = [0.0_dp, 0.0_dp, 0.0_dp]
Q(:,2) = [1.0_dp, 0.0_dp, 0.0_dp]
Q(:,3) = [0.0_dp, 1.0_dp, 0.0_dp]
Q(:,4) = [0.0_dp, 0.0_dp, 1.0_dp]

P(:,1) = [2.0_dp, 0.0_dp, 1.0_dp]
P(:,2) = [2.0_dp, 1.0_dp, 1.0_dp]
P(:,3) = [1.0_dp, 0.0_dp, 2.0_dp]
P(:,4) = [3.0_dp, 0.0_dp, 1.0_dp]


    scale = .true.

    call kabsch(P, Q, scale, R, t, c, rmsd)

    print *, ""
    print *, "Matrix P:"
    do i = 1, d
        print "(4F10.5)", P(i,:)
    end do

    print *, ""
    print *, "Matrix Q:"
    do i = 1, d
        print "(4F10.5)", Q(i,:)
    end do

    print *, "Rotation matrix R:"
    do i = 1, d
        print "(3F10.5)", R(i,:)
    end do

    print *, ""
    print *, "Translation vector t:"
    print "(3F10.5)", t

    ! Apply the transformation: Q_1 = c * R * Q + t
    do i = 1, N
        Q_1(:,i) = c * matmul(R, Q(:,i)) + t
    end do

    print *, ""
    print *, "Aligned Q (should match P):"
    do i = 1, d
        print "(4F10.5)", Q_1(i,:)
    end do

    print*, "Check P and RQ + t: ", all_close(P, Q_1, rel_tol=1.0e-6_dp, abs_tol=1.0e-12_dp)

    print *, ""
    print *, "Scale factor c:", c
    print *, "RMSD:", rmsd

end program example_kabsch
