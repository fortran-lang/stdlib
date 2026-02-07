program example_kabsch_real
    use stdlib_linalg_constants, only: dp
    use stdlib_spatial
    use stdlib_math, only: all_close
    use stdlib_linalg, only: svd, det
    implicit none

    integer, parameter :: d = 3, N = 4
    real(dp) :: P(d, N), Q(d, N), Q_1(d, N)
    real(dp) :: R(d, d), R_original(d, d)
    real(dp) :: t(d), t_original(d)
    real(dp) :: c, c_original
    real(dp) :: rmsd

    integer :: i, j
    real(dp) :: r1

    call random_seed()

    ! ----------------------------
    ! Random reference points Q
    ! ----------------------------
    do j = 1, N
        do i = 1, d
            call random_number(r1)
            Q(i,j) = r1
        end do
    end do

    ! ------------------------------------------------
    ! Random proper rotation matrix R_original
    ! Constructed via SVD: R = U * V^T
    ! ------------------------------------------------
    do i = 1, d
        do j = 1, d
            call random_number(r1)
            R_original(i,j) = r1
        end do
    end do

    block
        real(dp) :: U(d,d), Vt(d,d), S(d)

        call svd(R_original, S, U, Vt)
        R_original = matmul(U, Vt)

        ! Enforce det = +1 (no reflection)
        if (det(R_original) < 0.0_dp) then
            U(:,d) = -U(:,d)
            R_original = matmul(U, Vt)
        end if
    end block
    R_original(:,1) = -R_original(:,1)

    print *, "det(R_original):", det(R_original)

    ! ----------------------------
    ! Random scale and translation
    ! ----------------------------
    call random_number(r1)
    c_original = 0.5_dp + 2.0_dp * r1

    do i = 1, d
        call random_number(r1)
        t_original(i) = r1
    end do

    ! ----------------------------
    ! Construct P = c*R*Q + t
    ! ----------------------------
    do j = 1, N
        P(:,j) = c_original * matmul(R_original, Q(:,j)) + t_original
    end do

    ! ----------------------------
    ! Call Kabsch–Umeyama
    ! ----------------------------
    call kabsch(P, Q, R, t, c, rmsd)

    print *, ""
    print *, "Original rotation R_original:"
    do i = 1, d
        print *, R_original(i,:)
    end do

    print *, ""
    print *, "Recovered rotation R:"
    do i = 1, d
        print *, R(i,:)
    end do

    print *, ""
    print *, "Original translation t_original:"
    print *, t_original

    print *, ""
    print *, "Recovered translation t:"
    print *, t

    ! ----------------------------
    ! Apply recovered transform
    ! ----------------------------
    do j = 1, N
        Q_1(:,j) = c * matmul(R, Q(:,j)) + t
    end do

    print *, ""
    print *, "Check P ≈ c*R*Q + t: ", &
        all_close(P, Q_1, rel_tol=1.0e-10_dp, abs_tol=1.0e-12_dp)

    print *, ""
    print *, "Original scale c_original:", c_original
    print *, "Recovered scale c:", c

    print *, ""
    print *, "RMSD:", rmsd

    print *, ""
    print *, "Check R ≈ R_original: ", &
        all_close(R, R_original, rel_tol=1.0e-10_dp, abs_tol=1.0e-12_dp)

end program example_kabsch_real
