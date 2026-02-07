program example_kabsch
    use stdlib_linalg_constants, only: dp
    use stdlib_spatial
    use stdlib_math, only: all_close
    use stdlib_linalg, only: svd, det
    implicit none

    integer, parameter :: d = 3, N = 4
    complex(dp) :: P_original(d, N), Q_original(d, N), P_recovered(d, N)
    complex(dp) :: R_recovered(d, d), R_original(d, d)
    complex(dp) :: t_recovered(d), t_original(d)
    complex(dp)    :: c_recovered, c_original
    real(dp)    :: rmsd

    integer :: i, j
    real(dp) :: r1, r2

    call random_seed()

    ! ----------------------------
    ! Random complex reference points Q
    ! ----------------------------
    do j = 1, N
        do i = 1, d
            call random_number(r1)
            call random_number(r2)
            Q_original(i,j) = cmplx(r1, r2, kind=dp)
        end do
    end do

    ! ------------------------------------------------
    ! Random complex unitary matrix R_original
    ! Constructed via SVD: R = U * V^H
    ! ------------------------------------------------
    do i = 1, d
        do j = 1, d
            call random_number(r1)
            call random_number(r2)
            R_original(i,j) = cmplx(r1, r2, kind=dp)
        end do
    end do

    block
        complex(dp) :: U(d,d), Vt(d,d)
        real(dp)    :: S(d)

        call svd(R_original, S, U, Vt)
        R_original = matmul(U, Vt)   ! unitary

    end block
    
    ! Complex reflection of pi phase flip
    R_original(:, 1) = -R_original(:,1)

    print *, "abs(det(R_original)):", abs(det(R_original))

    ! ----------------------------
    ! Random scale and translation
    ! ----------------------------
    call random_number(r1)
    call random_number(r2)
    c_original = cmplx(r1, r2)

    do i = 1, d
        call random_number(r1)
        call random_number(r2)
        t_original(i) = cmplx(r1, r2, kind=dp)
    end do

    ! ----------------------------
    ! Construct P = c*R*Q + t
    ! ----------------------------
    do j = 1, N
        P_original(:,j) = c_original * matmul(R_original, Q_original(:,j)) + t_original
    end do

    ! ----------------------------
    ! Call complex Kabsch–Umeyama
    ! ----------------------------
    call kabsch(P_original, Q_original, R_recovered, t_recovered, c_recovered, rmsd)

    print *, ""
    print *, "Original rotation R_original:"
    do i = 1, d
        print *, R_original(i,:)
    end do

    print *, ""
    print *, "Recovered rotation R:"
    do i = 1, d
        print *, R_recovered(i,:)
    end do

    print *, ""
    print *, "Original translation t_original:"
    print *, t_original

    print *, ""
    print *, "Recovered translation t:"
    print *, t_recovered

    ! ----------------------------
    ! Apply recovered transform
    ! ----------------------------
    do j = 1, N
        P_recovered(:,j) = c_recovered * matmul(R_recovered, Q_original(:,j)) + t_recovered
    end do

    print *, ""
    print *, "Check P_original ≈ P_recovered: ", &
        all_close(P_original, P_recovered)

    print *, ""
    print *, "Original scale c_original:", c_original
    print *, "Recovered scale c:", c_recovered

    print *, ""
    print *, "RMSD:", rmsd

    print *, ""
    print *, "Check c_recovered * R_recovered ≈ c_original * R_original: ", &
        all_close(c_recovered*R_recovered, c_original*R_original)

end program example_kabsch