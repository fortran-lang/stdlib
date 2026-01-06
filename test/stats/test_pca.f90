program test_pca
    use stdlib_error, only: check
    use stdlib_kinds, only: sp, dp
    use stdlib_stats, only: pca, pca_transform, pca_inverse_transform
    use stdlib_linalg_state, only: linalg_state_type
    implicit none

    real(sp), parameter :: sptol = 1000 * epsilon(1._sp)
    real(dp), parameter :: dptol = 1000 * epsilon(1._dp)

    call test_pca_sp()
    call test_pca_dp()

contains

    subroutine test_pca_sp()
        real(sp) :: x(3, 2), components(2, 2), s(2), mu(2)
        real(sp) :: x_red(3, 1), comp_red(1, 2), s_red(1)
        real(sp) :: x_trans(3, 2), x_inv(3, 2)
        type(linalg_state_type) :: err

        ! Data: [1, 2], [3, 4], [5, 6]
        x = reshape([1.0_sp, 3.0_sp, 5.0_sp, 2.0_sp, 4.0_sp, 6.0_sp], [3, 2])

        ! Test SVD method
        call pca(x, components, s, x_mean=mu, method="svd", err=err)
        call check(err%ok(), "pca_sp svd err")
        call check(all(abs(mu - [3.0_sp, 4.0_sp]) < sptol), "pca_sp svd mean")
        ! First component should be approx [0.707, 0.707] (or negative)
        call check(abs(abs(components(1,1)) - 1.0_sp/sqrt(2.0_sp)) < sptol, "pca_sp svd comp1")
        call check(abs(s(1) - 4.0_sp) < sptol, "pca_sp svd s1")
        call check(abs(s(2)) < sptol, "pca_sp svd s2")

        ! Test Transform
        x_trans = pca_transform(x, components, mu)
        ! Second dimension should be zero
        call check(all(abs(x_trans(:, 2)) < sptol), "pca_sp transform")

        ! Test Inverse Transform
        x_inv = pca_inverse_transform(x_trans, components, mu)
        call check(all(abs(x_inv - x) < sptol), "pca_sp inverse")

        ! Test EIG method
        call pca(x, components, s, method="eig", err=err)
        call check(err%ok(), "pca_sp eig err")
        call check(abs(s(1) - 4.0_sp) < sptol, "pca_sp eig s1")

    end subroutine test_pca_sp

    subroutine test_pca_dp()
        real(dp) :: x(3, 2), components(2, 2), s(2), mu(2)
        real(dp) :: x_trans(3, 2), x_inv(3, 2)
        type(linalg_state_type) :: err

        x = reshape([1.0_dp, 3.0_dp, 5.0_dp, 2.0_dp, 4.0_dp, 6.0_dp], [3, 2])

        ! Test SVD method
        call pca(x, components, s, x_mean=mu, method="svd", err=err)
        call check(err%ok(), "pca_dp svd err")
        call check(all(abs(mu - [3.0_dp, 4.0_dp]) < dptol), "pca_dp svd mean")
        call check(abs(abs(components(1,1)) - 1.0_dp/sqrt(2.0_dp)) < dptol, "pca_dp svd comp1")
        call check(abs(s(1) - 4.0_dp) < dptol, "pca_dp svd s1")
        
        ! Test Transform/Inverse
        x_trans = pca_transform(x, components, mu)
        x_inv = pca_inverse_transform(x_trans, components, mu)
        call check(all(abs(x_inv - x) < dptol), "pca_dp inverse")

        ! Test EIG method
        call pca(x, components, s, method="eig", err=err)
        call check(err%ok(), "pca_dp eig err")
        call check(abs(s(1) - 4.0_dp) < dptol, "pca_dp eig s1")

    end subroutine test_pca_dp

end program test_pca
