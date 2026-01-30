program example_pca
    use stdlib_kinds, only: dp
    use stdlib_stats, only: pca, pca_transform, pca_inverse_transform
    use stdlib_linalg_state, only: linalg_state_type
    implicit none

    real(dp) :: x(3, 2), components(2, 2), s(2), mu(2)
    real(dp) :: x_trans(3, 2), x_inv(3, 2)
    type(linalg_state_type) :: err
    integer :: i

    ! Input data: 3 observations, 2 features
    x = reshape([1.0_dp, 3.0_dp, 5.0_dp, 2.0_dp, 4.0_dp, 6.0_dp], [3, 2])

    print *, "Original data:"
    do i = 1, 3
        print "(2f6.2)", x(i, :)
    end do

    ! Perform PCA
    call pca(x, components, s, x_mean=mu, err=err)

    if (err%ok()) then
        print *, ""
        print *, "Feature means:", mu
        print *, "Singular values:", s
        print *, "Principal components (as rows):"
        print "(2f6.3)", components(1, :)
        print "(2f6.3)", components(2, :)

        ! Transform data to principal components space
        call pca_transform(x, components, mu, x_trans)
        print *, ""
        print *, "Transformed data (projected):"
        do i = 1, 3
            print "(2f8.3)", x_trans(i, :)
        end do

        ! Inverse transform to reconstruct original data
        call pca_inverse_transform(x_trans, components, mu, x_inv)
        print *, ""
        print *, "Reconstructed data:"
        do i = 1, 3
            print "(2f6.2)", x_inv(i, :)
        end do
    else
        print *, "PCA failed: ", err%message
    end if

end program example_pca
