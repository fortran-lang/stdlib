module stdlib_stats_decomposition
  !! Provides statistical decomposition methods.
  !! (Specification: :doc:`/specs/stdlib_stats_decomposition`)
  use stdlib_kinds, only: sp, dp, xdp, qp
  use stdlib_linalg_state, only: linalg_state_type
  implicit none
  private
  ! Public API
  public :: pca, pca_transform, pca_inverse_transform

  interface pca
    !! version: experimental
    !!
    !! Principal Component Analysis (PCA)
    !! (Specification: :doc:`/specs/stdlib_stats_decomposition`)
      module subroutine pca_sp(x, components, singular_values, x_mean, &
                                  method, overwrite_x, err)
        real(sp), intent(inout) :: x(:,:)
        real(sp), intent(out) :: components(:,:)
        real(sp), intent(out) :: singular_values(:)
        real(sp), intent(out), optional :: x_mean(:)
        character(*), intent(in), optional :: method
        logical, intent(in), optional :: overwrite_x
        type(linalg_state_type), intent(out), optional :: err
      end subroutine pca_sp
      module subroutine pca_dp(x, components, singular_values, x_mean, &
                                  method, overwrite_x, err)
        real(dp), intent(inout) :: x(:,:)
        real(dp), intent(out) :: components(:,:)
        real(dp), intent(out) :: singular_values(:)
        real(dp), intent(out), optional :: x_mean(:)
        character(*), intent(in), optional :: method
        logical, intent(in), optional :: overwrite_x
        type(linalg_state_type), intent(out), optional :: err
      end subroutine pca_dp
  end interface pca


  interface pca_transform
    !! version: experimental
    !!
    !! Projects data into the reduced dimensional space
    !! (Specification: :doc:`/specs/stdlib_stats_decomposition`)
      module subroutine pca_transform_sp(x, components, x_transformed, x_mean)
        real(sp), intent(in) :: x(:,:)
        real(sp), intent(in) :: components(:,:)
        real(sp), intent(out) :: x_transformed(:,:)
        real(sp), intent(in), optional :: x_mean(:)
      end subroutine pca_transform_sp
      module subroutine pca_transform_dp(x, components, x_transformed, x_mean)
        real(dp), intent(in) :: x(:,:)
        real(dp), intent(in) :: components(:,:)
        real(dp), intent(out) :: x_transformed(:,:)
        real(dp), intent(in), optional :: x_mean(:)
      end subroutine pca_transform_dp
  end interface pca_transform


  interface pca_inverse_transform
    !! version: experimental
    !!
    !! Reconstructs original data from the reduced space
    !! (Specification: :doc:`/specs/stdlib_stats_decomposition`)
      module subroutine pca_inverse_transform_sp(x_reduced, components, x_reconstructed, x_mean)
        real(sp), intent(in) :: x_reduced(:,:)
        real(sp), intent(in) :: components(:,:)
        real(sp), intent(out) :: x_reconstructed(:,:)
        real(sp), intent(in), optional :: x_mean(:)
      end subroutine pca_inverse_transform_sp
      module subroutine pca_inverse_transform_dp(x_reduced, components, x_reconstructed, x_mean)
        real(dp), intent(in) :: x_reduced(:,:)
        real(dp), intent(in) :: components(:,:)
        real(dp), intent(out) :: x_reconstructed(:,:)
        real(dp), intent(in), optional :: x_mean(:)
      end subroutine pca_inverse_transform_dp
  end interface pca_inverse_transform


end module stdlib_stats_decomposition
