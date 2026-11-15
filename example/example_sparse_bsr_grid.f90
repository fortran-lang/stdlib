! This example uses the same grid graph for a scalar temperature field
! in CSR and a two-component velocity field in BSR. The low-level SpMV kernels
! take the same rowptr and col arrays directly, while the scalar and block
! values are stored separately.
program example_sparse_bsr_grid
    use stdlib_linalg_constants, only: dp
    use stdlib_sparse_constants, only: ilp
    use stdlib_sparse, only: spmv_kernel_bsr, spmv_kernel_csr, sparse_full, sparse_op_none
    implicit none

    ! A 2 x 2 grid, numbered left to right, bottom to top:
    !   3 -- 4
    !   |    |
    !   1 -- 2
    integer, parameter :: nodes = 4, nnz = 12
    integer(ilp), parameter :: rowptr(nodes+1) = [1, 4, 7, 10, 13]
    integer(ilp), parameter :: col(nnz) = [1, 2, 3,  1, 2, 4,  1, 3, 4,  2, 3, 4]
    real(dp) :: temperature(nodes), temperature_out(nodes), scalar_data(nnz)
    real(dp) :: velocity(2,nodes), velocity_out(2*nodes), block_data(2,2,nnz)
    integer :: i, j

    ! The scalar operator is the graph Laplacian: degree on the diagonal,
    ! -1 for each neighbor. The BSR operator uses the same stencil for each
    ! component and adds an on-node coupling between the two components.
    scalar_data = -1._dp
    block_data = 0._dp
    do i = 1, nodes
        do j = rowptr(i), rowptr(i+1)-1
            if (col(j) == i) then
                scalar_data(j) = 2._dp
                block_data(:,:,j) = reshape([2._dp, -0.5_dp, 0.5_dp, 2._dp], [2,2])
            else
                block_data(1,1,j) = -1._dp
                block_data(2,2,j) = -1._dp
            end if
        end do
    end do

    temperature = [1._dp, 2._dp, 3._dp, 4._dp]
    ! Components are adjacent in memory: [u1, v1, u2, v2, ...].
    velocity = reshape([1._dp, 4._dp, 2._dp, 3._dp, &
                        3._dp, 2._dp, 4._dp, 1._dp], [2,nodes])
    temperature_out = 0._dp
    velocity_out = 0._dp

    ! Both kernels use the same graph arrays; only their values differ.
    call spmv_kernel_csr(sparse_op_none, 1._dp, scalar_data, col, rowptr, &
                         sparse_full, temperature, 0._dp, temperature_out)
    call spmv_kernel_bsr(sparse_op_none, 1._dp, block_data, col, rowptr, &
                         sparse_full, reshape(velocity, [2*nodes]), 0._dp, velocity_out)

    if (any(abs(temperature_out - [-3._dp, -1._dp, 1._dp, 3._dp]) > 1.e-12_dp)) &
        error stop "Unexpected CSR result"
    if (any(abs(velocity_out - [-1._dp, 2.5_dp, 0.5_dp, 0._dp, &
                                2._dp, -2.5_dp, 3.5_dp, -5._dp]) > 1.e-12_dp)) &
        error stop "Unexpected BSR result"

    print *, "Node   temperature result   velocity result (u, v)"
    do i = 1, nodes
        print '(i4, 3f12.2)', i, temperature_out(i), velocity_out(2*i-1:2*i)
    end do
end program example_sparse_bsr_grid
