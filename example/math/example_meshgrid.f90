program example_meshgrid

    use stdlib_math, only: meshgrid, linspace, stdlib_meshgrid_ij
    use stdlib_kinds, only: sp

    implicit none

    integer, parameter :: nx = 3, ny = 2
    real(sp) :: x(nx), y(ny), &
            xm_cart(ny, nx), ym_cart(ny, nx), &
            xm_mat(nx, ny), ym_mat(nx, ny)

    x = linspace(0_sp, 1_sp, nx)
    y = linspace(0_sp, 1_sp, ny)

    call meshgrid(x, y, xm_cart, ym_cart)
    print *, "xm_cart = "
    call print_2d_array(xm_cart)
    print *, "ym_cart = "
    call print_2d_array(ym_cart)

    call meshgrid(x, y, xm_mat, ym_mat, indexing=stdlib_meshgrid_ij)
    print *, "xm_mat = "
    call print_2d_array(xm_mat)
    print *, "ym_mat = "
    call print_2d_array(ym_mat)

contains
    subroutine print_2d_array(array)
        real(sp), intent(in) :: array(:, :)
        integer :: i

        do i = 1, size(array, dim=1)
            print *, array(i, :)
        end do
    end subroutine
end program example_meshgrid
