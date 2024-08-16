program example_save_npz
    use stdlib_array, only: t_array_wrapper
    use stdlib_io_np, only: add_array, save_npz
    implicit none

    type(t_array_wrapper), allocatable :: arrays(:)
    real :: x(3, 2) = 1
    integer :: y(2, 3) = 2

    call add_array(arrays, x)
    call add_array(arrays, y)

    call save_npz('example_save.npz', arrays)
end
