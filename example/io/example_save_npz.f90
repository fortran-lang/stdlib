program example_save_npz
    use stdlib_array, only: array_wrapper_type, add_array
    use stdlib_io_np, only: save_npz
    implicit none

    type(array_wrapper_type), allocatable :: arrays(:)
    real :: x(3, 2) = 1
    integer :: y(2, 3) = 2

    call add_array(arrays, x)
    call add_array(arrays, y)

    call save_npz('example_save.npz', arrays)
end
