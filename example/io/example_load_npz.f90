program example_load_npz
    use stdlib_array, only: array_wrapper_type
    use stdlib_kinds, only: int32, sp
    use stdlib_io_np, only: load_npz
    implicit none

    type(array_wrapper_type), allocatable :: arrays(:)
    real(sp), allocatable :: array_1(:,:)
    integer(int32), allocatable :: array_2(:,:)

    call load_npz('example_load.npz', arrays)

    call arrays(1)%get_values(array_1)
    call arrays(2)%get_values(array_2)

    print *, array_1
    print *, array_2
end
