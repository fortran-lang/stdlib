program example_load_npz
    use stdlib_array
    use stdlib_io_np, only: load_npz
    implicit none

    type(t_array_wrapper), allocatable :: arrays(:)
    integer :: i

    call load_npz('example_load.npz', arrays)

    do i = 1, size(arrays)
        select type (array => arrays(i)%array)
          class is (t_array_rsp_2)
            print *, array%values
          class is (t_array_iint32_2)
            print *, array%values
          class default
            print *, 'Array ', i, ' is of unknown type.'
        end select
    end do
end
