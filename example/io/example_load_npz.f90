program example_load_npz
    use stdlib_array
    use stdlib_kinds, only: int32, sp
    use stdlib_io_np, only: load_npz
    implicit none

    type(t_array_wrapper), allocatable :: arrays(:)
    real(sp), allocatable :: array_1(:,:)
    integer(int32), allocatable :: array_2(:,:)
    integer :: i

    call load_npz('example_load.npz', arrays)

    do i = 1, size(arrays)
        select type (array => arrays(i)%array)
          class is (t_array_rsp_2)
            array_1 = array%values
          class is (t_array_iint32_2)
            array_2 = array%values
          class default
            print *, 'Array ', i, ' is of unexpected type.'
        end select
    end do

    print *, array_1
    print *, array_2
end
