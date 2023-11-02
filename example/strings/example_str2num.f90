program example_str2num
    use stdlib_kinds, only: dp
    use stdlib_str2num
    character(:), allocatable, target :: chain
    character(len=:), pointer :: cptr
    real(dp), allocatable :: r(:), p(:)
    integer :: i 

    chain = " 1.234   1.E1 1e0     0.1234E0  12.21e+001 -34.5E1"
    allocate( r(6), p(6) )
    !> Example for streamline conversion using `to_num_p`
    cptr => chain
    do i =1, 6
        r(i) = to_num_p( cptr , r(i) ) !> the pointer is shifted within the function
    end do
    read(chain,*) p
    print *, "Reading with to_num"
    print *, r
    print *, "Reading with formatted read"
    print *, p

end program