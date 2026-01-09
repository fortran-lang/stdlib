
submodule (stdlib_intrinsics) stdlib_intrinsics_matmul
    use stdlib_linalg_blas, only: gemm
    use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_VALUE_ERROR, LINALG_INTERNAL_ERROR
    use stdlib_constants
    implicit none

    character(len=*), parameter :: this = "stdlib_matmul"

contains

    ! Algorithm for the optimal parenthesization of matrices
    ! Reference: Cormen, "Introduction to Algorithms", 4ed, ch-14, section-2
    ! Internal use only!
    pure function matmul_chain_order(p) result(s)
        integer, intent(in) :: p(:)
        integer :: s(1:size(p) - 2, 2:size(p) - 1), m(1:size(p) - 1, 1:size(p) - 1)
        integer :: n, l, i, j, k, q
        n = size(p) - 1
        m(:,:) = 0
        s(:,:) = 0

        do l = 2, n
            do i = 1, n - l + 1
                j = i + l - 1
                m(i,j) = huge(1)
                
                do k = i, j - 1
                    q = m(i,k) + m(k+1,j) + p(i)*p(k+1)*p(j+1)

                    if (q < m(i, j)) then
                        m(i,j) = q
                        s(i,j) = k
                    end if
                end do
            end do
        end do
    end function matmul_chain_order

    
    pure function matmul_chain_mult_sp_3 (m1, m2, m3, start, s, p) result(r)
        real(sp), intent(in) :: m1(:,:), m2(:,:), m3(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        real(sp), allocatable :: r(:,:), temp(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 2)
        allocate(r(p(start), p(start + 3)))

        if (ord == start) then
            ! m1*(m2*m3)
            m = p(start + 1)
            n = p(start + 3)
            k = p(start + 2)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_sp, m2, m, m3, k, zero_sp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_sp, m1, m, temp, k, zero_sp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*m3
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m, n))
            call gemm('N', 'N', m, n, k, one_sp, m1, m, m2, k, zero_sp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_sp, temp, m, m3, k, zero_sp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_sp_3

    pure function matmul_chain_mult_sp_4 (m1, m2, m3, m4, start, s, p) result(r)
        real(sp), intent(in) :: m1(:,:), m2(:,:), m3(:,:), m4(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        real(sp), allocatable :: r(:,:), temp(:,:), temp1(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 3)
        allocate(r(p(start), p(start + 4)))

        if (ord == start) then
            ! m1*(m2*m3*m4)
            temp = matmul_chain_mult_sp_3(m2, m3, m4, start + 1, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_sp, m1, m, temp, k, zero_sp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*(m3*m4)
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_sp, m1, m, m2, k, zero_sp, temp, m)

            m = p(start + 2)
            n = p(start + 4)
            k = p(start + 3)
            allocate(temp1(m,n))
            call gemm('N', 'N', m, n, k, one_sp, m3, m, m4, k, zero_sp, temp1, m)

            m = p(start)
            n = p(start + 4)
            k = p(start + 2)
            call gemm('N', 'N', m, n, k, one_sp, temp, m, temp1, k, zero_sp, r, m)
        else if (ord == start + 2) then
            ! (m1*m2*m3)*m4
            temp = matmul_chain_mult_sp_3(m1, m2, m3, start, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 3)
            call gemm('N', 'N', m, n, k, one_sp, temp, m, m4, k, zero_sp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_sp_4

    pure module subroutine stdlib_matmul_sub_sp (res, m1, m2, m3, m4, m5, err)
        real(sp), intent(out), allocatable :: res(:,:)
        real(sp), intent(in) :: m1(:,:), m2(:,:)
        real(sp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out), optional :: err
        real(sp), allocatable :: temp(:,:), temp1(:,:)
        integer :: p(6), num_present, m, n, k
        integer, allocatable :: s(:,:)

        type(linalg_state_type) :: err0

        p(1) = size(m1, 1)
        p(2) = size(m2, 1)
        p(3) = size(m2, 2)

        if (size(m1, 2) /= p(2)) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m1=',shape(m1),&
                   ', m2=',shape(m2),'have incompatible sizes')
            call linalg_error_handling(err0, err)
            allocate(res(0, 0))
            return
        end if

        num_present = 2
        if (present(m3)) then

            if (size(m3, 1) /= p(3)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m2=',shape(m2), &
                       ', m3=',shape(m3),'have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(3) = size(m3, 1)
            p(4) = size(m3, 2)
            num_present = num_present + 1
        end if
        if (present(m4)) then

            if (size(m4, 1) /= p(4)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m3=',shape(m3), &
                       ', m4=',shape(m4),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(4) = size(m4, 1)
            p(5) = size(m4, 2)
            num_present = num_present + 1
        end if
        if (present(m5)) then

            if (size(m5, 1) /= p(5)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m4=',shape(m4), &
                       ', m5=',shape(m5),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(5) = size(m5, 1)
            p(6) = size(m5, 2)
            num_present = num_present + 1
        end if

        allocate(res(p(1), p(num_present + 1)))

        if (num_present == 2) then
            m = p(1)
            n = p(3)
            k = p(2)
            call gemm('N', 'N', m, n, k, one_sp, m1, m, m2, k, zero_sp, res, m)
            return
        end if

        ! Now num_present >= 3
        allocate(s(1:num_present - 1, 2:num_present))

        s = matmul_chain_order(p(1: num_present + 1))

        if (num_present == 3) then
            res = matmul_chain_mult_sp_3(m1, m2, m3, 1, s, p(1:4))
            return
        else if (num_present == 4) then
            res = matmul_chain_mult_sp_4(m1, m2, m3, m4, 1, s, p(1:5))
            return
        end if

        ! Now num_present is 5

        select case (s(1, 5))
            case (1)
                ! m1*(m2*m3*m4*m5)
                temp = matmul_chain_mult_sp_4(m2, m3, m4, m5, 2, s, p)
                m = p(1)
                n = p(6)
                k = p(2)
                call gemm('N', 'N', m, n, k, one_sp, m1, m, temp, k, zero_sp, res, m)
            case (2)
                ! (m1*m2)*(m3*m4*m5)
                m = p(1)
                n = p(3)
                k = p(2)
                allocate(temp(m,n))
                call gemm('N', 'N', m, n, k, one_sp, m1, m, m2, k, zero_sp, temp, m)

                temp1 = matmul_chain_mult_sp_3(m3, m4, m5, 3, s, p)

                k = n
                n = p(6)
                call gemm('N', 'N', m, n, k, one_sp, temp, m, temp1, k, zero_sp, res, m)
            case (3)
                ! (m1*m2*m3)*(m4*m5)
                temp = matmul_chain_mult_sp_3(m1, m2, m3, 3, s, p)

                m = p(4)
                n = p(6)
                k = p(5)
                allocate(temp1(m,n))
                call gemm('N', 'N', m, n, k, one_sp, m4, m, m5, k, zero_sp, temp1, m)

                k = m
                m = p(1)
                call gemm('N', 'N', m, n, k, one_sp, temp, m, temp1, k, zero_sp, res, m)
            case (4)
                ! (m1*m2*m3*m4)*m5
                temp = matmul_chain_mult_sp_4(m1, m2, m3, m4, 1, s, p)
                m = p(1)
                n = p(6)
                k = p(5)
                call gemm('N', 'N', m, n, k, one_sp, temp, m, m5, k, zero_sp, res, m)
            case default
                err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,"internal error: unexpected s(i,j)")
                call linalg_error_handling(err0,err)
        end select

    end subroutine stdlib_matmul_sub_sp

    pure module function stdlib_matmul_pure_sp (m1, m2, m3, m4, m5) result(r)
        real(sp), intent(in) :: m1(:,:), m2(:,:)
        real(sp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        real(sp), allocatable :: r(:,:) 

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5)
    end function stdlib_matmul_pure_sp

    module function stdlib_matmul_sp (m1, m2, m3, m4, m5, err) result(r)
        real(sp), intent(in) :: m1(:,:), m2(:,:)
        real(sp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out) :: err
        real(sp), allocatable :: r(:,:)

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5, err=err)
    end function stdlib_matmul_sp

    
    pure function matmul_chain_mult_dp_3 (m1, m2, m3, start, s, p) result(r)
        real(dp), intent(in) :: m1(:,:), m2(:,:), m3(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        real(dp), allocatable :: r(:,:), temp(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 2)
        allocate(r(p(start), p(start + 3)))

        if (ord == start) then
            ! m1*(m2*m3)
            m = p(start + 1)
            n = p(start + 3)
            k = p(start + 2)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_dp, m2, m, m3, k, zero_dp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_dp, m1, m, temp, k, zero_dp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*m3
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m, n))
            call gemm('N', 'N', m, n, k, one_dp, m1, m, m2, k, zero_dp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_dp, temp, m, m3, k, zero_dp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_dp_3

    pure function matmul_chain_mult_dp_4 (m1, m2, m3, m4, start, s, p) result(r)
        real(dp), intent(in) :: m1(:,:), m2(:,:), m3(:,:), m4(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        real(dp), allocatable :: r(:,:), temp(:,:), temp1(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 3)
        allocate(r(p(start), p(start + 4)))

        if (ord == start) then
            ! m1*(m2*m3*m4)
            temp = matmul_chain_mult_dp_3(m2, m3, m4, start + 1, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_dp, m1, m, temp, k, zero_dp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*(m3*m4)
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_dp, m1, m, m2, k, zero_dp, temp, m)

            m = p(start + 2)
            n = p(start + 4)
            k = p(start + 3)
            allocate(temp1(m,n))
            call gemm('N', 'N', m, n, k, one_dp, m3, m, m4, k, zero_dp, temp1, m)

            m = p(start)
            n = p(start + 4)
            k = p(start + 2)
            call gemm('N', 'N', m, n, k, one_dp, temp, m, temp1, k, zero_dp, r, m)
        else if (ord == start + 2) then
            ! (m1*m2*m3)*m4
            temp = matmul_chain_mult_dp_3(m1, m2, m3, start, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 3)
            call gemm('N', 'N', m, n, k, one_dp, temp, m, m4, k, zero_dp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_dp_4

    pure module subroutine stdlib_matmul_sub_dp (res, m1, m2, m3, m4, m5, err)
        real(dp), intent(out), allocatable :: res(:,:)
        real(dp), intent(in) :: m1(:,:), m2(:,:)
        real(dp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out), optional :: err
        real(dp), allocatable :: temp(:,:), temp1(:,:)
        integer :: p(6), num_present, m, n, k
        integer, allocatable :: s(:,:)

        type(linalg_state_type) :: err0

        p(1) = size(m1, 1)
        p(2) = size(m2, 1)
        p(3) = size(m2, 2)

        if (size(m1, 2) /= p(2)) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m1=',shape(m1),&
                   ', m2=',shape(m2),'have incompatible sizes')
            call linalg_error_handling(err0, err)
            allocate(res(0, 0))
            return
        end if

        num_present = 2
        if (present(m3)) then

            if (size(m3, 1) /= p(3)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m2=',shape(m2), &
                       ', m3=',shape(m3),'have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(3) = size(m3, 1)
            p(4) = size(m3, 2)
            num_present = num_present + 1
        end if
        if (present(m4)) then

            if (size(m4, 1) /= p(4)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m3=',shape(m3), &
                       ', m4=',shape(m4),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(4) = size(m4, 1)
            p(5) = size(m4, 2)
            num_present = num_present + 1
        end if
        if (present(m5)) then

            if (size(m5, 1) /= p(5)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m4=',shape(m4), &
                       ', m5=',shape(m5),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(5) = size(m5, 1)
            p(6) = size(m5, 2)
            num_present = num_present + 1
        end if

        allocate(res(p(1), p(num_present + 1)))

        if (num_present == 2) then
            m = p(1)
            n = p(3)
            k = p(2)
            call gemm('N', 'N', m, n, k, one_dp, m1, m, m2, k, zero_dp, res, m)
            return
        end if

        ! Now num_present >= 3
        allocate(s(1:num_present - 1, 2:num_present))

        s = matmul_chain_order(p(1: num_present + 1))

        if (num_present == 3) then
            res = matmul_chain_mult_dp_3(m1, m2, m3, 1, s, p(1:4))
            return
        else if (num_present == 4) then
            res = matmul_chain_mult_dp_4(m1, m2, m3, m4, 1, s, p(1:5))
            return
        end if

        ! Now num_present is 5

        select case (s(1, 5))
            case (1)
                ! m1*(m2*m3*m4*m5)
                temp = matmul_chain_mult_dp_4(m2, m3, m4, m5, 2, s, p)
                m = p(1)
                n = p(6)
                k = p(2)
                call gemm('N', 'N', m, n, k, one_dp, m1, m, temp, k, zero_dp, res, m)
            case (2)
                ! (m1*m2)*(m3*m4*m5)
                m = p(1)
                n = p(3)
                k = p(2)
                allocate(temp(m,n))
                call gemm('N', 'N', m, n, k, one_dp, m1, m, m2, k, zero_dp, temp, m)

                temp1 = matmul_chain_mult_dp_3(m3, m4, m5, 3, s, p)

                k = n
                n = p(6)
                call gemm('N', 'N', m, n, k, one_dp, temp, m, temp1, k, zero_dp, res, m)
            case (3)
                ! (m1*m2*m3)*(m4*m5)
                temp = matmul_chain_mult_dp_3(m1, m2, m3, 3, s, p)

                m = p(4)
                n = p(6)
                k = p(5)
                allocate(temp1(m,n))
                call gemm('N', 'N', m, n, k, one_dp, m4, m, m5, k, zero_dp, temp1, m)

                k = m
                m = p(1)
                call gemm('N', 'N', m, n, k, one_dp, temp, m, temp1, k, zero_dp, res, m)
            case (4)
                ! (m1*m2*m3*m4)*m5
                temp = matmul_chain_mult_dp_4(m1, m2, m3, m4, 1, s, p)
                m = p(1)
                n = p(6)
                k = p(5)
                call gemm('N', 'N', m, n, k, one_dp, temp, m, m5, k, zero_dp, res, m)
            case default
                err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,"internal error: unexpected s(i,j)")
                call linalg_error_handling(err0,err)
        end select

    end subroutine stdlib_matmul_sub_dp

    pure module function stdlib_matmul_pure_dp (m1, m2, m3, m4, m5) result(r)
        real(dp), intent(in) :: m1(:,:), m2(:,:)
        real(dp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        real(dp), allocatable :: r(:,:) 

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5)
    end function stdlib_matmul_pure_dp

    module function stdlib_matmul_dp (m1, m2, m3, m4, m5, err) result(r)
        real(dp), intent(in) :: m1(:,:), m2(:,:)
        real(dp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out) :: err
        real(dp), allocatable :: r(:,:)

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5, err=err)
    end function stdlib_matmul_dp

    
    pure function matmul_chain_mult_csp_3 (m1, m2, m3, start, s, p) result(r)
        complex(sp), intent(in) :: m1(:,:), m2(:,:), m3(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        complex(sp), allocatable :: r(:,:), temp(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 2)
        allocate(r(p(start), p(start + 3)))

        if (ord == start) then
            ! m1*(m2*m3)
            m = p(start + 1)
            n = p(start + 3)
            k = p(start + 2)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_csp, m2, m, m3, k, zero_csp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_csp, m1, m, temp, k, zero_csp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*m3
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m, n))
            call gemm('N', 'N', m, n, k, one_csp, m1, m, m2, k, zero_csp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_csp, temp, m, m3, k, zero_csp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_csp_3

    pure function matmul_chain_mult_csp_4 (m1, m2, m3, m4, start, s, p) result(r)
        complex(sp), intent(in) :: m1(:,:), m2(:,:), m3(:,:), m4(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        complex(sp), allocatable :: r(:,:), temp(:,:), temp1(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 3)
        allocate(r(p(start), p(start + 4)))

        if (ord == start) then
            ! m1*(m2*m3*m4)
            temp = matmul_chain_mult_csp_3(m2, m3, m4, start + 1, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_csp, m1, m, temp, k, zero_csp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*(m3*m4)
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_csp, m1, m, m2, k, zero_csp, temp, m)

            m = p(start + 2)
            n = p(start + 4)
            k = p(start + 3)
            allocate(temp1(m,n))
            call gemm('N', 'N', m, n, k, one_csp, m3, m, m4, k, zero_csp, temp1, m)

            m = p(start)
            n = p(start + 4)
            k = p(start + 2)
            call gemm('N', 'N', m, n, k, one_csp, temp, m, temp1, k, zero_csp, r, m)
        else if (ord == start + 2) then
            ! (m1*m2*m3)*m4
            temp = matmul_chain_mult_csp_3(m1, m2, m3, start, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 3)
            call gemm('N', 'N', m, n, k, one_csp, temp, m, m4, k, zero_csp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_csp_4

    pure module subroutine stdlib_matmul_sub_csp (res, m1, m2, m3, m4, m5, err)
        complex(sp), intent(out), allocatable :: res(:,:)
        complex(sp), intent(in) :: m1(:,:), m2(:,:)
        complex(sp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out), optional :: err
        complex(sp), allocatable :: temp(:,:), temp1(:,:)
        integer :: p(6), num_present, m, n, k
        integer, allocatable :: s(:,:)

        type(linalg_state_type) :: err0

        p(1) = size(m1, 1)
        p(2) = size(m2, 1)
        p(3) = size(m2, 2)

        if (size(m1, 2) /= p(2)) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m1=',shape(m1),&
                   ', m2=',shape(m2),'have incompatible sizes')
            call linalg_error_handling(err0, err)
            allocate(res(0, 0))
            return
        end if

        num_present = 2
        if (present(m3)) then

            if (size(m3, 1) /= p(3)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m2=',shape(m2), &
                       ', m3=',shape(m3),'have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(3) = size(m3, 1)
            p(4) = size(m3, 2)
            num_present = num_present + 1
        end if
        if (present(m4)) then

            if (size(m4, 1) /= p(4)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m3=',shape(m3), &
                       ', m4=',shape(m4),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(4) = size(m4, 1)
            p(5) = size(m4, 2)
            num_present = num_present + 1
        end if
        if (present(m5)) then

            if (size(m5, 1) /= p(5)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m4=',shape(m4), &
                       ', m5=',shape(m5),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(5) = size(m5, 1)
            p(6) = size(m5, 2)
            num_present = num_present + 1
        end if

        allocate(res(p(1), p(num_present + 1)))

        if (num_present == 2) then
            m = p(1)
            n = p(3)
            k = p(2)
            call gemm('N', 'N', m, n, k, one_csp, m1, m, m2, k, zero_csp, res, m)
            return
        end if

        ! Now num_present >= 3
        allocate(s(1:num_present - 1, 2:num_present))

        s = matmul_chain_order(p(1: num_present + 1))

        if (num_present == 3) then
            res = matmul_chain_mult_csp_3(m1, m2, m3, 1, s, p(1:4))
            return
        else if (num_present == 4) then
            res = matmul_chain_mult_csp_4(m1, m2, m3, m4, 1, s, p(1:5))
            return
        end if

        ! Now num_present is 5

        select case (s(1, 5))
            case (1)
                ! m1*(m2*m3*m4*m5)
                temp = matmul_chain_mult_csp_4(m2, m3, m4, m5, 2, s, p)
                m = p(1)
                n = p(6)
                k = p(2)
                call gemm('N', 'N', m, n, k, one_csp, m1, m, temp, k, zero_csp, res, m)
            case (2)
                ! (m1*m2)*(m3*m4*m5)
                m = p(1)
                n = p(3)
                k = p(2)
                allocate(temp(m,n))
                call gemm('N', 'N', m, n, k, one_csp, m1, m, m2, k, zero_csp, temp, m)

                temp1 = matmul_chain_mult_csp_3(m3, m4, m5, 3, s, p)

                k = n
                n = p(6)
                call gemm('N', 'N', m, n, k, one_csp, temp, m, temp1, k, zero_csp, res, m)
            case (3)
                ! (m1*m2*m3)*(m4*m5)
                temp = matmul_chain_mult_csp_3(m1, m2, m3, 3, s, p)

                m = p(4)
                n = p(6)
                k = p(5)
                allocate(temp1(m,n))
                call gemm('N', 'N', m, n, k, one_csp, m4, m, m5, k, zero_csp, temp1, m)

                k = m
                m = p(1)
                call gemm('N', 'N', m, n, k, one_csp, temp, m, temp1, k, zero_csp, res, m)
            case (4)
                ! (m1*m2*m3*m4)*m5
                temp = matmul_chain_mult_csp_4(m1, m2, m3, m4, 1, s, p)
                m = p(1)
                n = p(6)
                k = p(5)
                call gemm('N', 'N', m, n, k, one_csp, temp, m, m5, k, zero_csp, res, m)
            case default
                err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,"internal error: unexpected s(i,j)")
                call linalg_error_handling(err0,err)
        end select

    end subroutine stdlib_matmul_sub_csp

    pure module function stdlib_matmul_pure_csp (m1, m2, m3, m4, m5) result(r)
        complex(sp), intent(in) :: m1(:,:), m2(:,:)
        complex(sp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        complex(sp), allocatable :: r(:,:) 

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5)
    end function stdlib_matmul_pure_csp

    module function stdlib_matmul_csp (m1, m2, m3, m4, m5, err) result(r)
        complex(sp), intent(in) :: m1(:,:), m2(:,:)
        complex(sp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out) :: err
        complex(sp), allocatable :: r(:,:)

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5, err=err)
    end function stdlib_matmul_csp

    
    pure function matmul_chain_mult_cdp_3 (m1, m2, m3, start, s, p) result(r)
        complex(dp), intent(in) :: m1(:,:), m2(:,:), m3(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        complex(dp), allocatable :: r(:,:), temp(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 2)
        allocate(r(p(start), p(start + 3)))

        if (ord == start) then
            ! m1*(m2*m3)
            m = p(start + 1)
            n = p(start + 3)
            k = p(start + 2)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_cdp, m2, m, m3, k, zero_cdp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_cdp, m1, m, temp, k, zero_cdp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*m3
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m, n))
            call gemm('N', 'N', m, n, k, one_cdp, m1, m, m2, k, zero_cdp, temp, m)
            m = p(start)
            n = p(start + 3)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_cdp, temp, m, m3, k, zero_cdp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_cdp_3

    pure function matmul_chain_mult_cdp_4 (m1, m2, m3, m4, start, s, p) result(r)
        complex(dp), intent(in) :: m1(:,:), m2(:,:), m3(:,:), m4(:,:)
        integer, intent(in) :: start, s(:,2:), p(:)
        complex(dp), allocatable :: r(:,:), temp(:,:), temp1(:,:)
        integer :: ord, m, n, k
        ord = s(start, start + 3)
        allocate(r(p(start), p(start + 4)))

        if (ord == start) then
            ! m1*(m2*m3*m4)
            temp = matmul_chain_mult_cdp_3(m2, m3, m4, start + 1, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 1)
            call gemm('N', 'N', m, n, k, one_cdp, m1, m, temp, k, zero_cdp, r, m)
        else if (ord == start + 1) then
            ! (m1*m2)*(m3*m4)
            m = p(start)
            n = p(start + 2)
            k = p(start + 1)
            allocate(temp(m,n))
            call gemm('N', 'N', m, n, k, one_cdp, m1, m, m2, k, zero_cdp, temp, m)

            m = p(start + 2)
            n = p(start + 4)
            k = p(start + 3)
            allocate(temp1(m,n))
            call gemm('N', 'N', m, n, k, one_cdp, m3, m, m4, k, zero_cdp, temp1, m)

            m = p(start)
            n = p(start + 4)
            k = p(start + 2)
            call gemm('N', 'N', m, n, k, one_cdp, temp, m, temp1, k, zero_cdp, r, m)
        else if (ord == start + 2) then
            ! (m1*m2*m3)*m4
            temp = matmul_chain_mult_cdp_3(m1, m2, m3, start, s, p)
            m = p(start)
            n = p(start + 4)
            k = p(start + 3)
            call gemm('N', 'N', m, n, k, one_cdp, temp, m, m4, k, zero_cdp, r, m)
        else
            ! our internal functions are incorrent, abort
            error stop this//": error: unexpected s(i,j)"
        end if

    end function matmul_chain_mult_cdp_4

    pure module subroutine stdlib_matmul_sub_cdp (res, m1, m2, m3, m4, m5, err)
        complex(dp), intent(out), allocatable :: res(:,:)
        complex(dp), intent(in) :: m1(:,:), m2(:,:)
        complex(dp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out), optional :: err
        complex(dp), allocatable :: temp(:,:), temp1(:,:)
        integer :: p(6), num_present, m, n, k
        integer, allocatable :: s(:,:)

        type(linalg_state_type) :: err0

        p(1) = size(m1, 1)
        p(2) = size(m2, 1)
        p(3) = size(m2, 2)

        if (size(m1, 2) /= p(2)) then
            err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m1=',shape(m1),&
                   ', m2=',shape(m2),'have incompatible sizes')
            call linalg_error_handling(err0, err)
            allocate(res(0, 0))
            return
        end if

        num_present = 2
        if (present(m3)) then

            if (size(m3, 1) /= p(3)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m2=',shape(m2), &
                       ', m3=',shape(m3),'have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(3) = size(m3, 1)
            p(4) = size(m3, 2)
            num_present = num_present + 1
        end if
        if (present(m4)) then

            if (size(m4, 1) /= p(4)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m3=',shape(m3), &
                       ', m4=',shape(m4),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(4) = size(m4, 1)
            p(5) = size(m4, 2)
            num_present = num_present + 1
        end if
        if (present(m5)) then

            if (size(m5, 1) /= p(5)) then
                err0 = linalg_state_type(this, LINALG_VALUE_ERROR, 'matrices m4=',shape(m4), &
                       ', m5=',shape(m5),' have incompatible sizes')
                call linalg_error_handling(err0, err)
                allocate(res(0, 0))
                return
            end if

            p(5) = size(m5, 1)
            p(6) = size(m5, 2)
            num_present = num_present + 1
        end if

        allocate(res(p(1), p(num_present + 1)))

        if (num_present == 2) then
            m = p(1)
            n = p(3)
            k = p(2)
            call gemm('N', 'N', m, n, k, one_cdp, m1, m, m2, k, zero_cdp, res, m)
            return
        end if

        ! Now num_present >= 3
        allocate(s(1:num_present - 1, 2:num_present))

        s = matmul_chain_order(p(1: num_present + 1))

        if (num_present == 3) then
            res = matmul_chain_mult_cdp_3(m1, m2, m3, 1, s, p(1:4))
            return
        else if (num_present == 4) then
            res = matmul_chain_mult_cdp_4(m1, m2, m3, m4, 1, s, p(1:5))
            return
        end if

        ! Now num_present is 5

        select case (s(1, 5))
            case (1)
                ! m1*(m2*m3*m4*m5)
                temp = matmul_chain_mult_cdp_4(m2, m3, m4, m5, 2, s, p)
                m = p(1)
                n = p(6)
                k = p(2)
                call gemm('N', 'N', m, n, k, one_cdp, m1, m, temp, k, zero_cdp, res, m)
            case (2)
                ! (m1*m2)*(m3*m4*m5)
                m = p(1)
                n = p(3)
                k = p(2)
                allocate(temp(m,n))
                call gemm('N', 'N', m, n, k, one_cdp, m1, m, m2, k, zero_cdp, temp, m)

                temp1 = matmul_chain_mult_cdp_3(m3, m4, m5, 3, s, p)

                k = n
                n = p(6)
                call gemm('N', 'N', m, n, k, one_cdp, temp, m, temp1, k, zero_cdp, res, m)
            case (3)
                ! (m1*m2*m3)*(m4*m5)
                temp = matmul_chain_mult_cdp_3(m1, m2, m3, 3, s, p)

                m = p(4)
                n = p(6)
                k = p(5)
                allocate(temp1(m,n))
                call gemm('N', 'N', m, n, k, one_cdp, m4, m, m5, k, zero_cdp, temp1, m)

                k = m
                m = p(1)
                call gemm('N', 'N', m, n, k, one_cdp, temp, m, temp1, k, zero_cdp, res, m)
            case (4)
                ! (m1*m2*m3*m4)*m5
                temp = matmul_chain_mult_cdp_4(m1, m2, m3, m4, 1, s, p)
                m = p(1)
                n = p(6)
                k = p(5)
                call gemm('N', 'N', m, n, k, one_cdp, temp, m, m5, k, zero_cdp, res, m)
            case default
                err0 = linalg_state_type(this,LINALG_INTERNAL_ERROR,"internal error: unexpected s(i,j)")
                call linalg_error_handling(err0,err)
        end select

    end subroutine stdlib_matmul_sub_cdp

    pure module function stdlib_matmul_pure_cdp (m1, m2, m3, m4, m5) result(r)
        complex(dp), intent(in) :: m1(:,:), m2(:,:)
        complex(dp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        complex(dp), allocatable :: r(:,:) 

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5)
    end function stdlib_matmul_pure_cdp

    module function stdlib_matmul_cdp (m1, m2, m3, m4, m5, err) result(r)
        complex(dp), intent(in) :: m1(:,:), m2(:,:)
        complex(dp), intent(in), optional :: m3(:,:), m4(:,:), m5(:,:)
        type(linalg_state_type), intent(out) :: err
        complex(dp), allocatable :: r(:,:)

        call stdlib_matmul_sub(r, m1, m2, m3, m4, m5, err=err)
    end function stdlib_matmul_cdp

end submodule stdlib_intrinsics_matmul
