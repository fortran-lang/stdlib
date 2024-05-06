

submodule(stdlib_math) stdlib_math_meshgrid

    use stdlib_error, only: error_stop

contains

    module procedure meshgrid_1_iint8_iint8

        integer :: i1

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case (stdlib_meshgrid_ij)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_2_iint8_iint8

        integer :: i1,i2

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1) = &
          x2(i2)
  xm1(i2,i1) = &
          x1(i1)
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2) = &
          x1(i1)
  xm2(i1,i2) = &
          x2(i2)
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_3_iint8_iint8

        integer :: i1,i2,i3

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3) = &
          x2(i2)
  xm1(i2,i1,i3) = &
          x1(i1)
  xm3(i2,i1,i3) = &
          x3(i3)
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3) = &
          x1(i1)
  xm2(i1,i2,i3) = &
          x2(i2)
  xm3(i1,i2,i3) = &
          x3(i3)
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_4_iint8_iint8

        integer :: i1,i2,i3,i4

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4) = &
          x2(i2)
  xm1(i2,i1,i3,i4) = &
          x1(i1)
  xm3(i2,i1,i3,i4) = &
          x3(i3)
  xm4(i2,i1,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4) = &
          x1(i1)
  xm2(i1,i2,i3,i4) = &
          x2(i2)
  xm3(i1,i2,i3,i4) = &
          x3(i3)
  xm4(i1,i2,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_5_iint8_iint8

        integer :: i1,i2,i3,i4,i5

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_6_iint8_iint8

        integer :: i1,i2,i3,i4,i5,i6

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_7_iint8_iint8

        integer :: i1,i2,i3,i4,i5,i6,i7

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i2,i1,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i1,i2,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_1_iint16_iint16

        integer :: i1

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case (stdlib_meshgrid_ij)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_2_iint16_iint16

        integer :: i1,i2

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1) = &
          x2(i2)
  xm1(i2,i1) = &
          x1(i1)
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2) = &
          x1(i1)
  xm2(i1,i2) = &
          x2(i2)
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_3_iint16_iint16

        integer :: i1,i2,i3

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3) = &
          x2(i2)
  xm1(i2,i1,i3) = &
          x1(i1)
  xm3(i2,i1,i3) = &
          x3(i3)
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3) = &
          x1(i1)
  xm2(i1,i2,i3) = &
          x2(i2)
  xm3(i1,i2,i3) = &
          x3(i3)
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_4_iint16_iint16

        integer :: i1,i2,i3,i4

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4) = &
          x2(i2)
  xm1(i2,i1,i3,i4) = &
          x1(i1)
  xm3(i2,i1,i3,i4) = &
          x3(i3)
  xm4(i2,i1,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4) = &
          x1(i1)
  xm2(i1,i2,i3,i4) = &
          x2(i2)
  xm3(i1,i2,i3,i4) = &
          x3(i3)
  xm4(i1,i2,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_5_iint16_iint16

        integer :: i1,i2,i3,i4,i5

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_6_iint16_iint16

        integer :: i1,i2,i3,i4,i5,i6

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_7_iint16_iint16

        integer :: i1,i2,i3,i4,i5,i6,i7

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i2,i1,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i1,i2,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_1_iint32_iint32

        integer :: i1

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case (stdlib_meshgrid_ij)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_2_iint32_iint32

        integer :: i1,i2

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1) = &
          x2(i2)
  xm1(i2,i1) = &
          x1(i1)
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2) = &
          x1(i1)
  xm2(i1,i2) = &
          x2(i2)
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_3_iint32_iint32

        integer :: i1,i2,i3

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3) = &
          x2(i2)
  xm1(i2,i1,i3) = &
          x1(i1)
  xm3(i2,i1,i3) = &
          x3(i3)
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3) = &
          x1(i1)
  xm2(i1,i2,i3) = &
          x2(i2)
  xm3(i1,i2,i3) = &
          x3(i3)
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_4_iint32_iint32

        integer :: i1,i2,i3,i4

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4) = &
          x2(i2)
  xm1(i2,i1,i3,i4) = &
          x1(i1)
  xm3(i2,i1,i3,i4) = &
          x3(i3)
  xm4(i2,i1,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4) = &
          x1(i1)
  xm2(i1,i2,i3,i4) = &
          x2(i2)
  xm3(i1,i2,i3,i4) = &
          x3(i3)
  xm4(i1,i2,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_5_iint32_iint32

        integer :: i1,i2,i3,i4,i5

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_6_iint32_iint32

        integer :: i1,i2,i3,i4,i5,i6

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_7_iint32_iint32

        integer :: i1,i2,i3,i4,i5,i6,i7

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i2,i1,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i1,i2,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_1_iint64_iint64

        integer :: i1

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case (stdlib_meshgrid_ij)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_2_iint64_iint64

        integer :: i1,i2

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1) = &
          x2(i2)
  xm1(i2,i1) = &
          x1(i1)
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2) = &
          x1(i1)
  xm2(i1,i2) = &
          x2(i2)
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_3_iint64_iint64

        integer :: i1,i2,i3

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3) = &
          x2(i2)
  xm1(i2,i1,i3) = &
          x1(i1)
  xm3(i2,i1,i3) = &
          x3(i3)
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3) = &
          x1(i1)
  xm2(i1,i2,i3) = &
          x2(i2)
  xm3(i1,i2,i3) = &
          x3(i3)
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_4_iint64_iint64

        integer :: i1,i2,i3,i4

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4) = &
          x2(i2)
  xm1(i2,i1,i3,i4) = &
          x1(i1)
  xm3(i2,i1,i3,i4) = &
          x3(i3)
  xm4(i2,i1,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4) = &
          x1(i1)
  xm2(i1,i2,i3,i4) = &
          x2(i2)
  xm3(i1,i2,i3,i4) = &
          x3(i3)
  xm4(i1,i2,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_5_iint64_iint64

        integer :: i1,i2,i3,i4,i5

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_6_iint64_iint64

        integer :: i1,i2,i3,i4,i5,i6

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_7_iint64_iint64

        integer :: i1,i2,i3,i4,i5,i6,i7

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i2,i1,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i1,i2,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_1_rsp_rsp

        integer :: i1

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case (stdlib_meshgrid_ij)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_2_rsp_rsp

        integer :: i1,i2

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1) = &
          x2(i2)
  xm1(i2,i1) = &
          x1(i1)
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2) = &
          x1(i1)
  xm2(i1,i2) = &
          x2(i2)
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_3_rsp_rsp

        integer :: i1,i2,i3

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3) = &
          x2(i2)
  xm1(i2,i1,i3) = &
          x1(i1)
  xm3(i2,i1,i3) = &
          x3(i3)
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3) = &
          x1(i1)
  xm2(i1,i2,i3) = &
          x2(i2)
  xm3(i1,i2,i3) = &
          x3(i3)
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_4_rsp_rsp

        integer :: i1,i2,i3,i4

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4) = &
          x2(i2)
  xm1(i2,i1,i3,i4) = &
          x1(i1)
  xm3(i2,i1,i3,i4) = &
          x3(i3)
  xm4(i2,i1,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4) = &
          x1(i1)
  xm2(i1,i2,i3,i4) = &
          x2(i2)
  xm3(i1,i2,i3,i4) = &
          x3(i3)
  xm4(i1,i2,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_5_rsp_rsp

        integer :: i1,i2,i3,i4,i5

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_6_rsp_rsp

        integer :: i1,i2,i3,i4,i5,i6

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_7_rsp_rsp

        integer :: i1,i2,i3,i4,i5,i6,i7

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i2,i1,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i1,i2,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_1_rdp_rdp

        integer :: i1

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case (stdlib_meshgrid_ij)
  do i1 = 1, size(x1)
  xm1(i1) = &
          x1(i1)
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_2_rdp_rdp

        integer :: i1,i2

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1) = &
          x2(i2)
  xm1(i2,i1) = &
          x1(i1)
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2) = &
          x1(i1)
  xm2(i1,i2) = &
          x2(i2)
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_3_rdp_rdp

        integer :: i1,i2,i3

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3) = &
          x2(i2)
  xm1(i2,i1,i3) = &
          x1(i1)
  xm3(i2,i1,i3) = &
          x3(i3)
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3) = &
          x1(i1)
  xm2(i1,i2,i3) = &
          x2(i2)
  xm3(i1,i2,i3) = &
          x3(i3)
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_4_rdp_rdp

        integer :: i1,i2,i3,i4

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4) = &
          x2(i2)
  xm1(i2,i1,i3,i4) = &
          x1(i1)
  xm3(i2,i1,i3,i4) = &
          x3(i3)
  xm4(i2,i1,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4) = &
          x1(i1)
  xm2(i1,i2,i3,i4) = &
          x2(i2)
  xm3(i1,i2,i3,i4) = &
          x3(i3)
  xm4(i1,i2,i3,i4) = &
          x4(i4)
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_5_rdp_rdp

        integer :: i1,i2,i3,i4,i5

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5) = &
          x5(i5)
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_6_rdp_rdp

        integer :: i1,i2,i3,i4,i5,i6

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6) = &
          x6(i6)
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure
    module procedure meshgrid_7_rdp_rdp

        integer :: i1,i2,i3,i4,i5,i6,i7

        select case (optval(indexing, stdlib_meshgrid_xy))
        case (stdlib_meshgrid_xy)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i1 = 1, size(x1)
  do i2 = 1, size(x2)
  xm2(i2,i1,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm1(i2,i1,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm3(i2,i1,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i2,i1,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i2,i1,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i2,i1,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i2,i1,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case (stdlib_meshgrid_ij)
  do i7 = 1, size(x7)
  do i6 = 1, size(x6)
  do i5 = 1, size(x5)
  do i4 = 1, size(x4)
  do i3 = 1, size(x3)
  do i2 = 1, size(x2)
  do i1 = 1, size(x1)
  xm1(i1,i2,i3,i4,i5,i6,i7) = &
          x1(i1)
  xm2(i1,i2,i3,i4,i5,i6,i7) = &
          x2(i2)
  xm3(i1,i2,i3,i4,i5,i6,i7) = &
          x3(i3)
  xm4(i1,i2,i3,i4,i5,i6,i7) = &
          x4(i4)
  xm5(i1,i2,i3,i4,i5,i6,i7) = &
          x5(i5)
  xm6(i1,i2,i3,i4,i5,i6,i7) = &
          x6(i6)
  xm7(i1,i2,i3,i4,i5,i6,i7) = &
          x7(i7)
  end do
  end do
  end do
  end do
  end do
  end do
  end do
        case default
            call error_stop("ERROR (meshgrid): unexpected indexing.")
        end select
    end procedure

end submodule