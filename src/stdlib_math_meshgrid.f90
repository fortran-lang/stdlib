

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

end submodule
