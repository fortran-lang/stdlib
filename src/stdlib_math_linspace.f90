submodule (stdlib_math) stdlib_math_linspace

implicit none

contains

    pure module function linspace_default_1_rsp_rsp(start, end) result(res)
      real(sp), intent(in) :: start
      real(sp), intent(in) :: end

      real(sp) :: res(DEFAULT_LINSPACE_LENGTH)

      res = linspace(start, end, DEFAULT_LINSPACE_LENGTH)

    end function linspace_default_1_rsp_rsp
    pure module function linspace_default_1_rdp_rdp(start, end) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end

      real(dp) :: res(DEFAULT_LINSPACE_LENGTH)

      res = linspace(start, end, DEFAULT_LINSPACE_LENGTH)

    end function linspace_default_1_rdp_rdp

    pure module function linspace_n_1_rsp_rsp(start, end, n) result(res)
      real(sp), intent(in) :: start
      real(sp), intent(in) :: end
      integer, intent(in) :: n

      real(sp) :: res(max(n, 0))

      integer :: i    ! Looping index
      real(sp) :: interval ! Difference between adjacent elements


      if(n <= 0) return ! If passed length is less than or equal to 0, return an empty (allocated with length 0) array
      if(n == 1) then
        res(1) = end
        return
      end if

      interval = (end - start) / real((n - 1), sp)

      res(1) = start
      res(n) = end

      do i = 2, n - 1

        res(i) = real((i-1), sp) * interval + start

      end do

    end function linspace_n_1_rsp_rsp
    pure module function linspace_n_1_rdp_rdp(start, end, n) result(res)
      real(dp), intent(in) :: start
      real(dp), intent(in) :: end
      integer, intent(in) :: n

      real(dp) :: res(max(n, 0))

      integer :: i    ! Looping index
      real(dp) :: interval ! Difference between adjacent elements


      if(n <= 0) return ! If passed length is less than or equal to 0, return an empty (allocated with length 0) array
      if(n == 1) then
        res(1) = end
        return
      end if

      interval = (end - start) / real((n - 1), dp)

      res(1) = start
      res(n) = end

      do i = 2, n - 1

        res(i) = real((i-1), dp) * interval + start

      end do

    end function linspace_n_1_rdp_rdp


      module procedure linspace_default_1_csp_csp

        res = linspace(start, end, DEFAULT_LINSPACE_LENGTH)

      end procedure linspace_default_1_csp_csp
      module procedure linspace_default_1_cdp_cdp

        res = linspace(start, end, DEFAULT_LINSPACE_LENGTH)

      end procedure linspace_default_1_cdp_cdp

      module procedure linspace_n_1_csp_csp

        real(sp) :: x(max(n, 0)) ! array of the real part of complex number
        real(sp) :: y(max(n, 0)) ! array of the imaginary part of the complex number

        x = linspace(start%re, end%re, n)
        y = linspace(start%im, end%im, n)

        res = cmplx(x, y, kind=sp)

      end procedure linspace_n_1_csp_csp
      module procedure linspace_n_1_cdp_cdp

        real(dp) :: x(max(n, 0)) ! array of the real part of complex number
        real(dp) :: y(max(n, 0)) ! array of the imaginary part of the complex number

        x = linspace(start%re, end%re, n)
        y = linspace(start%im, end%im, n)

        res = cmplx(x, y, kind=dp)

      end procedure linspace_n_1_cdp_cdp

      module procedure linspace_default_1_iint8_iint8

        res = linspace(real(start, kind=dp), real(end, kind=dp), DEFAULT_LINSPACE_LENGTH)

      end procedure linspace_default_1_iint8_iint8
      module procedure linspace_default_1_iint16_iint16

        res = linspace(real(start, kind=dp), real(end, kind=dp), DEFAULT_LINSPACE_LENGTH)

      end procedure linspace_default_1_iint16_iint16
      module procedure linspace_default_1_iint32_iint32

        res = linspace(real(start, kind=dp), real(end, kind=dp), DEFAULT_LINSPACE_LENGTH)

      end procedure linspace_default_1_iint32_iint32
      module procedure linspace_default_1_iint64_iint64

        res = linspace(real(start, kind=dp), real(end, kind=dp), DEFAULT_LINSPACE_LENGTH)

      end procedure linspace_default_1_iint64_iint64

      module procedure linspace_n_1_iint8_iint8

        res = linspace(real(start, kind=dp), real(end, kind=dp), n)

      end procedure linspace_n_1_iint8_iint8
      module procedure linspace_n_1_iint16_iint16

        res = linspace(real(start, kind=dp), real(end, kind=dp), n)

      end procedure linspace_n_1_iint16_iint16
      module procedure linspace_n_1_iint32_iint32

        res = linspace(real(start, kind=dp), real(end, kind=dp), n)

      end procedure linspace_n_1_iint32_iint32
      module procedure linspace_n_1_iint64_iint64

        res = linspace(real(start, kind=dp), real(end, kind=dp), n)

      end procedure linspace_n_1_iint64_iint64

end submodule
