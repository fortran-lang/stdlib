submodule (stdlib_linalg) stdlib_linalg_cross_product

    implicit none

contains

    pure module function cross_product_rsp(a, b) result(res)
        real(sp), intent(in) :: a(3), b(3)
        real(sp) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_rsp
    pure module function cross_product_rdp(a, b) result(res)
        real(dp), intent(in) :: a(3), b(3)
        real(dp) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_rdp
    pure module function cross_product_csp(a, b) result(res)
        complex(sp), intent(in) :: a(3), b(3)
        complex(sp) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_csp
    pure module function cross_product_cdp(a, b) result(res)
        complex(dp), intent(in) :: a(3), b(3)
        complex(dp) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_cdp
    pure module function cross_product_iint8(a, b) result(res)
        integer(int8), intent(in) :: a(3), b(3)
        integer(int8) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_iint8
    pure module function cross_product_iint16(a, b) result(res)
        integer(int16), intent(in) :: a(3), b(3)
        integer(int16) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_iint16
    pure module function cross_product_iint32(a, b) result(res)
        integer(int32), intent(in) :: a(3), b(3)
        integer(int32) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_iint32
    pure module function cross_product_iint64(a, b) result(res)
        integer(int64), intent(in) :: a(3), b(3)
        integer(int64) :: res(3)

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)

    end function cross_product_iint64

end submodule
