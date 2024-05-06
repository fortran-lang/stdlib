
submodule (stdlib_math) stdlib_math_all_close

    implicit none
    
contains

    logical pure module function all_close_1_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(sp), intent(in) :: a(:), b(:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_1_rsp
    logical pure module function all_close_2_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(sp), intent(in) :: a(:,:), b(:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_2_rsp
    logical pure module function all_close_3_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(sp), intent(in) :: a(:,:,:), b(:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_3_rsp
    logical pure module function all_close_4_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(sp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_4_rsp
    logical pure module function all_close_5_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(sp), intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_5_rsp
    logical pure module function all_close_6_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(sp), intent(in) :: a(:,:,:,:,:,:), b(:,:,:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_6_rsp
    logical pure module function all_close_7_rsp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(sp), intent(in) :: a(:,:,:,:,:,:,:), b(:,:,:,:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_7_rsp
    logical pure module function all_close_1_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(dp), intent(in) :: a(:), b(:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_1_rdp
    logical pure module function all_close_2_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(dp), intent(in) :: a(:,:), b(:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_2_rdp
    logical pure module function all_close_3_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(dp), intent(in) :: a(:,:,:), b(:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_3_rdp
    logical pure module function all_close_4_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(dp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_4_rdp
    logical pure module function all_close_5_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(dp), intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_5_rdp
    logical pure module function all_close_6_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(dp), intent(in) :: a(:,:,:,:,:,:), b(:,:,:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_6_rdp
    logical pure module function all_close_7_rdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        real(dp), intent(in) :: a(:,:,:,:,:,:,:), b(:,:,:,:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_7_rdp
    logical pure module function all_close_1_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(sp), intent(in) :: a(:), b(:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_1_csp
    logical pure module function all_close_2_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(sp), intent(in) :: a(:,:), b(:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_2_csp
    logical pure module function all_close_3_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(sp), intent(in) :: a(:,:,:), b(:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_3_csp
    logical pure module function all_close_4_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(sp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_4_csp
    logical pure module function all_close_5_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(sp), intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_5_csp
    logical pure module function all_close_6_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(sp), intent(in) :: a(:,:,:,:,:,:), b(:,:,:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_6_csp
    logical pure module function all_close_7_csp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(sp), intent(in) :: a(:,:,:,:,:,:,:), b(:,:,:,:,:,:,:)
        real(sp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_7_csp
    logical pure module function all_close_1_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(dp), intent(in) :: a(:), b(:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_1_cdp
    logical pure module function all_close_2_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(dp), intent(in) :: a(:,:), b(:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_2_cdp
    logical pure module function all_close_3_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(dp), intent(in) :: a(:,:,:), b(:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_3_cdp
    logical pure module function all_close_4_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(dp), intent(in) :: a(:,:,:,:), b(:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_4_cdp
    logical pure module function all_close_5_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(dp), intent(in) :: a(:,:,:,:,:), b(:,:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_5_cdp
    logical pure module function all_close_6_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(dp), intent(in) :: a(:,:,:,:,:,:), b(:,:,:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_6_cdp
    logical pure module function all_close_7_cdp(a, b, rel_tol, abs_tol, equal_nan) result(close)

        complex(dp), intent(in) :: a(:,:,:,:,:,:,:), b(:,:,:,:,:,:,:)
        real(dp), intent(in), optional :: rel_tol, abs_tol
        logical, intent(in), optional :: equal_nan

        close = all(is_close(a, b, rel_tol, abs_tol, equal_nan))

    end function all_close_7_cdp

end submodule stdlib_math_all_close